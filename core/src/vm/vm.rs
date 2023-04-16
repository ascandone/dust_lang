use super::{
    bytecode::OpCode,
    stack::Stack,
    value::{Closure, Function, Value},
};
use std::fmt::{Display, Formatter};
use std::mem::transmute;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, Clone)]
struct Frame {
    /// offset for locals
    base_pointer: usize,

    /// index of the current bytecode instruction
    ip: usize,

    /// currently executing function
    closure: Rc<Closure>,
}

impl Frame {
    fn next_opcode(&mut self) -> u8 {
        let opcode = self.closure.function.bytecode[self.ip];
        self.ip += 1;
        opcode
    }

    fn next_opcode_u16(&mut self) -> u16 {
        let msb = self.next_opcode() as u16;
        let lsb = self.next_opcode() as u16;

        (msb << 8) + lsb
    }
}

#[derive(Default)]
pub struct Vm {
    globals: Vec<Value>,
}

#[derive(Debug)]
pub struct RuntimeErr {
    pub reason: String,
    pub current_function: Rc<Function>,
    pub call_stack: Vec<Rc<Function>>,
}

impl Display for RuntimeErr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.reason)?;
        write!(f, "\n    at {}", self.current_function.display_name())?;
        for s in self.call_stack.iter().rev() {
            write!(f, "\n    at {}", s.display_name())?;
        }
        Ok(())
    }
}

fn make_runtime_err(reason: String, frame: &Frame, frames: &Vec<Frame>) -> RuntimeErr {
    RuntimeErr {
        reason,
        current_function: Rc::clone(&frame.closure.function),
        call_stack: frames
            .iter()
            .map(|frame| Rc::clone(&frame.closure.function))
            .collect(),
    }
}

impl Vm {
    pub fn define_global(&mut self, id: u16, value: Value) {
        self.globals.resize(id as usize + 1, value);
    }

    pub fn run_main(&mut self, f: Rc<Function>) -> Result<Value, RuntimeErr> {
        self.run_function(f, &[])
    }

    pub fn run_function(
        &mut self,
        function: Rc<Function>,
        _args: &[Value],
    ) -> Result<Value, RuntimeErr> {
        let mut frames: Vec<Frame> = vec![];

        let mut frame = Frame {
            closure: Rc::new(Closure {
                function,
                free: vec![],
            }),
            ip: 0,
            base_pointer: 0,
        };

        let mut stack = Stack::<Value>::new();
        for _ in 0..frame.closure.function.locals {
            stack.push(Default::default())
        }

        loop {
            let opcode: OpCode = unsafe { transmute(frame.next_opcode()) };

            match opcode {
                OpCode::Const => {
                    let index = frame.next_opcode();
                    let constant_pool = &frame.closure.function.constant_pool;
                    let value = &constant_pool[index as usize];
                    stack.push(value.clone())
                }

                OpCode::ConstNil => stack.push(Value::Nil),
                OpCode::ConstTrue => stack.push(Value::Bool(true)),
                OpCode::ConstFalse => stack.push(Value::Bool(false)),

                OpCode::Pop => {
                    stack.pop();
                }

                OpCode::Jump => {
                    let index = frame.next_opcode_u16() as usize;
                    frame.ip = index;
                }

                OpCode::JumpIfFalse => {
                    let cond: bool = (&stack.pop())
                        .try_into()
                        .map_err(|reason| make_runtime_err(reason, &frame, &frames))?;
                    let index = frame.next_opcode_u16() as usize;

                    if !cond {
                        frame.ip = index;
                    }
                }

                OpCode::JumpIfFalseElsePop => {
                    let cond: bool = stack
                        .peek()
                        .try_into()
                        .map_err(|reason| make_runtime_err(reason, &frame, &frames))?;
                    let index = frame.next_opcode_u16() as usize;

                    if !cond {
                        frame.ip = index;
                    } else {
                        stack.pop();
                    }
                }

                OpCode::JumpIfTrueElsePop => {
                    let cond: bool = stack
                        .peek()
                        .try_into()
                        .map_err(|reason| make_runtime_err(reason, &frame, &frames))?;
                    let index = frame.next_opcode_u16() as usize;

                    if cond {
                        frame.ip = index;
                    } else {
                        stack.pop();
                    }
                }

                OpCode::SetGlobal => {
                    let value = stack.pop();
                    let ident = frame.next_opcode_u16() as usize;
                    self.globals.resize(ident + 1, value);
                    stack.push(Value::Nil)
                }

                OpCode::GetGlobal => {
                    let ident = frame.next_opcode_u16() as usize;
                    let value = self.globals.get(ident).unwrap().clone();
                    stack.push(value)
                }

                OpCode::SetLocal => {
                    let ident = frame.next_opcode() as usize;
                    let value = stack.pop();
                    stack.set(value, frame.base_pointer + ident);
                }

                OpCode::GetLocal => {
                    let ident = frame.next_opcode() as usize;
                    let value = stack.get(frame.base_pointer + ident);
                    stack.push(value.clone());
                }

                OpCode::Return => match frames.pop() {
                    None => return Ok(stack.pop()),
                    Some(parent_frame) => {
                        let ret_value = stack.pop();
                        let num_locals = stack.len() - frame.base_pointer;
                        for _ in 0..num_locals {
                            stack.pop();
                        }
                        stack.push(ret_value);
                        frame = parent_frame;
                    }
                },

                OpCode::GetFree => {
                    let index = frame.next_opcode() as usize;
                    let value = &frame.closure.free[index];
                    stack.push(value.clone());
                }

                OpCode::MakeClosure => {
                    let free_count = frame.next_opcode();
                    let function_index = frame.next_opcode() as usize;

                    let function = (&frame.closure.function.constant_pool[function_index])
                        .as_fn()
                        .unwrap();

                    let free_values = {
                        let mut free_values = vec![];
                        for _ in 0..free_count {
                            let value = stack.pop();
                            free_values.push(value);
                        }
                        free_values
                    };

                    let clo = Closure {
                        function,
                        free: free_values,
                    };

                    stack.push(Value::Closure(Rc::new(clo)));
                }

                OpCode::GetCurrentClosure => {
                    let clo = Rc::clone(&frame.closure);
                    stack.push(Value::Closure(clo))
                }

                OpCode::Call => {
                    let passed_args_number = frame.next_opcode();
                    let closure = match stack.pop() {
                        Value::Function(function) => Rc::new(Closure {
                            function,
                            free: vec![],
                        }),
                        Value::Closure(clo) => clo,
                        Value::NativeFunction(nf) => {
                            validate_args_number(passed_args_number, nf.args_number)
                                .map_err(|reason| make_runtime_err(reason, &frame, &frames))?;

                            let nf = &(nf.deref()).body;
                            let args = &stack.as_slice()[frame.base_pointer..];
                            let res = nf(args)
                                .map_err(|reason| make_runtime_err(reason, &frame, &frames))?;

                            stack.push(res.clone());
                            continue;
                        }
                        x => {
                            return Err(make_runtime_err(
                                format!("Expected a callable object (got {x} instead)"),
                                &frame,
                                &frames,
                            ))
                        }
                    };

                    let function = &closure.function;

                    validate_args_number(passed_args_number, function.arity)
                        .map_err(|reason| make_runtime_err(reason, &frame, &frames))?;

                    let base_pointer = stack.len() - passed_args_number as usize;

                    for _ in 0..function.locals {
                        stack.push(Default::default())
                    }

                    frames.push(frame);
                    frame = Frame {
                        closure,
                        ip: 0,
                        base_pointer,
                    };
                }

                // Algebraic/native ops
                OpCode::Add => op_2_partial(&mut stack, opcode, |a, b| match (a, b) {
                    (Value::Num(a), Value::Num(b)) => Some(Value::Num(a + b)),
                    (Value::String(a), Value::String(b)) => {
                        let mut s = a.to_string();
                        s.push_str(b);
                        Some(Value::String(Rc::new(s)))
                    }
                    _ => None,
                })
                .map_err(|reason| make_runtime_err(reason, &frame, &frames))?,

                OpCode::Mult => op_2_partial(&mut stack, opcode, |a, b| match (a, b) {
                    (Value::Num(a), Value::Num(b)) => Some(a * b),
                    _ => None,
                })
                .map_err(|reason| make_runtime_err(reason, &frame, &frames))?,

                OpCode::Div => op_2_partial(&mut stack, opcode, |a, b| match (a, b) {
                    (Value::Num(a), Value::Num(b)) => Some(a / b),
                    _ => None,
                })
                .map_err(|reason| make_runtime_err(reason, &frame, &frames))?,

                OpCode::Modulo => op_2_partial(&mut stack, opcode, |a, b| match (a, b) {
                    (Value::Num(a), Value::Num(b)) => Some(a % b),
                    _ => None,
                })
                .map_err(|reason| make_runtime_err(reason, &frame, &frames))?,

                OpCode::Sub => op_2_partial(&mut stack, opcode, |a, b| match (a, b) {
                    (Value::Num(a), Value::Num(b)) => Some(a - b),
                    _ => None,
                })
                .map_err(|reason| make_runtime_err(reason, &frame, &frames))?,

                OpCode::Eq => op_2(&mut stack, |a, b| a == b),
                OpCode::NotEq => op_2(&mut stack, |a, b| a != b),

                OpCode::Gt => op_2_partial(&mut stack, opcode, |a, b| match (a, b) {
                    (Value::Num(a), Value::Num(b)) => Some(a > b),
                    (Value::String(a), Value::String(b)) => Some(a > b),
                    _ => None,
                })
                .map_err(|reason| make_runtime_err(reason, &frame, &frames))?,

                OpCode::GtEq => op_2_partial(&mut stack, opcode, |a, b| match (a, b) {
                    (Value::Num(a), Value::Num(b)) => Some(a >= b),
                    (Value::String(a), Value::String(b)) => Some(a >= b),
                    _ => None,
                })
                .map_err(|reason| make_runtime_err(reason, &frame, &frames))?,

                OpCode::Lt => op_2_partial(&mut stack, opcode, |a, b| match (a, b) {
                    (Value::Num(a), Value::Num(b)) => Some(a < b),
                    (Value::String(a), Value::String(b)) => Some(a < b),
                    _ => None,
                })
                .map_err(|reason| make_runtime_err(reason, &frame, &frames))?,

                OpCode::LtEq => op_2_partial(&mut stack, opcode, |a, b| match (a, b) {
                    (Value::Num(a), Value::Num(b)) => Some(a <= b),
                    (Value::String(a), Value::String(b)) => Some(a <= b),
                    _ => None,
                })
                .map_err(|reason| make_runtime_err(reason, &frame, &frames))?,

                OpCode::Not => op_1_partial(&mut stack, opcode, |a| match a {
                    Value::Bool(a) => Some(!a),
                    _ => None,
                })
                .map_err(|reason| make_runtime_err(reason, &frame, &frames))?,

                OpCode::Negate => op_1_partial(&mut stack, opcode, |a| match a {
                    Value::Num(a) => Some(-a),
                    _ => None,
                })
                .map_err(|reason| make_runtime_err(reason, &frame, &frames))?,
            }
        }
    }
}

fn validate_args_number(passed_args_number: u8, function_arity: u8) -> Result<(), String> {
    if passed_args_number != function_arity {
        Err(format!(
            "Invalid args number passed: expected {}, got {} instead",
            function_arity, passed_args_number
        ))
    } else {
        Ok(())
    }
}

fn op_1_partial<F, R>(stack: &mut Stack<Value>, opcode: OpCode, f: F) -> Result<(), String>
where
    F: Fn(&Value) -> Option<R>,
    R: Into<Value>,
{
    let a = stack.pop();

    match f(&a) {
        Some(result) => {
            stack.push(result.into());
            Ok(())
        }
        None => Err(format!("Type error for {:?}: got {}", opcode, a)),
    }
}

fn op_2<F, R>(stack: &mut Stack<Value>, f: F)
where
    F: Fn(&Value, &Value) -> R,
    R: Into<Value>,
{
    let b = stack.pop();
    let a = stack.pop();
    let result = f(&a, &b);
    stack.push(result.into());
}

fn op_2_partial<F, R>(stack: &mut Stack<Value>, opcode: OpCode, f: F) -> Result<(), String>
where
    F: Fn(&Value, &Value) -> Option<R>,
    R: Into<Value>,
{
    let b = stack.pop();
    let a = stack.pop();
    match f(&a, &b) {
        Some(result) => {
            stack.push(result.into());
            Ok(())
        }
        None => Err(format!("Type error for {:?}: got {}, {}", opcode, a, b)),
    }
}
