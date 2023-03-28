use std::mem::transmute;
use std::rc::Rc;

use super::{
    bytecode::OpCode,
    stack::Stack,
    value::{Closure, Function, Value},
};

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

impl Vm {
    pub fn run_main(&mut self, f: Rc<Function>) -> Result<Value, String> {
        self.run_function(f, &[])
    }

    pub fn run_function(
        &mut self,
        function: Rc<Function>,
        _args: &[Value],
    ) -> Result<Value, String> {
        let mut frames = vec![];

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
                    let value = frame
                        .closure
                        .function
                        .constant_pool
                        .get(index as usize)
                        .unwrap()
                        .clone();
                    stack.push(value)
                }

                OpCode::ConstNil => stack.push(Value::Nil),
                OpCode::ConstTrue => stack.push(Value::Bool(true)),
                OpCode::ConstFalse => stack.push(Value::Bool(false)),

                OpCode::Pop => {
                    stack.pop();
                }

                OpCode::Mult => op_2(&mut stack, |a, b| match (a.clone(), b.clone()) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
                    _ => panic!("Type error in Mult: {a} * {b}"),
                }),

                OpCode::Add => op_2(&mut stack, |a, b| match (a.clone(), b.clone()) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                    (Value::String(a), Value::String(b)) => {
                        // TODO optimize
                        let s: String = a.to_string() + b.to_string().as_str();
                        Value::String(Rc::new(s))
                    }
                    _ => panic!("Type error in Add: {a} + {b}"),
                }),

                OpCode::Eq => op_2(&mut stack, |a, b| Value::Bool(a == b)),

                OpCode::GreaterThan => op_2(&mut stack, |a, b| match (a.clone(), b.clone()) {
                    (Value::Int(a), Value::Int(b)) => Value::Bool(a > b),
                    _ => panic!("Type error in GreatherThan: {a} > {b} @ {:?}", frame.ip - 1),
                }),

                OpCode::Not => op_1(&mut stack, |a| match a {
                    Value::Bool(a) => Value::Bool(!a),
                    _ => panic!("Type error"),
                }),

                OpCode::Jump => {
                    let index = frame.next_opcode_u16() as usize;
                    frame.ip = index;
                }

                OpCode::JumpIfNot => {
                    let cond = stack.pop().as_bool();
                    let index = frame.next_opcode_u16() as usize;

                    if !cond {
                        frame.ip = index;
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
                    let value = stack.get(frame.base_pointer + ident).clone();
                    stack.push(value);
                }

                OpCode::Return => match frames.pop() {
                    None => return Ok(stack.pop()),
                    Some(parent_frame) => {
                        frame = parent_frame;
                    }
                },

                OpCode::GetFree => {
                    let index = frame.next_opcode() as usize;
                    let value = frame.closure.free.get(index).unwrap();
                    stack.push(value.clone());
                }

                OpCode::MakeClosure => {
                    let free_count = frame.next_opcode();
                    // TODO why not fetch directly from constant pool?
                    let function = stack.pop().as_fn();

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

                OpCode::Call => {
                    let passed_args_number = frame.next_opcode();
                    let closure = match stack.pop() {
                        Value::Function(function) => Rc::new(Closure {
                            function,
                            free: vec![],
                        }),
                        Value::Closure(clo) => clo,
                        _ => return Err("Expected a callable object.".to_string()),
                    };
                    let base_pointer = stack.len() - passed_args_number as usize;

                    let function = &closure.function;

                    // ---- starting args validation+allocation

                    // TODO refactor 'sto schifo
                    let min_args = function.arity.required;
                    if passed_args_number < min_args {
                        let err_msg = format!(
                            "Too few args passed: expected at least {min_args}, passed {passed_args_number}",
                        );
                        return Err(err_msg);
                    }

                    if !function.arity.rest {
                        let max_args = function.arity.required + function.arity.optional;
                        if passed_args_number > max_args {
                            let err_msg = format!(
                                "Too many args passed: expected at most {max_args}, passed {passed_args_number}"
                            );
                            return Err(err_msg);
                        }
                    }

                    let missing_optional_args = function.arity.required as i16
                        + function.arity.optional as i16
                        - passed_args_number as i16;

                    for _ in 0..missing_optional_args {
                        stack.push(Value::default())
                    }

                    if function.arity.rest {
                        let rest_args_given = passed_args_number as i16
                            - (function.arity.required as i16)
                            - (function.arity.optional as i16);

                        let mut v = vec![];
                        for _ in 0..rest_args_given {
                            let value = stack.pop();
                            v.push(value);
                        }
                        v.reverse();

                        stack.push(v.into());
                    }
                    // ---- finished args validation+allocation

                    // ---- starting locals allocation
                    for _ in 0..function.locals {
                        stack.push(Default::default())
                    }
                    // ---- finished locals allocation

                    frames.push(frame);
                    frame = Frame {
                        closure,
                        ip: 0,
                        base_pointer,
                    };
                }
            }
        }
    }
}

fn op_1<F>(stack: &mut Stack<Value>, f: F)
where
    F: Fn(Value) -> Value,
{
    let a = stack.pop();
    let result = f(a);
    stack.push(result);
}

fn op_2<F>(stack: &mut Stack<Value>, f: F)
where
    F: Fn(Value, Value) -> Value,
{
    let b = stack.pop();
    let a = stack.pop();
    let result = f(a, b);
    stack.push(result);
}
