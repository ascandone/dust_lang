use crate::vm::bytecode::OpCode;
use crate::vm::value::{Function, Value};
use std::fmt::{Display, Formatter};
use std::mem::transmute;
use std::rc::Rc;

type Arity = Vec<ArityBytes>;

#[derive(Clone, Copy)]
enum ArityBytes {
    One,
    Two,
}

impl ArityBytes {
    fn bytes(&self) -> u8 {
        match self {
            ArityBytes::One => 1,
            ArityBytes::Two => 2,
        }
    }
}

fn opcode_arity(opcode: OpCode) -> Arity {
    use ArityBytes::*;
    use OpCode::*;

    match opcode {
        ConstNil | ConstTrue | ConstFalse | Pop | Return | GetCurrentClosure | PanicNoMatch => {
            vec![]
        }

        Const | SetLocal | GetLocal | GetFree | Call => vec![One],

        Jump
        | JumpIfFalse
        | JumpIfFalseElsePop
        | JumpIfTrueElsePop
        | SetGlobal
        | GetGlobal
        // | MatchEmptyMapElseJump
        => vec![Two],

        MakeClosure => vec![One, One],

        MatchTuple2ElseJump
        | MatchTuple3ElseJump
        | MatchEmptyListElseJump
        | MatchConsElseJump
        // | MatchConsMapElseJump
        => vec![Two, One],

        MatchConstElseJump => vec![Two, One, One],

        Add | Sub | Negate | Mult | Div | Modulo | Gt | GtEq | Lt | LtEq | Eq | NotEq | Not => {
            vec![]
        }
    }
}

fn write_args(
    f: &mut Formatter<'_>,
    arity: &Arity,
    index: usize,
    bytecode: &[u8],
) -> Result<usize, std::fmt::Error> {
    let mut offset = index;

    for (index, arity_bytes) in arity.iter().enumerate() {
        if index != 0 {
            write!(f, ",")?;
        }

        let arg = match arity_bytes {
            ArityBytes::One => bytecode[offset] as u16,
            ArityBytes::Two => u16::from_be_bytes([bytecode[offset], bytecode[offset + 1]]),
        };

        write!(f, " 0x{arg:0>2x}")?;
        offset += arity_bytes.bytes() as usize;
    }

    Ok(offset)
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut index = 0;
        let mut queue = vec![];

        loop {
            let opcode: OpCode = unsafe { transmute(self.bytecode[index]) };

            write!(f, "{:0>4x} {:?}", index, opcode)?;

            index += 1;

            let arity = opcode_arity(opcode);
            let next_index = write_args(f, &arity, index, &self.bytecode)?;

            match opcode {
                OpCode::Const => {
                    let arg = self.bytecode[index];
                    let value = &self.constant_pool[arg as usize];
                    if let Value::Function(f) = value {
                        queue.push(Rc::clone(f))
                    }

                    write!(f, " ({value})")?;
                }

                OpCode::MakeClosure => {
                    let arg_2 = self.bytecode[index + 1];
                    let value = &self.constant_pool[arg_2 as usize];
                    if let Value::Function(f) = value {
                        queue.push(Rc::clone(f))
                    }
                }

                /*
                 OpCode::MatchConsMapElseJump
                => {

                    let arg_2 = self.bytecode[index + 2];
                    let value = &self.constant_pool[arg_2 as usize];
                    write!(f, " ({value})")?;
                },*/
                OpCode::MatchConstElseJump => {
                    let arg_2 = self.bytecode[index + 2 + 1];
                    let value = &self.constant_pool[arg_2 as usize];
                    write!(f, " ({value})")?;
                }

                _ => {}
            }

            index = next_index;

            writeln!(f)?;

            if opcode == OpCode::Return {
                loop {
                    let item = queue.pop();
                    match item {
                        None => break,
                        Some(to_process) => {
                            write!(
                                f,
                                "\nDisassembly of '{}':\n",
                                Value::Function(Rc::clone(&to_process))
                            )?;
                            write!(f, "{}", &to_process)?;
                        }
                    }
                }

                return Ok(());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::vm::bytecode::OpCode;
    use crate::vm::value::{Function, Value};
    use std::rc::Rc;

    #[test]
    fn add_test() {
        let main = Function {
            arity: 1,
            constant_pool: vec![Value::Num(42.0)],
            bytecode: vec![
                /* 00 */ OpCode::GetLocal as u8,
                /* 01 */ 0,
                /* 02 */ OpCode::Const as u8,
                /* 03 */ 0,
                /* 04 */ OpCode::Add as u8,
                /* 05 */ OpCode::Return as u8,
            ],
            ..Default::default()
        };

        assert_eq!(
            main.to_string(),
            "0000 GetLocal 0x00
0002 Const 0x00 (42)
0004 Add
0005 Return
"
        )
    }

    #[test]
    fn get_global_test() {
        let main = Function {
            arity: 1,
            constant_pool: vec![Value::Num(42.0)],
            bytecode: vec![
                /* 00 */ OpCode::GetGlobal as u8,
                /* 01 */ 0x01,
                /* 02 */ 0xff,
                /* 03 */ OpCode::Return as u8,
            ],
            ..Default::default()
        };

        assert_eq!(
            main.to_string(),
            "0000 GetGlobal 0x1ff
0003 Return
"
        )
    }

    #[test]
    fn make_closure_test() {
        let add_2 = Function {
            arity: 1,
            constant_pool: vec![Value::Num(42.0)],
            bytecode: vec![
                /* 00 */ OpCode::GetLocal as u8,
                /* 01 */ 0,
                /* 02 */ OpCode::MakeClosure as u8,
                /* 03 */ 1,
                /* 04 */ 0,
                /* 05 */ OpCode::Return as u8,
            ],
            ..Default::default()
        };

        assert_eq!(
            add_2.to_string(),
            "0000 GetLocal 0x00
0002 MakeClosure 0x01, 0x00
0005 Return
"
        )
    }

    #[test]
    fn nested_dis_test() {
        let nested_f = Rc::new(Function {
            name: Some("nested".to_string()),
            bytecode: vec![
                /* 00 */ OpCode::ConstNil as u8,
                /* 01 */ OpCode::Return as u8,
            ],
            ..Default::default()
        });

        let nested_f_addr = Rc::as_ptr(&nested_f);

        let main = Function {
            constant_pool: vec![Value::Function(nested_f)],
            bytecode: vec![
                /* 00 */ OpCode::Const as u8,
                /* 01 */ 0,
                /* 02 */ OpCode::Return as u8,
            ],
            ..Default::default()
        };

        assert_eq!(
            main.to_string(),
            format!(
                "0000 Const 0x00 (#[function nested at {nested_f_addr:?}])
0002 Return

Disassembly of '#[function nested at {nested_f_addr:?}]':
0000 ConstNil
0001 Return
"
            )
        )
    }

    #[test]
    fn match_const_test() {
        let main = Function {
            constant_pool: vec![Value::Num(42.0)],
            bytecode: vec![
                /* 00 */ OpCode::MatchConstElseJump as u8,
                /* 01 */ 0,
                /* 02 */ 1, // addr
                /* 03 */ 3, // local
                /* 04 */ 0, // const
                /* 05 */ OpCode::Return as u8,
            ],
            ..Default::default()
        };

        assert_eq!(
            main.to_string(),
            "0000 MatchConstElseJump 0x01, 0x03, 0x00 (42)
0005 Return
"
        )
    }

    #[ignore]
    #[test]
    fn match_cons_map() {
        let main = Function {
            constant_pool: vec!["x".into()],
            bytecode: vec![
                // /* 00 */ OpCode::MatchConsMapElseJump as u8,
                /* 01 */ 0,
                /* 02 */ 10,
                /* 03 */ 0,
                /* 03 */ OpCode::Return as u8,
            ],
            ..Default::default()
        };

        assert_eq!(
            main.to_string(),
            "0000 MatchConsMapElseJump 0x0a, 0x00 (\"x\")
0004 Return
"
        )
    }
}
