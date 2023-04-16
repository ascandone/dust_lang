use crate::vm::bytecode::OpCode;
use crate::vm::value::Function;
use std::fmt::{Display, Formatter};
use std::mem::transmute;

enum Arity {
    Zero,
    One8,
    One16,
    Two8And8,
}

fn opcode_arity(opcode: OpCode) -> Arity {
    use crate::vm::disassembler::Arity::*;
    use OpCode::*;

    match opcode {
        ConstNil | ConstTrue | ConstFalse | Pop | Return | GetCurrentClosure => Zero,

        Const | SetLocal | GetLocal | GetFree | Call => One8,

        Jump | JumpIfFalse | JumpIfFalseElsePop | JumpIfTrueElsePop | SetGlobal | GetGlobal => {
            One16
        }

        MakeClosure => Two8And8,

        Add | Sub | Negate | Mult | Div | Modulo | Gt | GtEq | Lt | LtEq | Eq | NotEq | Not => Zero,
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut index = 0;

        loop {
            let opcode: OpCode = unsafe { transmute(self.bytecode[index]) };

            write!(f, "{:0>4x} {:?}", index, opcode)?;

            index += 1;

            match opcode_arity(opcode) {
                Arity::Zero => {}
                Arity::One8 => {
                    let arg = self.bytecode[index];
                    write!(f, " 0x{arg:0>2x}")?;

                    match opcode {
                        OpCode::Const => {
                            let value = &self.constant_pool[arg as usize];
                            write!(f, " ({value})")?;
                        }
                        _ => {}
                    };

                    index += 1;
                }

                Arity::One16 => {
                    let arg = u16::from_be_bytes([self.bytecode[index], self.bytecode[index + 1]]);
                    write!(f, " 0x{arg:0>2x}")?;
                    index += 2;
                }

                Arity::Two8And8 => {
                    let arg_1 = self.bytecode[index];
                    let arg_2 = self.bytecode[index + 1];
                    write!(f, " 0x{arg_1:0>2x}, 0x{arg_2:0>2x}")?;
                    index += 2;
                }
            };

            write!(f, "\n")?;

            if opcode == OpCode::Return {
                return Ok(());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::vm::bytecode::OpCode;
    use crate::vm::value::{Function, Value};

    #[test]
    fn add_test() {
        let add_2 = Function {
            name: Some("example".to_string()),
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
            add_2.to_string(),
            "0000 GetLocal 0x00
0002 Const 0x00 (42)
0004 Add
0005 Return
"
        )
    }

    #[test]
    fn get_global_test() {
        let add_2 = Function {
            name: Some("example".to_string()),
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
            add_2.to_string(),
            "0000 GetGlobal 0x1ff
0003 Return
"
        )
    }

    #[test]
    fn make_closure_test() {
        let add_2 = Function {
            name: Some("example".to_string()),
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
}
