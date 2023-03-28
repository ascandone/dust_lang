#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum OpCode {
    Const = 0x00,
    ConstNil,
    ConstTrue,
    ConstFalse,
    Pop,
    Jump,
    JumpIfNot,
    SetLocal,
    GetLocal,
    SetGlobal,
    GetGlobal,
    GetFree,

    Call,
    Return,
    MakeClosure,

    // Algebraic ops
    Add,
    GreaterThan,
    Not,
    Eq,
}
