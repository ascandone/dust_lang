#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum OpCode {
    Const = 0x00,
    ConstNil,
    ConstTrue,
    ConstFalse,
    Pop,
    Jump,
    JumpIfFalse,
    JumpIfFalseElsePop,
    JumpIfTrueElsePop,
    SetLocal,
    GetLocal,
    SetGlobal,
    GetGlobal,
    GetFree,

    Call,
    Return,
    MakeClosure,
    GetCurrentClosure,

    // Pattern matching
    PanicNoMatch,
    MatchConstElseJump,
    MatchEmptyListElseJump,
    MatchConsElseJump,
    MatchTuple2ElseJump,
    MatchTuple3ElseJump,
    // MatchEmptyMapElseJump,
    // MatchConsMapElseJump,

    // Algebraic ops
    Add,
    Sub,
    Negate,
    Mult,
    Div,
    Modulo,
    Gt,
    GtEq,
    Lt,
    LtEq,
    Eq,
    NotEq,
    Not,
}
