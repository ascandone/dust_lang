#[derive(PartialEq, Debug)]
pub enum Token {
    Eof,

    /// `=` operator
    Assign,

    /// `+` operator
    Plus,

    /// `-` operator
    Minus,

    /// `*` operator
    Mult,

    /// `!` operator
    Not,

    /// `<` operator
    Less,

    /// `>` operator
    Greater,

    /// `<=` operator
    LessEqual,

    /// `>=` operator
    GreaterEqual,

    /// `==` operator
    Eq,

    /// `!=` operator
    NotEq,

    /// `(`
    LParen,

    /// `)`
    RParen,

    /// `{`
    LBrace,

    /// `}`
    RBrace,

    /// `,`
    Comma,

    /// `;`
    Semicolon,

    /// `fn`
    Fn,

    /// `let`
    Let,

    /// `if`
    If,

    /// `else`
    Else,

    Ident(String),
    Num(f64),
}
