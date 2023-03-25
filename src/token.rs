#[derive(PartialEq, Debug)]
pub enum Token {
    Eof,

    /// `=` operator
    Assign,

    /// `+` operator
    Plus,

    /// `*` operator
    Mult,

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

    Ident(String),
    Num(f64),
}
