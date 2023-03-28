#[derive(PartialEq, Debug, Clone)]
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

    /// `/` operator
    Slash,

    /// `%` operator
    Percentage,

    /// `&&` operator
    DoubleAnd,

    /// `||` operator
    DoublePipe,

    /// `!` operator
    Bang,

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

    /// `nil`
    Nil,

    /// `true`
    True,

    /// `false`
    False,

    Ident(String),
    Num(f64),
    String(String),
}
