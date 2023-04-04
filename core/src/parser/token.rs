#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Eof,

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

    /// `|>` operator
    PipeRight,

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

    /// `=` operator
    Assign,

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

    /// `use`
    Use,

    /// `<-`
    ArrowLeft,

    /// `if`
    If,

    /// `import`
    Import,

    /// `pub`
    Pub,

    /// `else`
    Else,

    /// `nil`
    Nil,

    /// `true`
    True,

    /// `false`
    False,

    /// `as`
    As,

    /// `.`
    Dot,

    Ident(String),
    NsIndent(String),
    Num(f64),
    String(String),
}
