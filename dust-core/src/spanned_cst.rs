use crate::ast;
use crate::ast::{Ident, Pattern};
use crate::parser::Spanned;

#[derive(PartialEq, Debug)]
pub struct Program {
    pub statements: Vec<Spanned<Statement>>,
}

#[derive(PartialEq, Debug)]
pub struct Import {
    pub ns: ast::Namespace,
    pub rename: Option<ast::Namespace>,
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Let {
        public: bool,
        name: String,
        value: Spanned<Expr>,
    },
    Import(Import),
    Expr(Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParsingError {
    UnexpectedEof,
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    // Err
    Error(ParsingError),

    Nil,
    Bool(bool),
    Num(f64),
    String(String),
    Ident(Ident),
    Do(Box<Expr>, Box<Expr>),
    If {
        condition: Box<Expr>,
        if_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    Prefix(String, Box<Expr>),
    Infix(String, Box<Expr>, Box<Expr>),
    Pipe(Box<Expr>, Box<Expr>),
    Call {
        f: Box<Expr>,
        args: Vec<Expr>,
    },
    Let {
        pattern: Pattern,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    Use {
        params: Vec<String>,
        f_call: Box<Expr>,
        body: Box<Expr>,
    },
    Fn {
        params: Vec<String>,
        body: Box<Expr>,
    },

    // data structures
    EmptyList,
    Cons(Box<Expr>, Box<Expr>),

    EmptyMap,
    ConsMap((Box<Expr>, Box<Expr>), Box<Expr>),

    Match(Box<Expr>, Vec<(Pattern, Expr)>),

    Tuple(Vec<Expr>),
}
