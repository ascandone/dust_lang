use std::fmt;

pub type Program = Vec<Declaration>;

#[derive(PartialEq, Debug)]
pub enum Declaration {
    Let { name: String, value: Expr },
    Expr(Expr),
}

#[derive(PartialEq)]
pub enum Lit {
    Nil,
    Bool(bool),
    Num(f64),
    String(String),
}

impl fmt::Debug for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Nil => write!(f, "nil"),
            Lit::Bool(true) => write!(f, "true"),
            Lit::Bool(false) => write!(f, "false"),
            Lit::Num(n) => write!(f, "{n}"),
            Lit::String(str) => write!(f, "\"{str}\""),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    Ident(String),
    Lit(Lit),
    If {
        condition: Box<Expr>,
        if_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    Prefix(String, Box<Expr>),
    Infix(String, Box<Expr>, Box<Expr>),
    Call {
        f: Box<Expr>,
        args: Vec<Expr>,
    },
    Let {
        name: String,
        value: Box<Expr>,
        body: Box<Expr>,
    },
}

pub const NIL: Expr = Expr::Lit(Lit::Nil);

impl From<bool> for Expr {
    fn from(b: bool) -> Self {
        Expr::Lit(Lit::Bool(b))
    }
}

impl From<f64> for Expr {
    fn from(n: f64) -> Self {
        Expr::Lit(Lit::Num(n))
    }
}
