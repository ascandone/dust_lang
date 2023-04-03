use std::fmt;

pub type Program = Vec<Statement>;

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct Namespace(pub Vec<String>, pub String);

impl Namespace {
    #[cfg(test)]
    pub fn from_path(path: &[&str]) -> Namespace {
        match path {
            [init @ .., last] => Namespace(
                init.into_iter().map(|s| s.to_string()).collect(),
                last.to_string(),
            ),
            _ => panic!("Empty path"),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Import {
    pub ns: Namespace,
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Let {
        public: bool,
        name: String,
        value: Expr,
    },
    Import(Import),
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
pub struct Ident(pub Option<Namespace>, pub String);

#[derive(PartialEq, Debug)]
pub enum Expr {
    Ident(Ident),
    Lit(Lit),
    Do(Box<Expr>, Box<Expr>),
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
    Fn {
        params: Vec<String>,
        body: Box<Expr>,
    },
}

/// utility to create an unqualified identifier
pub fn ident(name: &str) -> Expr {
    Expr::Ident(Ident(None, name.to_string()))
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

impl From<&str> for Expr {
    fn from(s: &str) -> Self {
        Expr::Lit(Lit::String(s.to_string()))
    }
}
