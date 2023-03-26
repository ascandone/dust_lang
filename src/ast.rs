pub type Program = Vec<Declaration>;

#[derive(PartialEq, Debug)]
pub enum Declaration {
    Let { name: String, value: Expr },
    Expr(Expr),
}

#[derive(PartialEq, Debug)]
pub enum Lit {
    Nil,
    Bool(bool),
    Num(f64),
    String(String),
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
