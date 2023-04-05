use crate::ast;

pub type Program = Vec<Statement>;

pub fn try_from_program(program: Program) -> Result<ast::Program, String> {
    let mut cst_program = vec![];
    for statement in program {
        cst_program.push(statement.try_into()?)
    }
    Ok(cst_program)
}

#[cfg(test)]
/// utility to create an unqualified identifier
pub fn ident(name: &str) -> Expr {
    Expr::Ident(Ident(None, name.to_string()))
}

#[derive(PartialEq, Debug)]
pub struct Import {
    pub ns: ast::Namespace,
    pub rename: Option<ast::Namespace>,
}

impl Import {
    #[cfg(test)]
    pub fn new(ns: ast::Namespace) -> Self {
        Self { ns, rename: None }
    }
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

impl TryFrom<Statement> for ast::Statement {
    type Error = String;

    fn try_from(value: Statement) -> Result<Self, Self::Error> {
        match value {
            Statement::Let {
                name,
                value,
                public,
            } => Ok(ast::Statement::Let {
                public,
                name,
                value: value.try_into()?,
            }),

            Statement::Import(Import { ns, rename }) => {
                Ok(ast::Statement::Import(ast::Import { ns, rename }))
            }

            Statement::Expr(e) => Ok(ast::Statement::Expr(e.try_into()?)),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Ident(pub Option<ast::Namespace>, pub String);

#[derive(PartialEq, Debug)]
pub enum Expr {
    Ident(Ident),
    Lit(ast::Lit),
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
    Use {
        params: Vec<String>,
        f_call: Box<Expr>,
        body: Box<Expr>,
    },
    Fn {
        params: Vec<String>,
        body: Box<Expr>,
    },
}

pub const NIL: Expr = Expr::Lit(ast::Lit::Nil);

impl From<bool> for Expr {
    fn from(b: bool) -> Self {
        Expr::Lit(ast::Lit::Bool(b))
    }
}

impl From<f64> for Expr {
    fn from(n: f64) -> Self {
        Expr::Lit(ast::Lit::Num(n))
    }
}

impl From<&str> for Expr {
    fn from(s: &str) -> Self {
        Expr::Lit(ast::Lit::String(s.to_string()))
    }
}

impl TryFrom<Expr> for ast::Expr {
    type Error = String;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Ident(Ident(ns, name)) => Ok(ast::Expr::Ident(ast::Ident(ns, name))),

            Expr::Lit(l) => Ok(ast::Expr::Lit(l)),

            Expr::Do(x, y) => {
                let x = (*x).try_into()?;
                let y = (*y).try_into()?;
                Ok(ast::Expr::Do(Box::new(x), Box::new(y)))
            }

            Expr::If {
                condition,
                if_branch,
                else_branch,
            } => {
                let condition = (*condition).try_into()?;
                let if_branch = (*if_branch).try_into()?;
                let else_branch = (*else_branch).try_into()?;

                Ok(ast::Expr::If {
                    condition: Box::new(condition),
                    if_branch: Box::new(if_branch),
                    else_branch: Box::new(else_branch),
                })
            }

            Expr::Prefix(op, expr) => {
                let expr = (*expr).try_into()?;
                Ok(ast::Expr::Prefix(op, Box::new(expr)))
            }

            Expr::Infix(op, e1, e2) => {
                let e1 = (*e1).try_into()?;
                let e2 = (*e2).try_into()?;
                Ok(ast::Expr::Infix(op, Box::new(e1), Box::new(e2)))
            }

            Expr::Call { f, args } => {
                let f = (*f).try_into()?;

                let mut mapped_args = vec![];
                for arg in args {
                    mapped_args.push(arg.try_into()?)
                }

                Ok(ast::Expr::Call {
                    f: Box::new(f),
                    args: mapped_args,
                })
            }

            Expr::Let { name, value, body } => {
                let value = (*value).try_into()?;
                let body = (*body).try_into()?;

                Ok(ast::Expr::Let {
                    name,
                    value: Box::new(value),
                    body: Box::new(body),
                })
            }

            Expr::Use {
                body,
                params,
                f_call,
            } => {
                match *f_call {
                    Expr::Call { f, args } => {
                        // TODO check that f is an identifier
                        let f = (*f).try_into()?;
                        let body = (*body).try_into()?;

                        let mut converted_args: Vec<ast::Expr> = vec![];
                        for arg in args {
                            converted_args.push(arg.try_into()?)
                        }
                        converted_args.push(ast::Expr::Fn {
                            params,
                            body: Box::new(body),
                        });

                        Ok(ast::Expr::Call {
                            f: Box::new(f),
                            args: converted_args,
                        })
                    }

                    _ => Err("Expected a function call in use syntax sugar".to_string()),
                }
            }

            Expr::Fn { params, body } => {
                let body = (*body).try_into()?;

                Ok(ast::Expr::Fn {
                    params,
                    body: Box::new(body),
                })
            }
        }
    }
}
