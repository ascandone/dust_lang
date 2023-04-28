use crate::ast;
use crate::ast::{Ident, Namespace, Pattern};

#[derive(PartialEq, Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

pub fn try_from_program(program: Program) -> Result<ast::Program, String> {
    let mut cst_program = vec![];
    for statement in program.statements {
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
    Pipe(Box<Expr>, Box<Expr>),
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

    // data structures
    EmptyList,
    Cons(Box<Expr>, Box<Expr>),

    EmptyMap,
    ConsMap((Box<Expr>, Box<Expr>), Box<Expr>),

    Match(Box<Expr>, Vec<(Pattern, Expr)>),

    Tuple(Vec<Expr>),
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

fn cst_box_try_into_ast_box(x: Box<Expr>) -> Result<Box<ast::Expr>, String> {
    let x = (*x).try_into()?;
    Ok(Box::new(x))
}

impl TryFrom<Expr> for ast::Expr {
    type Error = String;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Ident(Ident(ns, name)) => Ok(ast::Expr::Ident(ast::Ident(ns, name))),

            Expr::Lit(l) => Ok(ast::Expr::Lit(l)),

            Expr::Do(x, y) => Ok(ast::Expr::Do(
                cst_box_try_into_ast_box(x)?,
                cst_box_try_into_ast_box(y)?,
            )),

            Expr::If {
                condition,
                if_branch,
                else_branch,
            } => Ok(ast::Expr::If {
                condition: cst_box_try_into_ast_box(condition)?,
                if_branch: cst_box_try_into_ast_box(if_branch)?,
                else_branch: cst_box_try_into_ast_box(else_branch)?,
            }),

            Expr::Prefix(op, expr) => Ok(ast::Expr::Prefix(op, cst_box_try_into_ast_box(expr)?)),

            Expr::Pipe(left, right) => match *right {
                Expr::Call { f, args } => {
                    let left = (*left).try_into()?;

                    let mut new_args: Vec<ast::Expr> = vec![left];
                    for arg in args {
                        new_args.push(arg.try_into()?)
                    }

                    Ok(ast::Expr::Call {
                        f: cst_box_try_into_ast_box(f)?,
                        args: new_args,
                    })
                }
                _ => Err(
                    "Invalid usage of `|>` macro: right element should be a function call"
                        .to_string(),
                ),
            },

            Expr::Infix(op, left, right) => Ok(ast::Expr::Infix(
                op,
                cst_box_try_into_ast_box(left)?,
                cst_box_try_into_ast_box(right)?,
            )),

            Expr::Call { f, args } => {
                let mut mapped_args = vec![];
                for arg in args {
                    mapped_args.push(arg.try_into()?)
                }

                Ok(ast::Expr::Call {
                    f: cst_box_try_into_ast_box(f)?,
                    args: mapped_args,
                })
            }

            Expr::Let { name, value, body } => Ok(ast::Expr::Match(
                cst_box_try_into_ast_box(value)?,
                vec![(Pattern::Identifier(name.to_string()), (*body).try_into()?)],
            )),

            Expr::Use {
                body,
                params,
                f_call,
            } => {
                match *f_call {
                    Expr::Call { f, args } => {
                        // TODO check that f is an identifier

                        let mut converted_args: Vec<ast::Expr> = vec![];
                        for arg in args {
                            converted_args.push(arg.try_into()?)
                        }
                        converted_args.push(ast::Expr::Fn {
                            params,
                            body: cst_box_try_into_ast_box(body)?,
                        });

                        Ok(ast::Expr::Call {
                            f: cst_box_try_into_ast_box(f)?,
                            args: converted_args,
                        })
                    }

                    _ => Err("Expected a function call in use syntax sugar".to_string()),
                }
            }

            Expr::Fn { params, body } => Ok(ast::Expr::Fn {
                params,
                body: cst_box_try_into_ast_box(body)?,
            }),

            Expr::EmptyList => Ok(ast::Expr::Call {
                f: Box::new(ast::Expr::Ident(Ident(
                    Some(Namespace(vec!["List".to_string()])),
                    "empty".to_string(),
                ))),
                args: vec![],
            }),

            Expr::EmptyMap => Ok(ast::Expr::Call {
                f: Box::new(ast::Expr::Ident(Ident(
                    Some(Namespace(vec!["Map".to_string()])),
                    "empty".to_string(),
                ))),
                args: vec![],
            }),

            Expr::Cons(hd, tl) => Ok(ast::Expr::Call {
                f: Box::new(ast::Expr::Ident(Ident(
                    Some(Namespace(vec!["List".to_string()])),
                    "cons".to_string(),
                ))),

                args: vec![(*hd).try_into()?, (*tl).try_into()?],
            }),

            Expr::ConsMap((k, v), map) => Ok(ast::Expr::Call {
                f: Box::new(ast::Expr::Ident(Ident(
                    Some(Namespace(vec!["Map".to_string()])),
                    "insert".to_string(),
                ))),

                args: vec![(*map).try_into()?, (*k).try_into()?, (*v).try_into()?],
            }),

            Expr::Tuple(values) => {
                let mut args = vec![];
                for value in values {
                    args.push(value.try_into()?)
                }

                Ok(ast::Expr::Call {
                    f: Box::new(ast::Expr::Ident(Ident(
                        Some(Namespace(vec!["Tuple".to_string()])),
                        match args.len() {
                            2 => "tuple2",
                            3 => "tuple3",
                            _ => return Err(format!("Invalid arity for tuple: {}", args.len())),
                        }
                        .to_string(),
                    ))),
                    args,
                })
            }

            Expr::Match(expr, clauses) => {
                let mut ast_clauses = vec![];
                for (pattern, expr) in clauses {
                    ast_clauses.push((pattern, expr.try_into()?))
                }

                Ok(ast::Expr::Match(
                    cst_box_try_into_ast_box(expr)?,
                    ast_clauses,
                ))
            }
        }
    }
}
