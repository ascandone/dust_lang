#[cfg(test)]
mod formatter_tests;
mod pretty;

use crate::cst::{Expr, Import, Program, Statement};
use crate::formatter::pretty::{Doc, PPrint};

pub fn format(program: Program) -> String {
    format!(
        "{}",
        PPrint {
            max_w: 80,
            nest_size: 2,
            doc: program.into(),
        }
    )
}

fn space_break() -> Doc {
    Doc::Break(" ".to_string())
}

fn ops_prec(str: &str) -> u8 {
    match str {
        "||" => 3,
        "&&" => 4,
        "==" | "!=" => 8,
        "<" | "<=" | ">" | ">=" => 9,
        "+" | "-" => 11,
        "*" | "/" | "%" => 12,
        _ => panic!("Invalid op: {str}"),
    }
}

fn parens(doc: Doc) -> Doc {
    Doc::vec(&[Doc::text("("), doc, Doc::text(")")])
}

fn expr_to_doc(doc: Expr, _inside_block: bool) -> Doc {
    match doc {
        Expr::Lit(l) => Doc::Text(format!("{l}").to_string()),
        Expr::Ident(id) => Doc::Text(format!("{id}").to_string()),
        Expr::If {
            condition,
            if_branch,
            else_branch,
        } => Doc::vec(&[
            Doc::text("if "),
            expr_to_doc(*condition, false),
            Doc::vec(&[
                Doc::text(" {"),
                Doc::vec(&[space_break(), expr_to_doc(*if_branch, true)]).nest(),
                space_break(),
                Doc::text("} else {"),
                Doc::vec(&[space_break(), expr_to_doc(*else_branch, true)]).nest(),
                space_break(),
                Doc::text("}"),
            ])
            .group()
            .force_broken(),
        ]),
        Expr::Fn { params, body } => {
            let mut params_docs = vec![];
            for (index, param) in params.into_iter().enumerate() {
                if index != 0 {
                    params_docs.push(Doc::text(","));
                }

                params_docs.push(Doc::text(" "));
                params_docs.push(Doc::Text(param));
            }

            Doc::vec(&[
                Doc::text("fn"),
                Doc::Vec(params_docs),
                Doc::text(" {"),
                Doc::vec(&[
                    Doc::vec(&[
                        //
                        space_break(),
                        expr_to_doc(*body, true),
                    ])
                    .nest(),
                    space_break(),
                    Doc::text("}"),
                ])
                .group(),
            ])
        }

        Expr::Call { f, args } => Doc::vec(&[
            match *f {
                Expr::Infix { .. } | Expr::Prefix { .. } => parens(expr_to_doc(*f, false)),
                _ => expr_to_doc(*f, false),
            },
            Doc::text("("),
            Doc::Vec(
                args.into_iter()
                    .enumerate()
                    .map(|(index, arg)| {
                        Doc::vec(&[
                            if index == 0 {
                                Doc::nil()
                            } else {
                                Doc::text(", ")
                            },
                            arg.into(),
                        ])
                    })
                    .collect(),
            ),
            Doc::text(")"),
        ]),

        Expr::Infix(op, left, right) => Doc::vec(&[
            expr_to_doc(*left, false),
            Doc::text(" "),
            Doc::Text(op.clone()),
            Doc::text(" "),
            match *right {
                Expr::Infix(ref nested_op, _, _) if ops_prec(&op) > ops_prec(nested_op) => {
                    parens(expr_to_doc(*right, false))
                }
                _ => expr_to_doc(*right, false),
            },
        ]),

        Expr::Prefix(op, expr) => Doc::vec(&[
            Doc::Text(op),
            match *expr {
                Expr::Infix { .. } => parens(expr_to_doc(*expr, false)),
                _ => expr_to_doc(*expr, false),
            },
        ]),

        Expr::Do(_, _) => todo!(),
        Expr::Pipe(_, _) => todo!(),
        Expr::Let { .. } => todo!(),
        Expr::Use { .. } => todo!(),
    }
}

impl Into<Doc> for Expr {
    fn into(self) -> Doc {
        expr_to_doc(self, false)
    }
}

impl Into<Doc> for Statement {
    fn into(self) -> Doc {
        match self {
            Statement::Let {
                public,
                name,
                value,
            } => Doc::Vec(vec![
                if public {
                    Doc::text("pub ")
                } else {
                    Doc::nil()
                },
                Doc::text("let "),
                Doc::Text(name),
                Doc::text(" = "),
                value.into(),
            ]),
            Statement::Import(Import { ns, rename }) => Doc::vec(&[
                Doc::text("import "),
                Doc::Text(ns.to_string()),
                match rename {
                    None => Doc::nil(),
                    Some(ns) => Doc::vec(&[Doc::text(" as "), Doc::Text(ns.to_string())]),
                },
            ]),
            Statement::Expr(e) => e.into(),
        }
    }
}

impl Into<Doc> for Program {
    fn into(self) -> Doc {
        let v = self
            .statements
            .into_iter()
            .enumerate()
            .map(|(index, s)| {
                Doc::vec(&[
                    if index == 0 {
                        Doc::nil()
                    } else {
                        Doc::vec(&[
                            //
                            Doc::text(";"),
                            Doc::LineBreak { lines: 2 },
                        ])
                    },
                    s.into(),
                ])
            })
            .collect();

        Doc::vec(&[Doc::Vec(v), Doc::LineBreak { lines: 1 }])
    }
}