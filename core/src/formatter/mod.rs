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

fn nested_group(doc: Doc) -> Doc {
    Doc::vec(&[space_break(), doc.group()]).nest()
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

fn parens_if_needed(needed: bool, doc: Doc) -> Doc {
    if needed {
        Doc::vec(&[Doc::text("("), doc, Doc::text(")")])
    } else {
        doc
    }
}

fn block_if_needed(needed: bool, doc: Doc) -> Doc {
    if needed {
        Doc::vec(&[
            Doc::text("{"),
            nested_group(doc),
            space_break(),
            Doc::text("}"),
        ])
        .group()
    } else {
        doc
    }
}

fn expr_to_doc(doc: Expr, inside_block: bool) -> Doc {
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
                nested_group(expr_to_doc(*if_branch, true)),
                space_break(),
                Doc::text("} else {"),
                nested_group(expr_to_doc(*else_branch, true)),
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
                    nested_group(expr_to_doc(*body, true)),
                    space_break(),
                    Doc::text("}"),
                ])
                .group(),
            ])
        }

        Expr::Call { f, args } => Doc::vec(&[
            parens_if_needed(
                matches!(*f, Expr::Infix { .. } | Expr::Prefix { .. }),
                expr_to_doc(*f, false),
            ),
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
            parens_if_needed(
                matches!(*right, Expr::Infix(ref nested_op, _, _) if ops_prec(&op) > ops_prec(nested_op) ),
                expr_to_doc(*right, false),
            ),
        ]),

        Expr::Prefix(op, expr) => Doc::vec(&[
            Doc::Text(op),
            parens_if_needed(
                matches!(*expr, Expr::Infix { .. }),
                expr_to_doc(*expr, false),
            ),
        ]),

        Expr::Do(x, y) => block_if_needed(
            !inside_block,
            Doc::vec(&[
                expr_to_doc(*x, true),
                Doc::text(";"),
                space_break(),
                expr_to_doc(*y, true),
            ])
            .group()
            .force_broken(),
        )
        .group(),

        Expr::Let { name, value, body } => block_if_needed(
            !inside_block,
            Doc::vec(&[
                Doc::text("let "),
                Doc::Text(name),
                Doc::text(" = "),
                expr_to_doc(*value, true),
                Doc::text(";"),
                space_break(),
                expr_to_doc(*body, true),
            ])
            .group()
            .force_broken(),
        )
        .group(),

        Expr::Use {
            params,
            body,
            f_call,
        } => block_if_needed(
            !inside_block,
            Doc::vec(&[
                Doc::text("use"),
                Doc::Vec(
                    params
                        .into_iter()
                        .enumerate()
                        .map(|(index, param)| {
                            Doc::vec(&[
                                if index != 0 {
                                    Doc::text(",")
                                } else {
                                    Doc::nil()
                                },
                                Doc::text(" "),
                                Doc::Text(param),
                            ])
                        })
                        .collect(),
                ),
                Doc::text(" <- "),
                expr_to_doc(*f_call, true),
                Doc::text(";"),
                space_break(),
                expr_to_doc(*body, true),
            ])
            .group()
            .force_broken(),
        )
        .group(),

        Expr::Pipe(_, _) => todo!(),
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
