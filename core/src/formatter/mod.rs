#[cfg(test)]
mod formatter_tests;
mod pretty;

use crate::ast::Pattern;
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
        parens(doc)
    } else {
        doc
    }
}

fn parens(doc: Doc) -> Doc {
    Doc::vec(&[Doc::text("("), doc, Doc::text(")")])
}

fn block_if_needed(needed: bool, doc: Doc) -> Doc {
    if needed {
        block(doc)
    } else {
        doc
    }
}

fn nested_if_needed(needed: bool, doc: Doc) -> Doc {
    if needed {
        nested_group(doc)
    } else {
        Doc::vec(&[Doc::text(" "), doc])
    }
}

fn block(doc: Doc) -> Doc {
    Doc::vec(&[
        Doc::text("{"),
        nested_group(doc).group(),
        space_break(),
        Doc::text("}"),
    ])
}

fn expr_to_doc(expr: Expr, inside_block: bool) -> Doc {
    match expr {
        Expr::Lit(l) => Doc::Text(format!("{l}")),
        Expr::Ident(id) => Doc::Text(format!("{id}")),
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
            match *right {
                Expr::Infix(ref nested_op, _, _) if ops_prec(&op) > ops_prec(nested_op) => {
                    parens(expr_to_doc(*right, false))
                }

                Expr::Pipe { .. } => block(expr_to_doc(*right, true)),

                _ => expr_to_doc(*right, false),
            },
        ]),

        Expr::Prefix(op, expr) => Doc::vec(&[
            Doc::Text(op),
            match *expr {
                Expr::Infix { .. } => parens(expr_to_doc(*expr, false)),
                Expr::Pipe { .. } => block(expr_to_doc(*expr, true)),
                _ => expr_to_doc(*expr, false),
            },
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
                Doc::text(" ="),
                format_let_value(*value),
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

        Expr::Pipe(x, f) => {
            Doc::vec(&[
                //
                expr_to_doc(*x, inside_block),
                space_break(),
                Doc::vec(&[Doc::text("|> "), expr_to_doc(*f, false)]),
            ])
            .force_broken()
        }

        Expr::Cons(_, _) | Expr::EmptyList => {
            let mut docs = vec![];
            let mut lst = expr;

            loop {
                match lst {
                    Expr::Cons(hd, next) => {
                        if !docs.is_empty() {
                            docs.push(Doc::text(", "))
                        }
                        docs.push(expr_to_doc(*hd, false));
                        lst = *next;
                    }

                    Expr::EmptyList => break,

                    _ => {
                        docs.push(Doc::text(", .."));
                        docs.push(expr_to_doc(lst, false));
                        break;
                    }
                };
            }

            Doc::vec(&[Doc::text("["), Doc::Vec(docs), Doc::text("]")])
        }

        // TODO format
        Expr::Tuple(_) => todo!(),

        Expr::Match(expr, clauses) => {
            let clauses: Vec<Doc> = clauses
                .into_iter()
                .enumerate()
                .map(|(index, (pattern, expr))| {
                    Doc::vec(&[
                        if index != 0 {
                            space_break()
                        } else {
                            Doc::vec(&[])
                        },
                        pattern_to_doc(pattern),
                        Doc::text(" => "),
                        expr_to_doc(expr, false),
                        Doc::text(","),
                    ])
                })
                .collect();

            Doc::vec(&[
                Doc::text("match "),
                expr_to_doc(*expr, false),
                Doc::text(" {"),
                if clauses.is_empty() {
                    Doc::text("}")
                } else {
                    Doc::vec(&[
                        nested_group(Doc::Vec(clauses)),
                        space_break(),
                        Doc::text("}"),
                    ])
                }
                .group()
                .force_broken(),
            ])
        }
    }
}

fn pattern_to_doc(pattern: Pattern) -> Doc {
    match pattern {
        Pattern::Identifier(ident) => Doc::Text(ident),
        Pattern::Lit(l) => Doc::Text(format!("{l}")),
        Pattern::Tuple2(x, y) => Doc::vec(&[
            Doc::text("#("),
            pattern_to_doc(*x),
            Doc::text(" , "),
            pattern_to_doc(*y),
            Doc::text(")"),
        ]),
        Pattern::Cons(_, _) | Pattern::EmptyList => {
            let mut docs = vec![];
            let mut lst = pattern;
            loop {
                match lst {
                    Pattern::Cons(hd, tl) => {
                        if !docs.is_empty() {
                            docs.push(Doc::text(", "))
                        }
                        docs.push(pattern_to_doc(*hd.clone()));
                        lst = *tl;
                    }

                    Pattern::EmptyList => break,

                    _ => {
                        docs.push(Doc::text(", .."));
                        docs.push(pattern_to_doc(lst));
                        break;
                    }
                }
            }

            Doc::vec(&[Doc::text("["), Doc::Vec(docs), Doc::text("]")])
        }
    }
}

impl Into<Doc> for Expr {
    fn into(self) -> Doc {
        expr_to_doc(self, false)
    }
}

fn format_let_value(value: Expr) -> Doc {
    nested_if_needed(
        matches!(value, Expr::Pipe { .. } | Expr::If { .. }),
        expr_to_doc(value, false),
    )
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
                Doc::text(" ="),
                format_let_value(value).force_broken(),
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

impl From<Program> for Doc {
    fn from(program: Program) -> Self {
        let v = program
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
