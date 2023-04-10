use crate::cst::{Expr, Import, Program, Statement};
use crate::pretty::{Doc, PPrint};

pub fn format(program: Program) -> String {
    format!(
        "{}",
        PPrint {
            max_w: 12,
            nest_size: 2,
            doc: program.into(),
        }
    )
}

fn space_break() -> Doc {
    Doc::Break(" ".to_string())
}

impl Into<Doc> for Expr {
    fn into(self) -> Doc {
        match self {
            Expr::Lit(l) => Doc::Text(format!("{l}").to_string()),
            Expr::Ident(id) => Doc::Text(format!("{id}").to_string()),
            Expr::If {
                condition,
                if_branch,
                else_branch,
            } => Doc::vec(&[
                Doc::text("if "),
                (*condition).into(),
                Doc::vec(&[
                    Doc::text(" {"),
                    Doc::vec(&[space_break(), Into::<Doc>::into(*if_branch)]).nest(),
                    space_break(),
                    Doc::text("} else {"),
                    Doc::vec(&[space_break(), Into::<Doc>::into(*else_branch)]).nest(),
                    space_break(),
                    Doc::text("}"),
                ])
                .group(),
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
                            Into::<Doc>::into(*body).group(),
                        ])
                        .nest(),
                        space_break(),
                        Doc::text("}"),
                    ])
                    .group(),
                ])
            }

            Expr::Call { f, args } => Doc::vec(&[
                (*f).into(),
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

            Expr::Do(_, _) => todo!(),
            Expr::Prefix(_, _) => todo!(),
            Expr::Infix(_, _, _) => todo!(),
            Expr::Pipe(_, _) => todo!(),
            Expr::Let { .. } => todo!(),
            Expr::Use { .. } => todo!(),
        }
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

#[cfg(test)]
mod tests {
    use crate::formatter::format;
    use crate::parser::parse;

    #[test]
    fn expr_fmt() {
        assert_fmt("nil\n");
        assert_fmt("42\n"); // TODO fmt float
        assert_fmt("true\n");
        assert_fmt("false\n");
        assert_fmt("\"abc\"\n");
    }

    #[test]
    fn expr_ident() {
        assert_fmt("x\n");
    }

    #[test]
    fn if_ident() {
        let expr = "if cond {
  expr_a
} else {
  expr_b
}
";

        assert_fmt(expr);

        let expr = "if a {
  1
} else {
  2
}
";
        assert_fmt(expr);
    }

    #[test]
    fn fn_expr() {
        assert_fmt("fn { nil }\n");
        assert_fmt(
            "fn {
  1234567
}
",
        );
        assert_fmt("fn a { nil }\n");
        assert_fmt(
            "fn a, b {
  nil
}
",
        );
        assert_fmt(
            "fn a, b, c {
  nil
}
",
        );
    }

    #[test]
    fn fn_expr_wrap() {
        let expr = "fn {
  if cond {
    1
  } else {
    2
  }
}
";

        assert_fmt(expr);
    }

    #[test]
    fn let_statement() {
        assert_fmt("let x = 42\n");
        assert_fmt("pub let x = 42\n");
        assert_fmt(
            "let x = if c {
  1
} else {
  2
}
",
        );
    }

    #[test]
    fn call_expr() {
        assert_fmt("f()\n");
        assert_fmt("f(1)\n");
        assert_fmt("f(1, 2)\n");
        assert_fmt("f(1, 2, 3)\n");
        assert_fmt(
            "f(x, fn {
  nil
})
",
        );
    }

    #[test]
    fn import_statement() {
        assert_fmt("import A\n");
        assert_fmt("import A.B\n");
        assert_fmt("import A.B as C\n");
    }

    #[test]
    fn multiple_statements() {
        assert_fmt(
            "let x = 1;

import A;

let y = 2
",
        );
    }

    fn assert_fmt(expr: &str) {
        assert_eq!(&format(parse(expr).unwrap()), expr);
    }
}
