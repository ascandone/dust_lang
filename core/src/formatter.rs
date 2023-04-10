use crate::cst::{Expr, Program, Statement};
use crate::pretty::{pprint, Doc};

const TAB_SIZE: usize = 2;
const PPRINT_W: isize = 12;

pub fn format(program: Program) -> String {
    pprint(PPRINT_W, program)
}

fn break_() -> Doc {
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
                    Doc::vec(&[break_(), Into::<Doc>::into(*if_branch)]).nest(TAB_SIZE),
                    break_(),
                    Doc::text("} else {"),
                    Doc::vec(&[break_(), Into::<Doc>::into(*else_branch)]).nest(TAB_SIZE),
                    break_(),
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
                            break_(),
                            Into::<Doc>::into(*body).group(),
                        ])
                        .nest(TAB_SIZE),
                        break_(),
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
            Statement::Import(_) => todo!(),
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
                        Doc::text(";\n\n")
                    },
                    s.into(),
                ])
            })
            .collect();

        Doc::vec(&[Doc::Vec(v), Doc::text("\n")])
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Lit;
    use crate::cst::Expr;
    use crate::formatter::PPRINT_W;
    use crate::parser::parse;
    use crate::pretty::pprint;

    #[test]
    fn expr_fmt() {
        assert_fmt("nil\n");
        assert_fmt("42\n");
        assert_fmt("true\n");
        assert_fmt("false\n");
        assert_fmt("\"abc\"\n");

        assert_eq!(pprint(PPRINT_W, Expr::Lit(Lit::Num(42.0))), "42");
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
    fn multiple_statements() {
        assert_fmt(
            "let x = 1;

let y = 2
",
        );
    }

    fn assert_fmt(expr: &str) {
        assert_eq!(pprint(PPRINT_W, parse(expr).unwrap()), expr.to_string());
    }
}
