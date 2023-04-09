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
                Doc::text(" {"),
                Doc::vec(&[break_(), Into::<Doc>::into(*if_branch)])
                    .group()
                    .nest(TAB_SIZE),
                break_(),
                Doc::text("} else {"),
                Doc::vec(&[break_(), Into::<Doc>::into(*else_branch)])
                    .group()
                    .nest(TAB_SIZE),
                break_(),
                Doc::text("}"),
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
                    Doc::vec(&[break_(), Into::<Doc>::into(*body)])
                        .group()
                        .nest(TAB_SIZE),
                    break_(),
                    Doc::text("}"),
                ])
            }

            Expr::Do(_, _) => todo!(),
            Expr::Prefix(_, _) => todo!(),
            Expr::Infix(_, _, _) => todo!(),
            Expr::Pipe(_, _) => todo!(),
            Expr::Call { .. } => todo!(),
            Expr::Let { .. } => todo!(),
            Expr::Use { .. } => todo!(),
        }
    }
}

impl Into<Doc> for Statement {
    fn into(self) -> Doc {
        match self {
            Statement::Let { .. } => todo!(),
            Statement::Import(_) => todo!(),
            Statement::Expr(e) => e.into(),
        }
    }
}

impl Into<Doc> for Program {
    fn into(self) -> Doc {
        let v = self.statements.into_iter().map(|s| s.into()).collect();
        Doc::Vec(v)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Ident, Lit};
    use crate::cst::Expr;
    use crate::formatter::PPRINT_W;
    use crate::parser::parse_expr;
    use crate::pretty::pprint;

    #[test]
    fn expr_fmt() {
        assert_fmt("nil");
        assert_fmt("42");
        assert_fmt("true");
        assert_fmt("false");
        assert_fmt("\"abc\"");

        assert_eq!(pprint(10, Expr::Lit(Lit::Num(42.0))), "42");
    }

    #[test]
    fn expr_ident() {
        assert_eq!(
            pprint(PPRINT_W, Expr::Ident(Ident(None, "x".to_string()))),
            "x"
        );
    }

    #[test]
    fn if_ident() {
        let expr = "if cond {
  expr_a
} else {
  expr_b
}";

        assert_fmt(expr);
    }

    #[test]
    fn fn_expr() {
        assert_fmt("fn { nil }");
        assert_fmt("fn a { nil }");
        assert_fmt(
            "fn a, b {
  nil
}",
        );
        assert_fmt(
            "fn a, b, c {
  nil
}",
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
}";

        assert_fmt(expr);
    }

    fn assert_fmt(expr: &str) {
        assert_eq!(
            pprint(PPRINT_W, parse_expr(expr).unwrap()),
            expr.to_string(),
        );
    }
}
