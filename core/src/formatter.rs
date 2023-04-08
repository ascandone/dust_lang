use crate::cst::{Expr, Statement};
use crate::pretty::Doc;

const TAB_SIZE: usize = 2;

pub fn break_() -> Doc {
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

            Expr::Do(_, _) => todo!(),
            Expr::Prefix(_, _) => todo!(),
            Expr::Infix(_, _, _) => todo!(),
            Expr::Pipe(_, _) => todo!(),
            Expr::Call { .. } => todo!(),
            Expr::Let { .. } => todo!(),
            Expr::Use { .. } => todo!(),
            Expr::Fn { .. } => todo!(),
        }
    }
}

impl Into<Doc> for Statement {
    fn into(self) -> Doc {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Ident, Lit};
    use crate::cst::{Expr, NIL};
    use crate::parser::parse_expr;
    use crate::pretty::pprint;

    #[test]
    fn expr_fmt() {
        assert_eq!(pprint(10, NIL), "nil");
        assert_eq!(pprint(10, Expr::Lit(Lit::Num(42.0))), "42");
        assert_eq!(
            pprint(10, Expr::Lit(Lit::String("abc".to_string()))),
            "\"abc\""
        );
        assert_eq!(pprint(10, Expr::Lit(Lit::Bool(true))), "true");
        assert_eq!(pprint(10, Expr::Lit(Lit::Bool(false))), "false");
    }

    #[test]
    fn expr_ident() {
        assert_eq!(pprint(10, Expr::Ident(Ident(None, "x".to_string()))), "x");
    }

    #[test]
    fn if_ident() {
        let expr = "if cond { expr_a } else { expr_b }";

        let out = "if cond {
  expr_a
} else {
  expr_b
}";
        assert_eq!(pprint(10, parse_expr(expr).unwrap()), out.to_string());
    }
}
