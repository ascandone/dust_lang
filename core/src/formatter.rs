use crate::cst;
use crate::cst::{Expr, Program, Statement};
use crate::pretty::Doc;
use std::ptr::write;

impl Into<Doc> for Expr {
    fn into(self) -> Doc {
        match self {
            Expr::Lit(l) => Doc::Text(format!("{}", l).to_string()),
            Expr::Ident(id) => todo!(),
            Expr::Do(_, _) => todo!(),
            Expr::If { .. } => todo!(),
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
    use crate::ast::Lit;
    use crate::cst::{Expr, Ident, NIL};
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
}
