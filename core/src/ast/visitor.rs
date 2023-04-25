use crate::ast::{Expr, Statement};

fn with_expr_visitor<F>(visitor: &F, expr: Expr) -> (Expr, bool)
where
    F: Fn(Expr) -> (Expr, bool),
{
    let (returned_expr, modified) = visitor(expr);

    match returned_expr {
        Expr::Lit(_) | Expr::Ident(_) => (returned_expr, modified),
        Expr::Infix(op, left, right) => {
            let (left, modified_left) = with_expr_visitor(visitor, *left);
            let (right, modified_right) = with_expr_visitor(visitor, *right);

            let expr = Expr::Infix(op, Box::new(left), Box::new(right));

            if modified || modified_left || modified_right {
                with_expr_visitor(visitor, expr)
            } else {
                (expr, false)
            }
        }

        Expr::Do(left, right) => {
            let (left, _) = with_expr_visitor(visitor, *left);
            let (right, _) = with_expr_visitor(visitor, *right);
            (Expr::Do(Box::new(left), Box::new(right)), false)
        }

        Expr::If {
            condition,
            if_branch,
            else_branch,
        } => {
            let (condition, _) = with_expr_visitor(visitor, *condition);
            let (if_branch, _) = with_expr_visitor(visitor, *if_branch);
            let (else_branch, _) = with_expr_visitor(visitor, *else_branch);

            (
                Expr::If {
                    condition: Box::new(condition),
                    if_branch: Box::new(if_branch),
                    else_branch: Box::new(else_branch),
                },
                false,
            )
        }

        Expr::Prefix(_, _) => todo!(),
        Expr::Call { .. } => todo!(),
        Expr::Let { .. } => todo!(),
        Expr::Fn { .. } => todo!(),
    }
}

pub fn visit_expr<F>(visitor: F, program: Vec<Statement>) -> Vec<Statement>
where
    F: Fn(Expr) -> (Expr, bool),
{
    program
        .into_iter()
        .map(|statement| match statement {
            Statement::Import(_) => statement,
            Statement::Let {
                name,
                value,
                public,
            } => Statement::Let {
                name,
                public,
                value: {
                    let (value, _) = with_expr_visitor(&visitor, value);
                    value
                },
            },
            Statement::Expr(expr) => Statement::Expr({
                let (value, _) = with_expr_visitor(&visitor, expr);
                value
            }),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::ast::visitor::visit_expr;
    use crate::ast::{Expr, Lit, Statement};
    use std::ops::Deref;

    // subst number 1 with 42
    fn simple_visitor(expr: Expr) -> (Expr, bool) {
        match expr {
            Expr::Lit(Lit::Num(n)) if n == 1.0 => (42.0.into(), true),
            _ => (expr, false),
        }
    }

    #[test]
    fn test_visit_expr() {
        let result = visit_expr(simple_visitor, vec![Statement::Expr(1.0.into())]);
        assert_eq!(result, vec![Statement::Expr(42.0.into())],)
    }

    #[test]
    fn test_visit_nested_expr() {
        let result = visit_expr(
            simple_visitor,
            vec![Statement::Expr(Expr::Infix(
                "+".to_string(),
                Box::new(1.0.into()),
                Box::new(2.0.into()),
            ))],
        );

        assert_eq!(
            result,
            vec![Statement::Expr(Expr::Infix(
                "+".to_string(),
                Box::new(42.0.into()),
                Box::new(2.0.into()),
            ))],
        )
    }

    #[test]
    fn test_visit_nested_expr_many_times() {
        fn sum_visitor(expr: Expr) -> (Expr, bool) {
            match &expr {
                Expr::Infix(op, left, right) if op == "+" => match (left.deref(), right.deref()) {
                    (Expr::Lit(Lit::Num(x)), Expr::Lit(Lit::Num(y))) => ((x + y).into(), true),
                    _ => (expr, false),
                },
                _ => (expr, false),
            }
        }

        let result = visit_expr(
            sum_visitor,
            vec![Statement::Expr(Expr::Infix(
                "+".to_string(),
                Box::new(1.0.into()),
                Box::new(Expr::Infix(
                    "+".to_string(),
                    Box::new(2.0.into()),
                    Box::new(3.0.into()),
                )),
            ))],
        );

        assert_eq!(result, vec![Statement::Expr((1.0 + 2.0 + 3.0).into())])
    }
}
