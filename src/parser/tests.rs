#[cfg(test)]
mod tests {
    use crate::{
        ast::{Expr, Statement, NIL},
        parser::{parse, parse_expr},
    };

    #[test]
    fn parse_nil() {
        assert_eq!(parse_expr("nil").unwrap(), NIL)
    }

    #[test]
    fn parse_bool() {
        assert_eq!(parse_expr("true").unwrap(), true.into());
        assert_eq!(parse_expr("false").unwrap(), false.into());
    }

    #[test]
    fn parse_num() {
        assert_eq!(parse_expr("0").unwrap(), 0.0.into());
        assert_eq!(parse_expr("10").unwrap(), 10.0.into());
    }

    #[test]
    fn parse_let_decl() {
        assert_eq!(
            parse("let x = nil").unwrap(),
            vec![Statement::Let {
                name: "x".to_string(),
                value: NIL
            }]
        )
    }

    #[test]
    fn parse_expr_program() {
        assert_eq!(parse("42").unwrap(), vec![Statement::Expr(42.0.into(),)])
    }

    #[test]
    fn parse_prefix() {
        assert_eq!(
            parse_expr("! true").unwrap(),
            Expr::Prefix("!".to_string(), Box::new(true.into()))
        );

        assert_eq!(
            parse_expr("!true").unwrap(),
            Expr::Prefix("!".to_string(), Box::new(true.into()))
        );

        assert_eq!(
            parse_expr("!!true").unwrap(),
            Expr::Prefix(
                "!".to_string(),
                Box::new(Expr::Prefix("!".to_string(), Box::new(true.into()))),
            )
        );

        assert_eq!(
            parse_expr("- 1").unwrap(),
            Expr::Prefix("-".to_string(), Box::new(1.0.into()))
        );
    }

    #[test]
    fn parse_infix() {
        assert_eq!(
            parse_expr("1 + 2").unwrap(),
            Expr::Infix("+".to_string(), Box::new(1.0.into()), Box::new(2.0.into()))
        );
    }

    #[test]
    fn parse_infix_twice() {
        // (+ (* 1 2) 3)
        assert_eq!(
            parse_expr("1 * 2 + 3").unwrap(),
            Expr::Infix(
                "+".to_string(),
                Box::new(Expr::Infix(
                    "*".to_string(),
                    Box::new(1.0.into()),
                    Box::new(2.0.into())
                )),
                Box::new(3.0.into()),
            )
        );
    }

    #[test]
    fn parse_infix_mixed() {
        assert_eq!(
            parse_expr("a + b * Z + c").unwrap(),
            Expr::Infix(
                "+".to_string(),
                Box::new(Expr::Infix(
                    "+".to_string(),
                    Box::new(Expr::Ident("a".to_string())),
                    Box::new(Expr::Infix(
                        "*".to_string(),
                        Box::new(Expr::Ident("b".to_string())),
                        Box::new(Expr::Ident("Z".to_string())),
                    )),
                )),
                Box::new(Expr::Ident("c".to_string())),
            )
        );
    }

    #[test]
    fn parse_parenthesized_expr() {
        assert_eq!(
            parse_expr("(1 + 2) * 3").unwrap(),
            Expr::Infix(
                "*".to_string(),
                Box::new(Expr::Infix(
                    "+".to_string(),
                    Box::new(1.0.into()),
                    Box::new(2.0.into()),
                )),
                Box::new(3.0.into()),
            )
        );
    }

    #[test]
    fn parse_call_expr_no_args() {
        assert_eq!(
            parse_expr("f()").unwrap(),
            Expr::Call {
                f: Box::new(Expr::Ident("f".to_string())),
                args: vec![]
            }
        );
    }

    #[test]
    fn parse_call_expr_one_arg() {
        assert_eq!(
            parse_expr("f(42)").unwrap(),
            Expr::Call {
                f: Box::new(Expr::Ident("f".to_string())),
                args: vec![42.0.into()]
            }
        );
    }

    #[test]
    fn parse_call_expr_two_args() {
        assert_eq!(
            parse_expr("f(0, 1)").unwrap(),
            Expr::Call {
                f: Box::new(Expr::Ident("f".to_string())),
                args: vec![0.0.into(), 1.0.into()]
            }
        );
    }

    #[test]
    fn parse_fn_expr_no_params() {
        assert_eq!(
            parse_expr("fn { nil }").unwrap(),
            Expr::Fn {
                params: vec![],
                body: Box::new(NIL)
            }
        );
    }

    #[test]
    fn parse_fn_expr_one_params() {
        assert_eq!(
            parse_expr("fn x { nil }").unwrap(),
            Expr::Fn {
                params: vec!["x".to_string()],
                body: Box::new(NIL)
            }
        );
    }

    #[test]
    fn parse_fn_expr_two_params() {
        assert_eq!(
            parse_expr("fn x, y { nil }").unwrap(),
            Expr::Fn {
                params: vec!["x".to_string(), "y".to_string()],
                body: Box::new(NIL)
            }
        );
    }

    #[test]
    fn parse_if_expr() {
        assert_eq!(
            parse_expr("if true { 0 } else { 1 }").unwrap(),
            Expr::If {
                condition: Box::new(true.into()),
                if_branch: Box::new(0.0.into()),
                else_branch: Box::new(1.0.into())
            }
        );
    }

    #[test]
    fn parse_parenthesized_if_expr() {
        assert_eq!(
            parse_expr("(if true { 0 } else { 1 })").unwrap(),
            Expr::If {
                condition: Box::new(true.into()),
                if_branch: Box::new(0.0.into()),
                else_branch: Box::new(1.0.into())
            }
        );
    }

    #[test]
    fn parse_let_inside_if() {
        assert_eq!(
            parse_expr("if true { let x = 0; 100 } else { 1 }").unwrap(),
            Expr::If {
                condition: Box::new(true.into()),
                if_branch: Box::new(Expr::Let {
                    name: "x".to_string(),
                    value: Box::new(0.0.into()),
                    body: Box::new(100.0.into())
                }),
                else_branch: Box::new(1.0.into())
            }
        );
    }

    #[test]
    fn parse_nested_let_inside_if() {
        assert_eq!(
            parse_expr("if true { let x = 0; let y = 1; 100 } else { 1 }").unwrap(),
            Expr::If {
                condition: Box::new(true.into()),
                if_branch: Box::new(Expr::Let {
                    name: "x".to_string(),
                    value: Box::new(0.0.into()),
                    body: Box::new(Expr::Let {
                        name: "y".to_string(),
                        value: Box::new(1.0.into()),
                        body: Box::new(100.0.into())
                    })
                }),
                else_branch: Box::new(1.0.into())
            }
        );
    }

    #[test]
    fn parse_let_inside_fn_body() {
        assert_eq!(
            parse_expr("fn { let x = 0; 1 }").unwrap(),
            Expr::Fn {
                params: vec![],
                body: Box::new(Expr::Let {
                    name: "x".to_string(),
                    value: Box::new(0.0.into()),
                    body: Box::new(1.0.into())
                })
            }
        );
    }

    #[test]
    fn parse_block_expr() {
        assert_eq!(parse_expr("{ 42 }").unwrap(), 42.0.into());
    }

    #[test]
    fn parse_let_inside_block_expr() {
        assert_eq!(
            parse_expr("{ let x = 0; 1 }").unwrap(),
            Expr::Let {
                name: "x".to_string(),
                value: Box::new(0.0.into()),
                body: Box::new(1.0.into()),
            }
        );
    }
}
