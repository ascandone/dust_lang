use super::ast::Namespace;
use crate::ast::{Ident, Lit, Pattern};
use crate::cst::{ident, Expr, Import, Program, Statement, NIL};
use crate::parser::{parse, parse_ast, parse_expr};

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
fn parse_str() {
    assert_eq!(parse_expr("\"\"").unwrap(), "".into());
    assert_eq!(parse_expr("\"abc\"").unwrap(), "abc".into());
}

#[test]
fn parse_ident() {
    assert_eq!(parse_expr("abc").unwrap(), ident("abc"));
    assert_eq!(parse_expr("abc1").unwrap(), ident("abc1"));
}

#[test]
fn parse_qualified_ident() {
    assert_eq!(
        parse_expr("A.abc").unwrap(),
        Expr::Ident(Ident(
            Some(Namespace(vec!["A".to_string()])),
            "abc".to_string()
        ))
    );

    assert_eq!(
        parse_expr("A.B.C.abc").unwrap(),
        Expr::Ident(Ident(
            Some(Namespace(vec![
                "A".to_string(),
                "B".to_string(),
                "C".to_string()
            ])),
            "abc".to_string()
        ))
    );
}

#[test]
fn parse_let_decl() {
    assert_eq!(
        parse("let x = nil").unwrap(),
        Program {
            statements: vec![Statement::Let {
                public: false,
                name: "x".into(),
                value: NIL
            }]
        },
    )
}

#[test]
fn parse_expr_program() {
    assert_eq!(
        parse("42").unwrap(),
        Program {
            statements: vec![Statement::Expr(42.0.into())]
        }
    )
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
        parse_expr("a + b * z + c").unwrap(),
        Expr::Infix(
            "+".to_string(),
            Box::new(Expr::Infix(
                "+".to_string(),
                Box::new(ident("a")),
                Box::new(Expr::Infix(
                    "*".to_string(),
                    Box::new(ident("b")),
                    Box::new(ident("z")),
                )),
            )),
            Box::new(ident("c")),
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
            f: Box::new(ident("f")),
            args: vec![]
        }
    );
}

#[test]
fn parse_call_expr_one_arg() {
    assert_eq!(
        parse_expr("f(42)").unwrap(),
        Expr::Call {
            f: Box::new(ident("f")),
            args: vec![42.0.into()]
        }
    );
}

#[test]
fn parse_call_expr_two_args() {
    assert_eq!(
        parse_expr("f(0, 1)").unwrap(),
        Expr::Call {
            f: Box::new(ident("f")),
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
fn parse_nested_if_sugar() {
    assert_eq!(
        parse_expr("if true { 0 } else if false { 1 } else { 2 }").unwrap(),
        Expr::If {
            condition: Box::new(true.into()),
            if_branch: Box::new(0.0.into()),
            else_branch: Box::new(Expr::If {
                condition: Box::new(false.into()),
                if_branch: Box::new(1.0.into()),
                else_branch: Box::new(2.0.into())
            })
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
                pattern: "x".into(),
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
                pattern: "x".into(),
                value: Box::new(0.0.into()),
                body: Box::new(Expr::Let {
                    pattern: "y".into(),
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
                pattern: "x".into(),
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
            pattern: "x".into(),
            value: Box::new(0.0.into()),
            body: Box::new(1.0.into()),
        }
    );
}

#[test]
fn parse_comma_statements() {
    assert_eq!(
        parse_expr("{ 0; 1 }").unwrap(),
        Expr::Do(Box::new(0.0.into()), Box::new(1.0.into()),)
    );
}

#[test]
fn parse_comma_statements_with_infix() {
    assert_eq!(
        parse_expr("{ 2 + 3; 1 }").unwrap(),
        Expr::Do(
            Box::new(Expr::Infix(
                "+".to_string(),
                Box::new(2.0.into()),
                Box::new(3.0.into()),
            )),
            Box::new(1.0.into()),
        )
    );
}

#[test]
fn parse_comma_statements_twice() {
    assert_eq!(
        parse_expr("{ 0;1;2 }").unwrap(),
        Expr::Do(
            Box::new(0.0.into()),
            Box::new(Expr::Do(Box::new(1.0.into()), Box::new(2.0.into()))),
        )
    );
}

#[test]
fn parse_let_and_semicolon() {
    assert_eq!(
        parse_expr("{ let x = 0; 1; 2 }").unwrap(),
        Expr::Let {
            pattern: "x".into(),
            value: Box::new(0.0.into()),
            body: Box::new(Expr::Do(Box::new(1.0.into()), Box::new(2.0.into()),))
        },
    );
}

#[test]
fn parse_let_statement_and_semicolon() {
    assert_eq!(
        parse("let x = 0; 1; 2").unwrap(),
        Program {
            statements: vec![
                Statement::Let {
                    public: false,
                    name: "x".into(),
                    value: 0.0.into(),
                },
                Statement::Expr(1.0.into()),
                Statement::Expr(2.0.into()),
            ]
        }
    );
}

#[test]
fn parse_let_statement_and_semicolon_with_infix() {
    assert_eq!(
        parse("let x = 0; 1 + 2; 3").unwrap(),
        Program {
            statements: vec![
                Statement::Let {
                    public: false,
                    name: "x".into(),
                    value: 0.0.into(),
                },
                Statement::Expr(Expr::Infix(
                    "+".to_string(),
                    Box::new(1.0.into()),
                    Box::new(2.0.into()),
                )),
                Statement::Expr(3.0.into()),
            ]
        }
    );
}

#[test]
fn parse_let_star_sugar() {
    let sugar = "
        {
            use a <- f(x, y);
            expr(a)
        }
    ";

    let desugared = "
        f(x, y, fn a {
            expr(a)
        })
    ";

    assert_eq!(parse_ast(sugar).unwrap(), parse_ast(desugared).unwrap());
}

#[test]
fn parse_let_star_sugar_no_args() {
    let sugar = "
        {
            use <- f(x, y);
            expr()
        }
    ";

    let desugared = "
        f(x, y, fn {
            expr()
        })
    ";

    assert_eq!(parse_ast(sugar).unwrap(), parse_ast(desugared).unwrap());
}

#[test]
fn parse_let_star_sugar_many_args() {
    let sugar = "
        {
            use a, b <- f(x, y);
            expr(a, b)
        }
    ";

    let desugared = "
        f(x, y, fn a, b {
            expr(a, b)
        })
    ";

    assert_eq!(parse_ast(sugar).unwrap(), parse_ast(desugared).unwrap());
}

#[test]
fn parse_nested_let_star_sugar() {
    let sugar = "
        {
            use a <- f(x, y);
            let b = 100;
            use c <- g(b, a);
            h(c)
        }
    ";

    let desugared = "
        f(x, y, fn a {
            let b = 100;
            g(b, a, fn c {
                h(c)
            })
        })
    ";

    assert_eq!(parse_ast(sugar).unwrap(), parse_ast(desugared).unwrap());
}

#[test]
fn parse_pub_let() {
    assert_eq!(
        parse("pub let x = nil").unwrap(),
        Program {
            statements: vec![Statement::Let {
                public: true,
                name: "x".into(),
                value: NIL
            }]
        }
    );
}

#[test]
fn parse_import_statement() {
    assert_eq!(
        parse("import A").unwrap(),
        Program {
            statements: vec![Statement::Import(Import {
                ns: Namespace(vec!["A".to_string()]),
                rename: None
            })]
        }
    );

    assert_eq!(
        parse("import A.B").unwrap(),
        Program {
            statements: vec![Statement::Import(Import {
                ns: Namespace(vec!["A".to_string(), "B".to_string()]),
                rename: None
            })]
        }
    );
}

#[test]
fn parse_import_statement_rename() {
    assert_eq!(
        parse("import A as B").unwrap(),
        Program {
            statements: vec![Statement::Import(Import {
                ns: Namespace(vec!["A".to_string()]),
                rename: Some(Namespace(vec!["B".to_string()]))
            })]
        }
    );
}

#[test]
fn parse_empty_list_lit() {
    assert_eq!(parse_expr("[]").unwrap(), Expr::EmptyList);
    assert_eq!(parse_expr("[ ]").unwrap(), Expr::EmptyList);
}

#[test]
fn parse_singleton_list_lit() {
    assert_eq!(
        parse_expr("[ 1 ]").unwrap(),
        Expr::Cons(Box::new(1.0.into()), Box::new(Expr::EmptyList))
    );
}

#[test]
fn parse_list_lit() {
    assert_eq!(
        parse_expr("[ 1, 2, 3 ]").unwrap(),
        Expr::Cons(
            Box::new(1.0.into()),
            Box::new(Expr::Cons(
                Box::new(2.0.into()),
                Box::new(Expr::Cons(
                    Box::new(3.0.into()),
                    Box::new(Expr::EmptyList) //
                ))
            ))
        )
    );
}

#[test]
fn parse_list_cons() {
    assert_eq!(
        parse_expr("[ hd, ..tl ]").unwrap(),
        Expr::Cons(Box::new(ident("hd")), Box::new(ident("tl"),))
    );
}

#[test]
fn parse_mixed_list_cons() {
    assert_eq!(
        parse_expr("[ x, y, ..tl ]").unwrap(),
        Expr::Cons(
            Box::new(ident("x")),
            Box::new(Expr::Cons(Box::new(ident("y")), Box::new(ident("tl"))))
        )
    );
}

#[test]
fn parse_empty_map() {
    assert_eq!(parse_expr("#{}").unwrap(), Expr::EmptyMap);
}

#[test]
fn parse_singleton_map() {
    assert_eq!(
        parse_expr("#{ \"x\" => 42 }").unwrap(),
        Expr::ConsMap(
            (Box::new("x".into()), Box::new(42.0.into())),
            Box::new(Expr::EmptyMap)
        )
    );
}

#[test]
fn parse_cons_map() {
    assert_eq!(
        parse_expr("#{ \"x\" => 42, ..tl }").unwrap(),
        Expr::ConsMap(
            (Box::new("x".into()), Box::new(42.0.into())),
            Box::new(ident("tl"))
        )
    );
}

#[test]
fn parse_empty_match() {
    assert_eq!(
        parse_expr("match true {}").unwrap(),
        Expr::Match(Box::new(true.into()), vec![])
    );
}

#[test]
fn parse_const_num_clause_match() {
    assert_eq!(
        parse_expr(
            "match x {
    0 => a,
}"
        )
        .unwrap(),
        Expr::Match(
            Box::new(ident("x")),
            vec![
                //
                (Pattern::Lit(Lit::Num(0.0)), ident("a"))
            ]
        )
    );
}

#[test]
fn parse_const_true_match() {
    assert_eq!(
        parse_expr(
            "match x {
    true => a,
}"
        )
        .unwrap(),
        Expr::Match(
            Box::new(ident("x")),
            vec![
                //
                (Pattern::Lit(Lit::Bool(true)), ident("a"))
            ]
        )
    );
}

#[test]
fn parse_const_false_match() {
    assert_eq!(
        parse_expr(
            "match x {
    false => a,
}"
        )
        .unwrap(),
        Expr::Match(
            Box::new(ident("x")),
            vec![
                //
                (Pattern::Lit(Lit::Bool(false)), ident("a"))
            ]
        )
    );
}

#[test]
fn parse_const_nil_match() {
    assert_eq!(
        parse_expr(
            "match x {
    nil => a,
}"
        )
        .unwrap(),
        Expr::Match(
            Box::new(ident("x")),
            vec![
                //
                (Pattern::Lit(Lit::Nil), ident("a"))
            ]
        )
    );
}

#[test]
fn parse_const_str_match() {
    assert_eq!(
        parse_expr(
            "match x {
    \"abc\" => a,
}"
        )
        .unwrap(),
        Expr::Match(
            Box::new(ident("x")),
            vec![
                //
                (Pattern::Lit(Lit::String("abc".to_string())), ident("a"))
            ]
        )
    );
}

#[test]
fn parse_ident_match() {
    assert_eq!(
        parse_expr(
            "match x {
    id => a,
}"
        )
        .unwrap(),
        Expr::Match(
            Box::new(ident("x")),
            vec![
                //
                (Pattern::Identifier("id".to_string()), ident("a"))
            ]
        )
    );
}

#[test]
fn parse_empty_list_match() {
    assert_eq!(
        parse_expr(
            "match x {
    [] => a,
}"
        )
        .unwrap(),
        Expr::Match(
            Box::new(ident("x")),
            vec![
                //
                (Pattern::EmptyList, ident("a"))
            ]
        )
    );
}

#[test]
fn parse_empty_map_match() {
    assert_eq!(
        parse_expr(
            "match x {
    #{} => a,
}"
        )
        .unwrap(),
        Expr::Match(
            //
            Box::new(ident("x")),
            vec![(Pattern::EmptyMap, ident("a"))]
        )
    );
}

#[test]
fn parse_cons_map_match() {
    assert_eq!(
        parse_expr(
            "match x {
    #{ \"k\" => v, .. rest } => a,
}"
        )
        .unwrap(),
        Expr::Match(
            //
            Box::new(ident("x")),
            vec![(
                Pattern::ConsMap(
                    (
                        "k".to_string(),
                        Box::new(Pattern::Identifier("v".to_string()))
                    ),
                    Box::new(Pattern::Identifier("rest".to_string()))
                ),
                ident("a")
            )]
        )
    );
}

#[test]
fn parse_cons_list_match() {
    assert_eq!(
        parse_expr(
            "match x {
    [hd, ..tl] => a,
}"
        )
        .unwrap(),
        Expr::Match(
            Box::new(ident("x")),
            vec![
                //
                (
                    Pattern::Cons(
                        Box::new(Pattern::Identifier("hd".to_string())),
                        Box::new(Pattern::Identifier("tl".to_string())),
                    ),
                    ident("a")
                )
            ]
        )
    );
}

#[test]
fn parse_nested_cons_list_match() {
    assert_eq!(
        parse_expr(
            "match x {
    [42, ..[]] => a,
}"
        )
        .unwrap(),
        Expr::Match(
            Box::new(ident("x")),
            vec![(
                Pattern::Cons(
                    Box::new(Pattern::Lit(Lit::Num(42.0))),
                    Box::new(Pattern::EmptyList),
                ),
                ident("a")
            )]
        )
    );
}

#[test]
fn parse_tuple_match() {
    assert_eq!(
        parse_expr(
            "match x {
    #(x, y) => a,
    #(x, y, z) => b,
}"
        )
        .unwrap(),
        Expr::Match(
            Box::new(ident("x")),
            vec![
                (
                    Pattern::Tuple(vec![
                        Pattern::Identifier("x".to_string()),
                        Pattern::Identifier("y".to_string()),
                    ]),
                    ident("a")
                ),
                (
                    Pattern::Tuple(vec![
                        Pattern::Identifier("x".to_string()),
                        Pattern::Identifier("y".to_string()),
                        Pattern::Identifier("z".to_string()),
                    ]),
                    ident("b")
                ),
            ]
        )
    );
}

#[test]
fn parse_nested_cons_list_match_sugar() {
    assert_eq!(
        parse_expr(
            "match x {
    [42] => a,
    [1, 2, 3, ..tl] => b,
}"
        )
        .unwrap(),
        Expr::Match(
            Box::new(ident("x")),
            vec![
                (
                    Pattern::Cons(
                        Box::new(Pattern::Lit(Lit::Num(42.0))),
                        Box::new(Pattern::EmptyList),
                    ),
                    ident("a")
                ),
                (
                    Pattern::Cons(
                        Box::new(Pattern::Lit(Lit::Num(1.0))),
                        Box::new(Pattern::Cons(
                            Box::new(Pattern::Lit(Lit::Num(2.0))),
                            Box::new(Pattern::Cons(
                                Box::new(Pattern::Lit(Lit::Num(3.0))),
                                Box::new(Pattern::Identifier("tl".to_string())),
                            ),),
                        )),
                    ),
                    ident("b")
                )
            ]
        )
    );
}

#[test]
fn parse_tuple() {
    assert_eq!(
        parse_expr("#(1, 2)").unwrap(),
        Expr::Tuple(vec![1.0.into(), 2.0.into()])
    );

    assert_eq!(
        parse_expr("#(1, 2, 3)").unwrap(),
        Expr::Tuple(vec![1.0.into(), 2.0.into(), 3.0.into()])
    );
}
