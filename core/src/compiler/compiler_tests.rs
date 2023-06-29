use crate::ast::Let;
use crate::ast::{ident, Ident, Import, Lit, Namespace, Pattern};
use crate::{
    ast::{Expr, Statement, NIL},
    compiler::compiler::Compiler,
    vm::{
        bytecode::OpCode,
        value::{Function, Value},
    },
};
use std::rc::Rc;
use std::string::ToString;

fn new_compiler() -> Compiler {
    let name = Namespace(vec!["Main".to_string()]);
    Compiler::new(name)
}

#[test]
fn const_true_test() {
    let ast = true.into();
    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(f.arity, 0);
    assert_eq!(
        f.bytecode,
        vec![OpCode::ConstTrue as u8, OpCode::Return as u8]
    );
}

#[test]
fn const_false_test() {
    let ast = false.into();
    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        f.bytecode,
        vec![OpCode::ConstFalse as u8, OpCode::Return as u8]
    );
}

#[test]
fn nil_const_test() {
    let ast = NIL;
    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        f.bytecode,
        vec![OpCode::ConstNil as u8, OpCode::Return as u8]
    );
}

#[test]
fn int_const_test() {
    let ast = 42.0.into();
    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(f.constant_pool[0], Value::Num(42.0));

    assert_eq!(
        f.bytecode,
        vec![OpCode::Const as u8, 0, OpCode::Return as u8]
    );
}

#[test]
fn int_const_are_cached_test() {
    let ast = Expr::Do(Box::new(42.0.into()), Box::new(42.0.into()));

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(f.constant_pool, vec![42.0.into()]);

    assert_eq!(
        f.bytecode,
        vec![
            OpCode::Const as u8,
            0,
            OpCode::Pop as u8,
            OpCode::Const as u8,
            0,
            OpCode::Return as u8
        ]
    );
}

#[test]
fn str_const_are_cached_test() {
    let ast = Expr::Do(Box::new("abc".into()), Box::new("abc".into()));

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(f.constant_pool, vec!["abc".into()]);

    assert_eq!(
        f.bytecode,
        vec![
            OpCode::Const as u8,
            0,
            OpCode::Pop as u8,
            OpCode::Const as u8,
            0,
            OpCode::Return as u8
        ]
    );
}

#[test]
fn string_const_test() {
    let ast = "abc".into();
    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        f.constant_pool[0],
        Value::String(Rc::new("abc".to_string()))
    );

    assert_eq!(
        f.bytecode,
        vec![OpCode::Const as u8, 0, OpCode::Return as u8]
    );
}

#[test]
fn multiple_exprs_do_test() {
    let ast = Expr::Do(Box::new(NIL), Box::new(true.into()));
    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            OpCode::ConstNil as u8,
            OpCode::Pop as u8,
            OpCode::ConstTrue as u8,
            OpCode::Return as u8,
        ]
    );
}

#[test]
fn def_test() {
    let ast = vec![Statement::Let {
        pattern: "x".into(),
        value: true.into(),
        public: false,
    }];

    let f = new_compiler().compile_program(ast, "main").unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            OpCode::ConstTrue as u8,
            OpCode::SetGlobal as u8,
            0,
            0,
            OpCode::ConstNil as u8,
            OpCode::Return as u8
        ]
    );
}

#[test]
fn def_twice_test() {
    let ast = vec![
        Statement::Let {
            pattern: "x".into(),
            value: true.into(),
            public: false,
        },
        Statement::Let {
            pattern: "y".into(),
            value: false.into(),
            public: false,
        },
    ];

    let f = new_compiler().compile_program(ast, "main").unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            OpCode::ConstTrue as u8,
            OpCode::SetGlobal as u8,
            0,
            0,
            OpCode::ConstNil as u8,
            OpCode::Pop as u8,
            OpCode::ConstFalse as u8,
            OpCode::SetGlobal as u8,
            0,
            1,
            OpCode::ConstNil as u8,
            OpCode::Return as u8
        ]
    );
}

#[test]
fn global_scope_test() {
    let ast = vec![
        Statement::Let {
            public: false,
            pattern: "x".into(),
            value: true.into(),
        },
        Statement::Expr(ident("x")),
    ];

    let f = new_compiler().compile_program(ast, "main").unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            OpCode::ConstTrue as u8,
            OpCode::SetGlobal as u8,
            0,
            0,
            OpCode::ConstNil as u8,
            OpCode::Pop as u8,
            OpCode::GetGlobal as u8,
            0,
            0,
            OpCode::Return as u8
        ],
        "{f}"
    );
}

#[ignore]
#[test]
fn global_scope_pattern_test() {
    let ast = vec![Statement::Let {
        public: false,
        pattern: Pattern::EmptyList,
        value: NIL,
    }];

    let f = new_compiler().compile_program(ast, "main").unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            /* 00 */ OpCode::ConstNil as u8,
            /* 01 */ OpCode::SetLocal as u8,
            /* 02 */ 0,
            /* 03 */ OpCode::MatchEmptyListElseJump as u8,
            /* 04 */ 0,
            /* 05 */ 11, // goto PanicNoMatch
            /* 06 */ 0,
            /* 07 */ OpCode::ConstNil as u8,
            /* 08 */ OpCode::Jump as u8,
            /* 09 */ 0,
            /* 10 */ 12, // goto return
            /* 11 */ OpCode::PanicNoMatch as u8,
            /* 12 */ OpCode::Return as u8
        ]
    );
}

#[ignore]
#[test]
fn global_scope_pattern_cons_test() {
    let ast = vec![Statement::Let {
        public: false,
        pattern: Pattern::Cons(
            Box::new(Pattern::Identifier("hd".to_string())),
            Box::new(Pattern::Identifier("tl".to_string())),
        ),
        value: NIL,
    }];

    let f = new_compiler().compile_program(ast, "main").unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            /* 00 */ OpCode::ConstNil as u8,
            /* 01 */ OpCode::GetLocal as u8,
            /* 02 */ 0,
            /* 03 */ OpCode::MatchConsElseJump as u8,
            /* 04 */ 0,
            /* 05 */ 14, // goto PanicNoMatch
            /* 06 */ 0,
            /* 07 */ OpCode::SetGlobal as u8,
            /* 08 */ 0,
            /* 09 */ 0,
            /* 10 */ OpCode::SetGlobal as u8,
            /* 11 */ 0,
            /* 12 */ 1,
            /* 13 */ OpCode::ConstNil as u8,
            /* 14 */ OpCode::Jump as u8,
            /* 15 */ 0,
            /* 16 */ 15, // goto return
            /* 17 */ OpCode::PanicNoMatch as u8,
            /* 18 */ OpCode::Return as u8
        ]
    );
}

#[ignore]
#[test]
fn global_scope_pattern_cons_map_test() {
    let ast = vec![Statement::Let {
        public: false,
        pattern: Pattern::ConsMap(
            (
                "key".to_string(),
                Box::new(Pattern::Identifier("x".to_string())),
            ),
            Box::new(Pattern::Identifier("rest".to_string())),
        ),
        value: NIL,
    }];

    let f = new_compiler().compile_program(ast, "main").unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            /* 00 */ OpCode::ConstNil as u8,
            // /* 01 */ OpCode::MatchConsMapElseJump as u8,
            /* 02 */ 0,
            /* 03 */ 15, // goto PanicNoMatch
            /* 04 */ 0,
            /* 05 */ OpCode::SetGlobal as u8,
            /* 06 */ 0,
            /* 07 */ 0,
            /* 08 */ OpCode::SetGlobal as u8,
            /* 09 */ 0,
            /* 10 */ 1,
            /* 11 */ OpCode::ConstNil as u8,
            /* 12 */ OpCode::Jump as u8,
            /* 13 */ 0,
            /* 14 */ 16, // goto return
            /* 15 */ OpCode::PanicNoMatch as u8,
            /* 16 */ OpCode::Return as u8
        ]
    );
}

#[test]
fn and_test() {
    let ast = Expr::Infix(
        "&&".to_string(),
        Box::new(true.into()),
        Box::new(false.into()),
    );

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            // left
            /*  0 */ OpCode::ConstTrue as u8,
            /*  1 */ OpCode::JumpIfFalseElsePop as u8,
            /*  2 */ 0,
            /*  3 */ 5,
            // right
            /*  4 */ OpCode::ConstFalse as u8,
            /* 11 */ OpCode::Return as u8, // <-
        ]
    );
}

#[test]
fn or_test() {
    let ast = Expr::Infix(
        "||".to_string(),
        Box::new(true.into()),
        Box::new(false.into()),
    );

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            // left
            /*  0 */ OpCode::ConstTrue as u8,
            /*  1 */ OpCode::JumpIfTrueElsePop as u8,
            /*  2 */ 0,
            /*  3 */ 5,
            // right
            /*  4 */ OpCode::ConstFalse as u8,
            /* 11 */ OpCode::Return as u8, // <-
        ]
    );
}

#[test]
fn if_expr_test() {
    let ast = Expr::If {
        condition: Box::new(true.into()),
        if_branch: Box::new(0.0.into()),
        else_branch: Box::new(1.0.into()),
    };

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(f.constant_pool, vec![Value::Num(0.0), Value::Num(1.0),]);

    assert_eq!(
        f.bytecode,
        vec![
            /*  0 */ OpCode::ConstTrue as u8,
            /*  1 */ OpCode::JumpIfFalse as u8,
            /*  2 */ 0,
            /*  3 */ 9,
            /*  4 */ OpCode::Const as u8,
            /*  5 */ 0,
            /*  6 */ OpCode::Jump as u8,
            /*  7 */ 0,
            /*  8 */ 11,
            /*  9 */ OpCode::Const as u8, // <-
            /* 10 */ 1,
            /* 11 */ OpCode::Return as u8, // <-
        ]
    );
}

#[test]
fn lambda_expr_no_args_test() {
    // (lambda* () 42)
    let ast = Expr::Fn {
        params: vec![],
        body: Box::new(42.0.into()),
    };

    let f = new_compiler().compile_expr(ast).unwrap();

    let compiled_lambda = Function {
        bytecode: vec![OpCode::Const as u8, 0, OpCode::Return as u8],
        constant_pool: vec![Value::Num(42.0)],
        ..Function::default()
    };

    assert_eq!(
        f.constant_pool,
        vec![Value::Function(Rc::new(compiled_lambda))]
    );

    assert_eq!(
        f.bytecode,
        vec![OpCode::Const as u8, 0, OpCode::Return as u8,]
    );
}

#[test]
fn infer_lambda_name_from_let_expr() {
    // { let f = fn {nil}; nil }
    let ast = Let {
        name: "f".to_string(),
        value: Box::new(Expr::Fn {
            params: vec![],
            body: Box::new(NIL),
        }),
        body: Box::new(NIL),
    }
    .as_match();

    let main = new_compiler().compile_expr(ast).unwrap();

    let f = &main.constant_pool[0].as_fn().unwrap();

    assert_eq!(f.name, Some("f".to_string()));
}

#[test]
fn infer_lambda_name_from_let_statement() {
    // let f = fn {nil}
    let ast = Statement::Let {
        public: false,
        pattern: "f".into(),
        value: Expr::Fn {
            params: vec![],
            body: Box::new(NIL),
        },
    };

    let main = new_compiler().compile_program(vec![ast], "main").unwrap();

    let f = &main.constant_pool[0].as_fn().unwrap();

    assert_eq!(f.name, Some("f".to_string()));
}

#[test]
fn lambda_expr_required_args_test() {
    // (lambda* (x y) 42)
    let ast = Expr::Fn {
        params: vec!["x".to_string(), "y".to_string()],
        body: Box::new(NIL),
    };

    let f = new_compiler().compile_expr(ast).unwrap();

    let compiled_lambda = Function {
        bytecode: vec![OpCode::ConstNil as u8, OpCode::Return as u8],
        arity: 2,
        ..Function::default()
    };

    assert_eq!(
        f.constant_pool,
        vec![Value::Function(Rc::new(compiled_lambda))]
    );

    assert_eq!(
        f.bytecode,
        vec![OpCode::Const as u8, 0, OpCode::Return as u8,]
    );
}

#[test]
fn lambda_args_lookup_test() {
    // (lambda* (x y) y)
    let ast = Expr::Fn {
        params: vec!["x".to_string(), "y".to_string()],
        body: Box::new(ident("y")),
    };

    let f = new_compiler().compile_expr(ast).unwrap();

    let compiled_lambda = Function {
        arity: 2,
        bytecode: vec![OpCode::GetLocal as u8, 1, OpCode::Return as u8],
        ..Function::default()
    };

    assert_eq!(
        f.constant_pool,
        vec![Value::Function(Rc::new(compiled_lambda))]
    );
}

#[test]
fn f_call_no_args_test() {
    // (lambda* () 42)
    let f = Expr::Fn {
        params: vec![],
        body: Box::new(42.0.into()),
    };

    let ast = Expr::Call {
        f: Box::new(f),
        args: vec![],
    };

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            OpCode::Const as u8,
            0,
            OpCode::Call as u8,
            0,
            OpCode::Return as u8
        ]
    );
}

#[test]
fn f_call_test() {
    // let f _ = nil; (f #true)
    let f = Expr::Fn {
        params: vec![],
        body: Box::new(NIL),
    };

    let ast = Expr::Call {
        f: Box::new(f),
        args: vec![true.into()],
    };

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            OpCode::ConstTrue as u8,
            OpCode::Const as u8,
            0,
            OpCode::Call as u8,
            1,
            OpCode::Return as u8
        ]
    );
}

#[test]
fn let_test() {
    // (let1 (x #true) nil)

    let ast = Let {
        name: "x".to_string(),
        value: Box::new(true.into()),
        body: Box::new(NIL),
    }
    .as_match();

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(f.locals, 1);

    assert_eq!(
        f.bytecode,
        vec![
            OpCode::ConstTrue as u8,
            OpCode::SetLocal as u8,
            0,
            OpCode::ConstNil as u8,
            OpCode::Return as u8
        ]
    );
}

#[test]
fn multiple_let_test() {
    // {
    //   let x = true;
    //   let y = false;
    //   nil
    // }

    let ast = Let {
        name: "x".to_string(),
        value: Box::new(true.into()),
        body: Box::new(
            Let {
                name: "y".to_string(),
                value: Box::new(false.into()),
                body: Box::new(NIL),
            }
            .as_match(),
        ),
    }
    .as_match();

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(f.locals, 2);

    assert_eq!(
        f.bytecode,
        vec![
            OpCode::ConstTrue as u8,
            OpCode::SetLocal as u8,
            0,
            OpCode::ConstFalse as u8,
            OpCode::SetLocal as u8,
            1,
            OpCode::ConstNil as u8,
            OpCode::Return as u8
        ]
    );
}

#[test]
fn local_binding_test() {
    // (let1 (x #true) x)

    let ast = Let {
        name: "x".to_string(),
        value: Box::new(true.into()),
        body: Box::new(ident("x")),
    }
    .as_match();

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            OpCode::ConstTrue as u8,
            OpCode::SetLocal as u8,
            0,
            OpCode::GetLocal as u8,
            0,
            OpCode::Return as u8
        ]
    );
}

#[test]
fn nested_fn() {
    // \x y. y

    let ast = Expr::Fn {
        params: vec!["x".to_string()],
        body: Box::new(Expr::Fn {
            params: vec!["y".to_string()],
            body: Box::new(ident("y")),
        }),
    };

    let main = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        main.bytecode,
        vec![OpCode::Const as u8, 0, OpCode::Return as u8],
        "main bytecode"
    );

    let Value::Function(outer_function) =  &main.constant_pool[0] else {
            panic!("expected a fn");
        };

    assert_eq!(
        outer_function.bytecode,
        vec![OpCode::Const as u8, 0, OpCode::Return as u8],
        "function opcodes"
    );

    assert_eq!(
        outer_function.constant_pool[0],
        Value::Function(Rc::new(Function {
            arity: 1,
            bytecode: vec![OpCode::GetLocal as u8, 0, OpCode::Return as u8],
            ..Default::default()
        })),
        "closure opcodes"
    );
}

#[test]
fn make_closure_test() {
    // \x y. x + y

    let ast = Expr::Fn {
        params: vec!["x".to_string()],
        body: Box::new(Expr::Fn {
            params: vec!["y".to_string()],
            body: Box::new(Expr::Infix(
                "+".to_string(),
                Box::new(ident("x")),
                Box::new(ident("y")),
            )),
        }),
    };

    let main = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        main.bytecode,
        vec![OpCode::Const as u8, 0, OpCode::Return as u8],
        "main bytecode"
    );

    let Value::Function(outer_function) =  &main.constant_pool[0] else {
            panic!("expected a fn");
        };

    assert_eq!(
        outer_function.bytecode,
        vec![
            OpCode::GetLocal as u8,
            0,
            OpCode::MakeClosure as u8,
            1,
            0,
            OpCode::Return as u8
        ],
        "function opcodes"
    );

    assert_eq!(
        outer_function.constant_pool[0],
        Value::Function(Rc::new(Function {
            arity: 1,
            bytecode: vec![
                OpCode::GetFree as u8,
                0,
                OpCode::GetLocal as u8,
                0,
                OpCode::Add as u8,
                OpCode::Return as u8
            ],
            ..Default::default()
        })),
        "closure opcodes"
    );
}

#[test]
fn make_let_closure_test() {
    // { let x = true; fn { x } }
    // match true { x => fn { x } }

    let ast = Let {
        name: "x".to_string(),
        value: Box::new(true.into()),
        body: Box::new(Expr::Fn {
            params: vec![],
            body: Box::new(ident("x")),
        }),
    }
    .as_match();

    let main = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        main.bytecode,
        vec![
            OpCode::ConstTrue as u8,
            OpCode::SetLocal as u8,
            0,
            // lambda:
            OpCode::GetLocal as u8,
            0,
            OpCode::MakeClosure as u8,
            1,
            0,
            OpCode::Return as u8
        ]
    );

    assert_eq!(
        main.constant_pool[0],
        Value::Function(Rc::new(Function {
            bytecode: vec![OpCode::GetFree as u8, 0, OpCode::Return as u8],
            ..Default::default()
        }))
    )
}

#[test]
fn get_current_closure_test() {
    // let f = fn { f }

    let ast = Statement::Let {
        pattern: "f".into(),
        value: Expr::Fn {
            params: vec![],
            body: Box::new(Expr::Ident(Ident(None, "f".to_string()))),
        },
        public: false,
    };

    let main = new_compiler().compile_program(vec![ast], "main").unwrap();
    let f = &main.constant_pool[0].as_fn().unwrap();

    assert_eq!(
        f.bytecode,
        vec![OpCode::GetCurrentClosure as u8, OpCode::Return as u8]
    );
}

#[test]
fn error_on_rec_invalid_params() {
    // let f = fn x { f() }
    let ast = Statement::Let {
        public: false,
        pattern: "f".into(),
        value: Expr::Fn {
            params: vec!["x".to_string()],
            body: Box::new(Expr::Call {
                f: Box::new(Expr::Ident(Ident(None, "f".to_string()))),
                args: vec![],
            }),
        },
    };

    let result = new_compiler().compile_program(vec![ast], "main");
    assert!(result.is_err());
}

#[test]
fn tailcall_test() {
    // let f = fn x, y { f(x + 1, y + x) }

    let ast = Statement::Let {
        public: false,
        pattern: "f".into(),
        value: Expr::Fn {
            params: vec!["x".to_string(), "y".to_string()],
            body: Box::new(Expr::Call {
                f: Box::new(Expr::Ident(Ident(None, "f".to_string()))),
                args: vec![
                    Expr::Infix("+".to_string(), Box::new(ident("x")), Box::new(1.0.into())),
                    Expr::Infix("+".to_string(), Box::new(ident("x")), Box::new(ident("y"))),
                ],
            }),
        },
    };

    let main = new_compiler().compile_program(vec![ast], "main").unwrap();
    let f = &main.constant_pool[0].as_fn().unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            // x
            OpCode::GetLocal as u8,
            0,
            OpCode::Const as u8,
            0,
            OpCode::Add as u8,
            // y
            OpCode::GetLocal as u8,
            0,
            OpCode::GetLocal as u8,
            1,
            OpCode::Add as u8,
            // recur
            OpCode::SetLocal as u8,
            1,
            OpCode::SetLocal as u8,
            0,
            OpCode::Jump as u8,
            0,
            0,
            OpCode::Return as u8
        ]
    );
}

#[test]
fn let1_does_not_leak_test() {
    // { let x = true; true }
    // x
    let ast = Expr::Do(
        Box::new(
            Let {
                name: "x".to_string(),
                value: Box::new(true.into()),
                body: Box::new(true.into()),
            }
            .as_match(),
        ),
        Box::new(ident("x")),
    );

    let result = new_compiler().compile_expr(ast);
    assert!(result.is_err(), "{:?} should be Err(_)", result)
}

#[test]
fn let_only_allocates_needed() {
    // { let x = true; nil }
    // { let y = true; nil }
    let ast = Expr::Do(
        Box::new(
            Let {
                name: "x".to_string(),
                value: Box::new(true.into()),
                body: Box::new(NIL),
            }
            .as_match(),
        ),
        Box::new(
            Let {
                name: "y".to_string(),
                value: Box::new(true.into()),
                body: Box::new(NIL),
            }
            .as_match(),
        ),
    );

    let main = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        main.bytecode,
        vec![
            // x
            OpCode::ConstTrue as u8,
            OpCode::SetLocal as u8,
            0,
            OpCode::ConstNil as u8,
            OpCode::Pop as u8,
            // y
            OpCode::ConstTrue as u8,
            OpCode::SetLocal as u8,
            0,
            OpCode::ConstNil as u8,
            OpCode::Return as u8
        ]
    );
}

#[test]
fn modules_import_test() {
    /*
    A.ds
    ```
    nil
    ```

    Main.ds
    ```
    import A
    ```
     */
    let a_ns = Namespace(vec!["A".to_string()]);

    let mut compiler = new_compiler();
    compiler.add_module(a_ns.clone(), vec![Statement::Expr(NIL)]);

    let program = vec![Statement::Import(Import {
        ns: a_ns,
        rename: None,
    })];

    let main = compiler.compile_program(program, "main").unwrap();

    assert_eq!(
        main.bytecode,
        vec![OpCode::ConstNil as u8, OpCode::Return as u8]
    );
}

#[test]
fn modules_import_twice_test() {
    /*
    A.ds
    ```
    true
    ```

    Main.ds
    ```
    import A
    import A
    ```
     */
    let a_ns = Namespace(vec!["A".to_string()]);

    let mut compiler = new_compiler();
    compiler.add_module(a_ns.clone(), vec![Statement::Expr(true.into())]);

    let program = vec![
        Statement::Import(Import {
            ns: a_ns.clone(),
            rename: None,
        }),
        Statement::Import(Import {
            ns: a_ns,
            rename: None,
        }),
    ];

    let main = compiler.compile_program(program, "main").unwrap();

    assert_eq!(
        main.bytecode,
        vec![
            OpCode::ConstTrue as u8,
            OpCode::Pop as u8,
            OpCode::ConstNil as u8,
            OpCode::Return as u8
        ]
    );
}

#[test]
fn modules_import_value() {
    /*
    A.ds
    ```
    pub let x = true
    ```

    Main.ds
    ```
    import A
    A.x
    ```
     */
    let a_ns = Namespace(vec!["A".to_string()]);

    let mut compiler = new_compiler();
    compiler.add_module(
        a_ns.clone(),
        vec![Statement::Let {
            public: true,
            pattern: "x".into(),
            value: true.into(),
        }],
    );

    let program = vec![
        Statement::Import(Import {
            ns: a_ns.clone(),
            rename: None,
        }),
        Statement::Expr(Expr::Ident(Ident(Some(a_ns), "x".to_string()))),
    ];

    let main = compiler.compile_program(program, "main").unwrap();

    assert_eq!(
        main.bytecode,
        vec![
            OpCode::ConstTrue as u8,
            OpCode::SetGlobal as u8,
            0,
            0,
            OpCode::ConstNil as u8,
            OpCode::Pop as u8,
            OpCode::GetGlobal as u8,
            0,
            0,
            OpCode::Return as u8
        ]
    );
}

#[test]
fn module_not_imported_err() {
    /*
    A.ds
    ```
    pub let x = true
    ```

    Main.ds
    ```
    A.x // should be err
    ```
     */
    let a_ns = Namespace(vec!["A".to_string()]);

    let mut compiler = new_compiler();
    compiler.add_module(
        a_ns.clone(),
        vec![Statement::Let {
            public: true,
            pattern: "x".into(),
            value: true.into(),
        }],
    );

    let program = vec![Statement::Expr(Expr::Ident(Ident(
        Some(a_ns),
        "x".to_string(),
    )))];

    let result = compiler.compile_program(program, "main");
    assert!(matches!(result, Err(_)));
}

#[test]
fn modules_imports_are_scoped() {
    /*
    A.ds
    ```
    import B;
    ```

    B.ds
    ```
    pub let x = true
    ```

    Main.ds
    ```
    import A;
    B.x // should be err
    ```
     */
    let a_ns = Namespace(vec!["A".to_string()]);
    let b_ns = Namespace(vec!["B".to_string()]);

    let mut compiler = new_compiler();
    compiler.add_module(
        a_ns.clone(),
        vec![Statement::Import(Import::new(b_ns.clone()))],
    );

    compiler.add_module(
        b_ns.clone(),
        vec![Statement::Let {
            public: true,
            pattern: "x".into(),
            value: true.into(),
        }],
    );

    let program = vec![
        Statement::Import(Import::new(a_ns)),
        Statement::Expr(Expr::Ident(Ident(Some(b_ns), "x".to_string()))),
    ];

    assert!(matches!(compiler.compile_program(program, "main"), Err(_)));
}

#[test]
fn modules_renamed_imports() {
    /*
    A.ds
    ```
    pub let x = true
    ```

    Main.ds
    ```
    import A as B;
    B.x
    ```
     */
    let a_ns = Namespace(vec!["A".to_string()]);
    let b_ns = Namespace(vec!["B".to_string()]);

    let mut compiler = new_compiler();
    compiler.add_module(
        a_ns.clone(),
        vec![Statement::Let {
            public: true,
            pattern: "x".into(),
            value: true.into(),
        }],
    );

    let program = vec![
        Statement::Import(Import {
            ns: a_ns,
            rename: Some(b_ns.clone()),
        }),
        Statement::Expr(Expr::Ident(Ident(Some(b_ns), "x".to_string()))),
    ];

    let main = compiler.compile_program(program, "main").unwrap();

    assert_eq!(
        main.bytecode,
        vec![
            OpCode::ConstTrue as u8,
            OpCode::SetGlobal as u8,
            0,
            0,
            OpCode::ConstNil as u8,
            OpCode::Pop as u8,
            OpCode::GetGlobal as u8,
            0,
            0,
            OpCode::Return as u8
        ]
    );
}

#[test]
fn empty_match_test() {
    let ast = Expr::Match(Box::new(true.into()), vec![]);

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            OpCode::ConstTrue as u8,
            OpCode::SetLocal as u8,
            0,
            OpCode::PanicNoMatch as u8,
            OpCode::Return as u8,
        ]
    );
}

#[test]
fn ident_match_test() {
    let ast = Expr::Match(
        Box::new(true.into()),
        vec![
            //
            (Pattern::Identifier("x".to_string()), false.into()),
        ],
    );

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            /*  0 */ OpCode::ConstTrue as u8,
            /*  1 */ OpCode::SetLocal as u8,
            /*  2 */ 0,
            /*  3 */ OpCode::ConstFalse as u8,
            /*  4 */ OpCode::Return as u8,
        ]
    );
}

#[test]
fn empty_list_match_test() {
    let ast = Expr::Match(
        Box::new(true.into()),
        vec![(Pattern::EmptyList, false.into())],
    );

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(f.locals, 1, "locals");

    assert_eq!(
        f.bytecode,
        vec![
            /*  0 */ OpCode::ConstTrue as u8,
            /*  1 */ OpCode::SetLocal as u8,
            /*  2 */ 0,
            /*  3 */ OpCode::MatchEmptyListElseJump as u8,
            /*  4 */ 0,
            /*  5 */ 11,
            /*  6 */ 0,
            /*  7 */ OpCode::ConstFalse as u8,
            /*  8 */ OpCode::Jump as u8,
            /*  9 */ 0,
            /* 10 */ 12,
            /* 11 */ OpCode::PanicNoMatch as u8,
            /* 12 */ OpCode::Return as u8, // <-
        ]
    );
}

#[test]
fn empty_map_match_test() {
    let ast = Expr::Match(
        Box::new(true.into()),
        vec![(Pattern::EmptyMap, false.into())],
    );

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(f.locals, 1);

    assert_eq!(
        f.bytecode,
        vec![
            /*  0 */ OpCode::ConstTrue as u8,
            /*  1 */ OpCode::SetLocal as u8,
            /*  2 */ 0,
            /*  3 */ OpCode::MatchEmptyMapElseJump as u8,
            /*  4 */ 0,
            /*  5 */ 11,
            /*  6 */ 0,
            /*  7 */ OpCode::ConstFalse as u8,
            /*  8 */ OpCode::Jump as u8,
            /*  9 */ 0,
            /* 10 */ 12,
            /* 11 */ OpCode::PanicNoMatch as u8,
            /* 12 */ OpCode::Return as u8, // <-
        ]
    );
}

#[test]
fn const_match_test() {
    let ast = Expr::Match(
        Box::new(true.into()),
        vec![(Pattern::Lit(Lit::Num(42.0)), false.into())],
    );

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(f.constant_pool, vec![42.0.into()]);

    assert_eq!(f.locals, 1, "locals");

    assert_eq!(
        f.bytecode,
        vec![
            /*  0 */ OpCode::ConstTrue as u8,
            /*  1 */ OpCode::SetLocal as u8,
            /*  2 */ 0,
            /*  3 */ OpCode::MatchConstElseJump as u8,
            /*  4 */ 0,
            /*  5 */ 12,
            /*  6 */ 0,
            /*  7 */ 0,
            /*  8 */ OpCode::ConstFalse as u8,
            /*  9 */ OpCode::Jump as u8,
            /* 10 */ 0,
            /* 11 */ 13,
            /* 12 */ OpCode::PanicNoMatch as u8,
            /* 13 */ OpCode::Return as u8, // <-
        ]
    );
}

#[test]
fn const_match_test_many_clauses() {
    let ast = Expr::Match(
        Box::new(NIL),
        vec![
            (Pattern::Lit(Lit::Num(1.0)), false.into()),
            (Pattern::Lit(Lit::Num(2.0)), true.into()),
        ],
    );

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(f.constant_pool, vec![1.0.into(), 2.0.into()]);

    assert_eq!(
        f.bytecode,
        vec![
            /*  0 */ OpCode::ConstNil as u8,
            /*  1 */ OpCode::SetLocal as u8,
            /*  2 */ 0,
            /*  3 */ OpCode::MatchConstElseJump as u8,
            /*  4 */ 0,
            /*  5 */ 12,
            /*  6 */ 0,
            /*  7 */ 0,
            /*  8 */ OpCode::ConstFalse as u8,
            /*  9 */ OpCode::Jump as u8,
            /* 10 */ 0,
            /* 11 */ 22,
            /* 12 */ OpCode::MatchConstElseJump as u8,
            /* 13 */ 0,
            /* 14 */ 21,
            /* 15 */ 1,
            /* 16 */ 0,
            /* 17 */ OpCode::ConstTrue as u8,
            /* 18 */ OpCode::Jump as u8,
            /* 19 */ 0,
            /* 20 */ 22,
            /* 21 */ OpCode::PanicNoMatch as u8,
            /* 22 */ OpCode::Return as u8, // <-
        ],
        "{f}",
    );
}

#[test]
fn cons_match_test() {
    let ast = Expr::Match(
        Box::new(true.into()),
        vec![(
            Pattern::Cons(
                Box::new(Pattern::Identifier("hd".to_string())),
                Box::new(Pattern::Identifier("tl".to_string())),
            ),
            Expr::Ident(Ident(None, "tl".to_string())),
        )],
    );

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(f.locals, 3, "locals");

    assert_eq!(
        f.bytecode,
        vec![
            /*  0 */ OpCode::ConstTrue as u8,
            /*  1 */ OpCode::SetLocal as u8,
            /*  2 */ 0,
            /*  3 */ OpCode::MatchConsElseJump as u8,
            /*  4 */ 0,
            /*  5 */ 16,
            /*  6 */ 0,
            /*  7 */ OpCode::SetLocal as u8,
            /*  8 */ 1,
            /*  9 */ OpCode::SetLocal as u8,
            /* 10 */ 2,
            /* 11 */ OpCode::GetLocal as u8,
            /* 12 */ 2,
            /* 13 */ OpCode::Jump as u8,
            /* 14 */ 0,
            /* 15 */ 17,
            /* 16 */ OpCode::PanicNoMatch as u8,
            /* 17 */ OpCode::Return as u8, // <-
        ]
    );
}

#[test]
fn cons_map_match_test() {
    let ast = Expr::Match(
        Box::new(42.0.into()),
        vec![(
            Pattern::ConsMap(
                (
                    "key".to_string(),
                    Box::new(Pattern::Identifier("x".to_string())),
                ),
                Box::new(Pattern::Identifier("rest".to_string())),
            ),
            false.into(),
        )],
    );

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(f.constant_pool, vec![42.0.into(), "key".into()]);

    assert_eq!(f.locals, 3);

    assert_eq!(
        f.bytecode,
        vec![
            /*  0 */ OpCode::Const as u8,
            /*  1 */ 0,
            /*  2 */ OpCode::SetLocal as u8,
            /*  3 */ 0,
            /*  4 */ OpCode::MatchConsMapElseJump as u8,
            /*  5 */ 0,
            /*  6 */ 17,
            /*  7 */ 0,
            /*  8 */ 1,
            /*  9 */ OpCode::SetLocal as u8,
            /* 10 */ 1,
            /* 11 */ OpCode::SetLocal as u8,
            /* 12 */ 2,
            /* 13 */ OpCode::ConstFalse as u8,
            /* 14 */ OpCode::Jump as u8,
            /* 15 */ 0,
            /* 16 */ 18,
            /* 17 */ OpCode::PanicNoMatch as u8,
            /* 18 */ OpCode::Return as u8, // <-
        ]
    );
}

#[test]
fn tuple2_match_test() {
    let ast = Expr::Match(
        Box::new(true.into()),
        vec![(
            Pattern::Tuple(vec![
                Pattern::Identifier("x".to_string()),
                Pattern::Identifier("x".to_string()),
            ]),
            Expr::Ident(Ident(None, "x".to_string())),
        )],
    );

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(f.locals, 3, "locals");

    assert_eq!(
        f.bytecode,
        vec![
            /*  0 */ OpCode::ConstTrue as u8,
            /*  1 */ OpCode::SetLocal as u8,
            /*  2 */ 0,
            /*  3 */ OpCode::MatchTuple2ElseJump as u8,
            /*  4 */ 0,
            /*  5 */ 16,
            /*  6 */ 0,
            /*  7 */ OpCode::SetLocal as u8,
            /*  8 */ 1,
            /*  9 */ OpCode::SetLocal as u8,
            /* 10 */ 2,
            /* 11 */ OpCode::GetLocal as u8,
            /* 12 */ 2,
            /* 13 */ OpCode::Jump as u8,
            /* 14 */ 0,
            /* 15 */ 17,
            /* 16 */ OpCode::PanicNoMatch as u8,
            /* 17 */ OpCode::Return as u8, // <-
        ]
    );
}
