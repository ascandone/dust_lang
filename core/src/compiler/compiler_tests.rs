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
        name: "x".to_string(),
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
            OpCode::Return as u8
        ]
    );
}

#[test]
fn def_twice_test() {
    let ast = vec![
        Statement::Let {
            name: "x".to_string(),
            value: true.into(),
            public: false,
        },
        Statement::Let {
            name: "y".to_string(),
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
            OpCode::Pop as u8,
            OpCode::ConstFalse as u8,
            OpCode::SetGlobal as u8,
            0,
            1,
            OpCode::Return as u8
        ]
    );
}

#[test]
fn global_scope_test() {
    let ast = vec![
        Statement::Let {
            public: false,
            name: "x".to_string(),
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
            OpCode::Pop as u8,
            OpCode::GetGlobal as u8,
            0,
            0,
            OpCode::Return as u8
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
    let ast = Expr::Let {
        name: "f".to_string(),
        value: Box::new(Expr::Fn {
            params: vec![],
            body: Box::new(NIL),
        }),
        body: Box::new(NIL),
    };

    let main = new_compiler().compile_expr(ast).unwrap();

    let f = &main.constant_pool[0].as_fn().unwrap();

    assert_eq!(f.name, Some("f".to_string()));
}

#[test]
fn infer_lambda_name_from_let_statement() {
    // let f = fn {nil}
    let ast = Statement::Let {
        public: false,
        name: "f".to_string(),
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

    let ast = Expr::Let {
        name: "x".to_string(),
        value: Box::new(true.into()),
        body: Box::new(NIL),
    };

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
    // (let1 (x #true)
    //   (let (y #false)
    //      nil))

    let ast = Expr::Let {
        name: "x".to_string(),
        value: Box::new(true.into()),
        body: Box::new(Expr::Let {
            name: "y".to_string(),
            value: Box::new(false.into()),
            body: Box::new(NIL),
        }),
    };

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

    let ast = Expr::Let {
        name: "x".to_string(),
        value: Box::new(true.into()),
        body: Box::new(ident("x")),
    };

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
    // (let1 (x #true) (lambda* () x))

    let ast = Expr::Let {
        name: "x".to_string(),
        value: Box::new(true.into()),
        body: Box::new(Expr::Fn {
            params: vec![],
            body: Box::new(ident("x")),
        }),
    };

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
        name: "f".to_string(),
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
        name: "f".to_string(),
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
        name: "f".to_string(),
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
    // (do (let1 (x #true) #true) x)
    let ast = Expr::Do(
        Box::new(Expr::Let {
            name: "x".to_string(),
            value: Box::new(true.into()),
            body: Box::new(true.into()),
        }),
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
        Box::new(Expr::Let {
            name: "x".to_string(),
            value: Box::new(true.into()),
            body: Box::new(NIL),
        }),
        Box::new(Expr::Let {
            name: "y".to_string(),
            value: Box::new(true.into()),
            body: Box::new(NIL),
        }),
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
            name: "x".to_string(),
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
            name: "x".to_string(),
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
            name: "x".to_string(),
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
            name: "x".to_string(),
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
            /*  4 */ OpCode::Jump as u8,
            /*  5 */ 0,
            /*  6 */ 8,
            /*  7 */ OpCode::PanicNoMatch as u8,
            /*  8 */ OpCode::Return as u8, // <-
        ]
    );
}

#[test]
fn nil_match_test() {
    let ast = Expr::Match(
        Box::new(true.into()),
        vec![(Pattern::EmptyList, false.into())],
    );

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            /*  0 */ OpCode::ConstTrue as u8,
            /*  1 */ OpCode::MatchEmptyListElseJump as u8,
            /*  2 */ 0,
            /*  3 */ 8,
            /*  4 */ OpCode::ConstFalse as u8,
            /*  5 */ OpCode::Jump as u8,
            /*  6 */ 0,
            /*  7 */ 9,
            /*  8 */ OpCode::PanicNoMatch as u8,
            /*  9 */ OpCode::Return as u8, // <-
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

    assert_eq!(
        f.bytecode,
        vec![
            /*  0 */ OpCode::ConstTrue as u8,
            /*  1 */ OpCode::MatchConstElseJump as u8,
            /*  2 */ 0,
            /*  3 */ 9,
            /*  4 */ 0,
            /*  5 */ OpCode::ConstFalse as u8,
            /*  6 */ OpCode::Jump as u8,
            /*  7 */ 0,
            /*  8 */ 10,
            /*  9 */ OpCode::PanicNoMatch as u8,
            /* 10 */ OpCode::Return as u8, // <-
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
            /*  1 */ OpCode::MatchConstElseJump as u8,
            /*  2 */ 0,
            /*  3 */ 9,
            /*  4 */ 0,
            /*  5 */ OpCode::ConstFalse as u8,
            /*  6 */ OpCode::Jump as u8,
            /*  7 */ 0,
            /*  8 */ 18,
            /*  9 */ OpCode::MatchConstElseJump as u8,
            /* 10 */ 0,
            /* 11 */ 17,
            /* 12 */ 1,
            /* 13 */ OpCode::ConstTrue as u8,
            /* 14 */ OpCode::Jump as u8,
            /* 15 */ 0,
            /* 16 */ 18,
            /* 17 */ OpCode::PanicNoMatch as u8,
            /* 18 */ OpCode::Return as u8, // <-
        ]
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
            false.into(),
        )],
    );

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            /*  0 */ OpCode::ConstTrue as u8,
            /*  1 */ OpCode::MatchConsElseJump as u8,
            /*  2 */ 0,
            /*  3 */ 12,
            /*  4 */ OpCode::SetLocal as u8,
            /*  5 */ 0,
            /*  6 */ OpCode::SetLocal as u8,
            /*  7 */ 1,
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
fn tuple2_match_test() {
    let ast = Expr::Match(
        Box::new(true.into()),
        vec![(
            Pattern::Tuple(vec![
                Pattern::Identifier("x".to_string()),
                Pattern::Identifier("x".to_string()),
            ]),
            false.into(),
        )],
    );

    let f = new_compiler().compile_expr(ast).unwrap();

    assert_eq!(
        f.bytecode,
        vec![
            /*  0 */ OpCode::ConstTrue as u8,
            /*  1 */ OpCode::MatchTuple2ElseJump as u8,
            /*  2 */ 0,
            /*  3 */ 12,
            /*  4 */ OpCode::SetLocal as u8,
            /*  5 */ 0,
            /*  6 */ OpCode::SetLocal as u8,
            /*  7 */ 1,
            /*  8 */ OpCode::ConstFalse as u8,
            /*  9 */ OpCode::Jump as u8,
            /* 10 */ 0,
            /* 11 */ 13,
            /* 12 */ OpCode::PanicNoMatch as u8,
            /* 13 */ OpCode::Return as u8, // <-
        ]
    );
}
