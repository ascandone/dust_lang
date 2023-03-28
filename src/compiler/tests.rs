#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{
        ast::{Expr, Statement, NIL},
        compiler::compiler::{to_big_endian_u16, Compiler},
        vm::{
            bytecode::OpCode,
            value::{Function, FunctionArity, Value},
        },
    };

    #[test]
    fn const_true_test() {
        let ast = true.into();
        let f = Compiler::new().compile_expr(ast).unwrap();

        assert_eq!(f.arity.required, 0);
        assert_eq!(
            f.bytecode,
            vec![OpCode::ConstTrue as u8, OpCode::Return as u8]
        );
    }

    #[test]
    fn const_false_test() {
        let ast = false.into();
        let f = Compiler::new().compile_expr(ast).unwrap();

        assert_eq!(
            f.bytecode,
            vec![OpCode::ConstFalse as u8, OpCode::Return as u8]
        );
    }

    #[test]
    fn nil_const_test() {
        let ast = NIL;
        let f = Compiler::new().compile_expr(ast).unwrap();

        assert_eq!(
            f.bytecode,
            vec![OpCode::ConstNil as u8, OpCode::Return as u8]
        );
    }

    #[test]
    fn int_const_test() {
        let ast = 42.0.into();
        let f = Compiler::new().compile_expr(ast).unwrap();

        assert_eq!(f.constant_pool[0], Value::Num(42.0));

        assert_eq!(
            f.bytecode,
            vec![OpCode::Const as u8, 0, OpCode::Return as u8]
        );
    }

    #[test]
    fn string_const_test() {
        let ast = "abc".into();
        let f = Compiler::new().compile_expr(ast).unwrap();

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
        let f = Compiler::new().compile_expr(ast).unwrap();

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
        }];

        let f = Compiler::new().compile_program(ast).unwrap();

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
            },
            Statement::Let {
                name: "y".to_string(),
                value: false.into(),
            },
        ];

        let f = Compiler::new().compile_program(ast).unwrap();

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
                name: "x".to_string(),
                value: true.into(),
            },
            Statement::Expr(Expr::Ident("x".to_string())),
        ];

        let f = Compiler::new().compile_program(ast).unwrap();

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

        let f = Compiler::new().compile_expr(ast).unwrap();

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

        let f = Compiler::new().compile_expr(ast).unwrap();

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

        let f = Compiler::new().compile_expr(ast).unwrap();

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

        let f = Compiler::new().compile_expr(ast).unwrap();

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
    fn lambda_expr_required_args_test() {
        // (lambda* (x y) 42)
        let ast = Expr::Fn {
            params: vec!["x".to_string(), "y".to_string()],
            body: Box::new(NIL),
        };

        let f = Compiler::new().compile_expr(ast).unwrap();

        let compiled_lambda = Function {
            bytecode: vec![OpCode::ConstNil as u8, OpCode::Return as u8],
            arity: FunctionArity {
                required: 2,
                ..FunctionArity::default()
            },
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
            body: Box::new(Expr::Ident("y".to_string())),
        };

        let f = Compiler::new().compile_expr(ast).unwrap();

        let compiled_lambda = Function {
            bytecode: vec![OpCode::GetLocal as u8, 1, OpCode::Return as u8],
            arity: FunctionArity {
                required: 2,
                ..FunctionArity::default()
            },
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

        let f = Compiler::new().compile_expr(ast).unwrap();

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

        let f = Compiler::new().compile_expr(ast).unwrap();

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

        let f = Compiler::new().compile_expr(ast).unwrap();

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

        let f = Compiler::new().compile_expr(ast).unwrap();

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
            body: Box::new(Expr::Ident("x".to_string())),
        };

        let f = Compiler::new().compile_expr(ast).unwrap();

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
    fn make_closure_test() {
        // (let1 (x #true) (lambda* () x))

        let ast = Expr::Let {
            name: "x".to_string(),
            value: Box::new(true.into()),
            body: Box::new(Expr::Fn {
                params: vec![],
                body: Box::new(Expr::Ident("x".to_string())),
            }),
        };

        let main = Compiler::new().compile_expr(ast).unwrap();

        assert_eq!(
            main.bytecode,
            vec![
                OpCode::ConstTrue as u8,
                OpCode::SetLocal as u8,
                0,
                // lambda:
                OpCode::GetLocal as u8,
                0,
                OpCode::Const as u8,
                0,
                OpCode::MakeClosure as u8,
                1,
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
    fn let1_does_not_leak_test() {
        // (do (let1 (x #true) #true) x)
        let ast = Expr::Do(
            Box::new(Expr::Let {
                name: "x".to_string(),
                value: Box::new(true.into()),
                body: Box::new(true.into()),
            }),
            Box::new(Expr::Ident("x".to_string())),
        );

        let result = Compiler::new().compile_expr(ast);
        assert!(result.is_err(), "{:?} should be Err(_)", result)
    }

    #[test]
    fn endianess() {
        for n in &[0, 1, 22, 255, 256, 300, 600, 1400, u16::pow(2, 8) - 1] {
            let (msb, lsb) = to_big_endian_u16(*n);
            assert_eq!(
                ((msb as u16) << 8) + lsb as u16,
                *n,
                "big_endianess_invariant for {n}"
            );
        }
    }
}
