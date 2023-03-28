use std::rc::Rc;

use super::{
    ast::Ast,
    sexpr::SExpr,
    symbol_table::{Scope, SymbolTable},
};
use crate::vm::{
    bytecode::OpCode,
    value::{Function, Value},
};

#[derive(Default)]
pub struct Compiler {
    symbol_table: SymbolTable,
    // TODO keep track of macros
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
        }
    }

    /// Compile an s-expression into a zero-arity function containing it's chunk of bytecode.
    pub fn compile_sexpr(&mut self, sexpr: &SExpr) -> Result<Function, String> {
        let ast = sexpr.try_into()?;
        self.compile_ast(ast)
    }

    fn compile_ast_chunk(&mut self, f: &mut Function, ast: Ast) -> Result<(), String> {
        match ast {
            Ast::Const(Value::Nil) => f.bytecode.push(OpCode::ConstNil as u8),
            Ast::Const(Value::Bool(true)) => f.bytecode.push(OpCode::ConstTrue as u8),
            Ast::Const(Value::Bool(false)) => f.bytecode.push(OpCode::ConstFalse as u8),
            Ast::Const(value) => {
                alloc_const(f, value);
            }

            Ast::Symbol(name) => {
                let lookup = self.symbol_table.resolve(&name);
                match lookup {
                    Some(scope) => compile_symbol_lookup(f, scope),
                    None => return Err(format!("Lookup not found for {name}")),
                }
            }

            Ast::Call(caller, args) => {
                // TODO return err when args > 256
                let args_len = args.len();

                for arg in args {
                    self.compile_ast_chunk(f, arg)?;
                }
                self.compile_ast_chunk(f, *caller)?;
                f.bytecode.push(OpCode::Call as u8);
                f.bytecode.push(args_len as u8);
            }

            Ast::Do(asts) => match asts.len() {
                0 => f.bytecode.push(OpCode::ConstNil as u8),

                _ => {
                    let mut is_first = true;

                    for ast in asts {
                        if !is_first {
                            f.bytecode.push(OpCode::Pop as u8);
                        }

                        self.compile_ast_chunk(f, ast)?;
                        is_first = false;
                    }
                }
            },

            Ast::Def(binding, body) => {
                self.compile_ast_chunk(f, *body)?;
                f.bytecode.push(OpCode::SetGlobal as u8);

                let index = self.symbol_table.define_global(&binding);

                let (msb, lsb) = to_big_endian_u16(index);
                f.bytecode.push(msb);
                f.bytecode.push(lsb);
            }

            Ast::If(cond, x, y) => {
                self.compile_ast_chunk(f, *cond)?;
                let first_jump_index = set_jump_placeholder(f, OpCode::JumpIfNot);
                self.compile_ast_chunk(f, *x)?;
                let second_jump_index = set_jump_placeholder(f, OpCode::Jump);

                // TODO throw on overflow
                set_big_endian_u16(f, first_jump_index, f.bytecode.len() as u16);

                self.compile_ast_chunk(f, *y)?;

                // TODO throw on overflow
                set_big_endian_u16(f, second_jump_index, f.bytecode.len() as u16);
            }

            Ast::Let1(name, value, body) => {
                let binding_index = self.symbol_table.define_local(&name);

                self.compile_ast_chunk(f, *value)?;
                f.bytecode.push(OpCode::SetLocal as u8);
                f.bytecode.push(binding_index);
                self.compile_ast_chunk(f, *body)?;
                self.symbol_table.remove_local(&name);
            }

            Ast::Lambda(params, body) => {
                self.symbol_table.enter_scope();
                let mut inner_f = Function {
                    arity: (&params).into(),
                    ..Function::default()
                };

                for required in params.required {
                    self.symbol_table.define_local(&required);
                }

                for optional in params.optional {
                    self.symbol_table.define_local(&optional);
                }

                if let Some(rest_param) = params.rest {
                    self.symbol_table.define_local(&rest_param);
                }

                self.compile_ast_chunk(&mut inner_f, *body)?;
                inner_f.bytecode.push(OpCode::Return as u8);
                inner_f.locals = self.symbol_table.count_locals() - count_arity_bindings(&inner_f);
                let free_vars = &self.symbol_table.free();
                self.symbol_table.exit_scope();

                if free_vars.is_empty() {
                    alloc_const(f, Value::Function(Rc::new(inner_f)));
                } else {
                    for scope in free_vars {
                        compile_symbol_lookup(f, *scope);
                    }

                    alloc_const(f, Value::Function(Rc::new(inner_f)));
                    f.bytecode.push(OpCode::MakeClosure as u8);
                    f.bytecode.push(free_vars.len() as u8);
                }
            }
        };

        Ok(())
    }

    /// Compile an AST expression into a zero-arity function containing it's chunk of bytecode.
    fn compile_ast(&mut self, ast: Ast) -> Result<Function, String> {
        let mut f = Function::default();
        self.compile_ast_chunk(&mut f, ast)?;
        f.bytecode.push(OpCode::Return as u8);
        f.locals = self.symbol_table.count_locals();
        Ok(f)
    }
}

fn compile_symbol_lookup(f: &mut Function, scope: Scope) {
    match scope {
        Scope::Global(index) => {
            f.bytecode.push(OpCode::GetGlobal as u8);
            push_big_endian_u16(f, index);
        }

        Scope::Local(index) => {
            f.bytecode.push(OpCode::GetLocal as u8);
            f.bytecode.push(index);
        }

        Scope::Free(index) => {
            f.bytecode.push(OpCode::GetFree as u8);
            f.bytecode.push(index);
        }
    }
}

fn count_arity_bindings(f: &Function) -> u8 {
    f.arity.required + f.arity.optional + f.arity.rest as u8
}

fn alloc_const(f: &mut Function, value: Value) {
    let current_index = f.constant_pool.len();
    f.constant_pool.push(value);

    f.bytecode.push(OpCode::Const as u8);
    f.bytecode.push(current_index as u8);
}

fn set_jump_placeholder(f: &mut Function, opcode: OpCode) -> usize {
    f.bytecode.push(opcode as u8);
    let jump_index = f.bytecode.len();
    f.bytecode.push(0);
    f.bytecode.push(0);
    jump_index
}

pub fn to_big_endian_u16(n: u16) -> (u8, u8) {
    let msb = n >> 8;
    (msb as u8, n as u8)
}

pub fn push_big_endian_u16(f: &mut Function, value: u16) {
    let (msb, lsb) = to_big_endian_u16(value);
    f.bytecode.push(msb);
    f.bytecode.push(lsb);
}

pub fn set_big_endian_u16(f: &mut Function, index: usize, value: u16) {
    let (msb, lsb) = to_big_endian_u16(value);

    f.bytecode[index] = msb;
    f.bytecode[index + 1] = lsb;
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{
        compiler::{
            ast::{Ast, Params},
            compiler::{to_big_endian_u16, Compiler},
        },
        vm::{
            bytecode::OpCode,
            value::{Function, FunctionArity, Value},
        },
    };

    #[test]
    fn const_true_test() {
        let ast = Ast::Const(Value::Bool(true));
        let f = Compiler::new().compile_ast(ast).unwrap();

        assert_eq!(f.arity.required, 0);
        assert_eq!(
            f.bytecode,
            vec![OpCode::ConstTrue as u8, OpCode::Return as u8]
        );
    }

    #[test]
    fn const_false_test() {
        let ast = Ast::Const(Value::Bool(false));
        let f = Compiler::new().compile_ast(ast).unwrap();

        assert_eq!(
            f.bytecode,
            vec![OpCode::ConstFalse as u8, OpCode::Return as u8]
        );
    }

    #[test]
    fn nil_const_test() {
        let ast = Ast::Const(Value::Nil);
        let f = Compiler::new().compile_ast(ast).unwrap();

        assert_eq!(
            f.bytecode,
            vec![OpCode::ConstNil as u8, OpCode::Return as u8]
        );
    }

    #[test]
    fn int_const_test() {
        let ast = Ast::Const(Value::Int(42));
        let f = Compiler::new().compile_ast(ast).unwrap();

        assert_eq!(f.constant_pool[0], Value::Int(42));

        assert_eq!(
            f.bytecode,
            vec![OpCode::Const as u8, 0, OpCode::Return as u8]
        );
    }

    #[test]
    fn string_const_test() {
        let ast = Ast::Const(Value::String(Rc::new("abc".to_string())));
        let f = Compiler::new().compile_ast(ast).unwrap();

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
    fn symbol_const_test() {
        let ast = Ast::Const(Value::Symbol(Rc::new("abc".to_string())));
        let f = Compiler::new().compile_ast(ast).unwrap();

        assert_eq!(
            f.constant_pool[0],
            Value::Symbol(Rc::new("abc".to_string()))
        );

        assert_eq!(
            f.bytecode,
            vec![OpCode::Const as u8, 0, OpCode::Return as u8]
        );
    }

    #[test]
    fn empty_do_test() {
        let ast = Ast::Do(vec![]);
        let f = Compiler::new().compile_ast(ast).unwrap();

        assert_eq!(
            f.bytecode,
            vec![OpCode::ConstNil as u8, OpCode::Return as u8,]
        );
    }

    #[test]
    fn multiple_exprs_do_test() {
        let ast = Ast::Do(vec![Ast::Const(Value::Nil), Ast::Const(Value::Bool(true))]);
        let f = Compiler::new().compile_ast(ast).unwrap();

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
    fn single_expr_do_test() {
        let ast = Ast::Do(vec![Ast::Const(Value::Nil)]);
        let f = Compiler::new().compile_ast(ast).unwrap();

        assert_eq!(
            f.bytecode,
            vec![OpCode::ConstNil as u8, OpCode::Return as u8]
        );
    }

    #[test]
    fn def_test() {
        let ast = Ast::Def("x".to_string(), Box::new(Ast::Const(Value::Bool(true))));

        let f = Compiler::new().compile_ast(ast).unwrap();

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
        let ast = Ast::Do(vec![
            Ast::Def("x".to_string(), Box::new(Ast::Const(Value::Bool(true)))),
            Ast::Def("y".to_string(), Box::new(Ast::Const(Value::Bool(false)))),
        ]);

        let f = Compiler::new().compile_ast(ast).unwrap();

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
        let ast = Ast::Do(vec![
            Ast::Def("x".to_string(), Box::new(Ast::Const(Value::Bool(true)))),
            Ast::Symbol("x".to_string()),
        ]);

        let f = Compiler::new().compile_ast(ast).unwrap();

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
    fn if_expr_test() {
        let ast = Ast::If(
            Box::new(Ast::Const(Value::Bool(true))),
            Box::new(Ast::Const(Value::Symbol(Rc::new("b1".to_string())))),
            Box::new(Ast::Const(Value::Symbol(Rc::new("b2".to_string())))),
        );

        let f = Compiler::new().compile_ast(ast).unwrap();

        assert_eq!(
            f.constant_pool,
            vec![
                Value::Symbol(Rc::new("b1".to_string())),
                Value::Symbol(Rc::new("b2".to_string())),
            ]
        );

        assert_eq!(
            f.bytecode,
            vec![
                /*  0 */ OpCode::ConstTrue as u8,
                /*  1 */ OpCode::JumpIfNot as u8,
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
        let ast = Ast::Lambda(Params::default(), Box::new(Ast::Const(Value::Int(42))));
        let f = Compiler::new().compile_ast(ast).unwrap();

        let compiled_lambda = Function {
            bytecode: vec![OpCode::Const as u8, 0, OpCode::Return as u8],
            constant_pool: vec![Value::Int(42)],
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
        let params = Params {
            required: vec!["x".to_string(), "y".to_string()],
            ..Params::default()
        };

        let ast = Ast::Lambda(params, Box::new(Ast::Const(Value::Nil)));
        let f = Compiler::new().compile_ast(ast).unwrap();

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
    fn lambda_expr_optional_args_test() {
        // (lambda* (&opt x) ())
        let params = Params {
            optional: vec!["x".to_string()],
            ..Params::default()
        };

        let ast = Ast::Lambda(params, Box::new(Ast::Const(Value::Nil)));
        let f = Compiler::new().compile_ast(ast).unwrap();

        let compiled_lambda = Function {
            bytecode: vec![OpCode::ConstNil as u8, OpCode::Return as u8],
            arity: FunctionArity {
                optional: 1,
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
        let params = Params {
            required: vec!["x".to_string(), "y".to_string()],
            ..Params::default()
        };

        let ast = Ast::Lambda(params, Box::new(Ast::Symbol("y".to_string())));
        let f = Compiler::new().compile_ast(ast).unwrap();

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
    fn lambda_args_optional_lookup_test() {
        // (lambda* (&opt x) x)
        let params = Params {
            optional: vec!["x".to_string()],
            ..Params::default()
        };

        let ast = Ast::Lambda(params, Box::new(Ast::Symbol("x".to_string())));
        let f = Compiler::new().compile_ast(ast).unwrap();

        let compiled_lambda = Function {
            bytecode: vec![OpCode::GetLocal as u8, 0, OpCode::Return as u8],
            arity: FunctionArity {
                optional: 1,
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
    fn lambda_expr_rest_args_test() {
        // (lambda* (&rest x) nil)
        let params = Params {
            rest: Some("x".to_string()),
            ..Params::default()
        };

        let ast = Ast::Lambda(params, Box::new(Ast::Const(Value::Nil)));
        let f = Compiler::new().compile_ast(ast).unwrap();

        let compiled_lambda = Function {
            bytecode: vec![OpCode::ConstNil as u8, OpCode::Return as u8],
            arity: FunctionArity {
                rest: true,
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
        let compiled_lambda = Function {
            bytecode: vec![OpCode::Const as u8, 0, OpCode::Return as u8],
            constant_pool: vec![Value::Int(42)],
            ..Function::default()
        };

        let ast = Ast::Call(
            Box::new(Ast::Const(Value::Function(Rc::new(compiled_lambda)))),
            vec![],
        );

        let f = Compiler::new().compile_ast(ast).unwrap();

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
        let compiled_lambda = Function {
            bytecode: vec![OpCode::ConstNil as u8, OpCode::Return as u8],
            ..Function::default()
        };

        let ast = Ast::Call(
            Box::new(Ast::Const(Value::Function(Rc::new(compiled_lambda)))),
            vec![Ast::Const(Value::Bool(true))],
        );

        let f = Compiler::new().compile_ast(ast).unwrap();

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

        let ast = Ast::Let1(
            "x".to_string(),
            Box::new(Ast::Const(Value::Bool(true))),
            Box::new(Ast::Const(Value::Nil)),
        );

        let f = Compiler::new().compile_ast(ast).unwrap();

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

        let ast = Ast::Let1(
            "x".to_string(),
            Box::new(Ast::Const(Value::Bool(true))),
            Box::new(Ast::Let1(
                "y".to_string(),
                Box::new(Ast::Const(Value::Bool(false))),
                Box::new(Ast::Const(Value::Nil)),
            )),
        );

        let f = Compiler::new().compile_ast(ast).unwrap();

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

        let ast = Ast::Let1(
            "x".to_string(),
            Box::new(Ast::Const(Value::Bool(true))),
            Box::new(Ast::Symbol("x".to_string())),
        );

        let f = Compiler::new().compile_ast(ast).unwrap();

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

        let ast = Ast::Let1(
            "x".to_string(),
            Box::new(Ast::Const(Value::Bool(true))),
            Box::new(Ast::Lambda(
                Params::default(),
                Box::new(Ast::Symbol("x".to_string())),
            )),
        );

        let main = Compiler::new().compile_ast(ast).unwrap();

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
        let ast = Ast::Do(vec![
            Ast::Let1(
                "x".to_string(),
                Box::new(Ast::Const(Value::Bool(true))),
                Box::new(Ast::Const(Value::Bool(true))),
            ),
            Ast::Symbol("x".to_string()),
        ]);

        let result = Compiler::new().compile_ast(ast);
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
