use super::symbol_table::{Scope, SymbolTable};
use crate::{
    ast::{Expr, Lit, Program, Statement},
    vm::{
        bytecode::OpCode,
        value::{Function, FunctionArity, Value},
    },
};
use std::rc::Rc;

fn prefix_to_opcode(op: &str) -> Option<OpCode> {
    match op {
        "!" => Some(OpCode::Not),
        _ => None,
    }
}

fn infix_to_opcode(op: &str) -> Option<OpCode> {
    match op {
        "==" => Some(OpCode::Eq),
        "+" => Some(OpCode::Add),
        ">" => Some(OpCode::GreaterThan),
        _ => None,
    }
}

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

    fn compile_expr_chunk(&mut self, f: &mut Function, ast: Expr) -> Result<(), String> {
        match ast {
            Expr::Lit(Lit::Nil) => f.bytecode.push(OpCode::ConstNil as u8),
            Expr::Lit(Lit::Bool(true)) => f.bytecode.push(OpCode::ConstTrue as u8),
            Expr::Lit(Lit::Bool(false)) => f.bytecode.push(OpCode::ConstFalse as u8),
            Expr::Lit(Lit::String(s)) => alloc_const(f, Value::String(Rc::new(s))),
            // TODO repo int as f64
            Expr::Lit(Lit::Num(n)) => alloc_const(f, Value::Int(n as i64)),

            Expr::Ident(name) => {
                let lookup = self.symbol_table.resolve(&name);
                match lookup {
                    Some(scope) => compile_symbol_lookup(f, scope),
                    None => return Err(format!("Lookup not found for {name}")),
                }
            }

            Expr::If {
                condition,
                if_branch,
                else_branch,
            } => {
                self.compile_expr_chunk(f, *condition)?;
                let first_jump_index = set_jump_placeholder(f, OpCode::JumpIfNot);
                self.compile_expr_chunk(f, *if_branch)?;
                let second_jump_index = set_jump_placeholder(f, OpCode::Jump);

                // TODO throw on overflow
                set_big_endian_u16(f, first_jump_index, f.bytecode.len() as u16);

                self.compile_expr_chunk(f, *else_branch)?;

                // TODO throw on overflow
                set_big_endian_u16(f, second_jump_index, f.bytecode.len() as u16);
            }

            Expr::Fn { params, body } => {
                self.symbol_table.enter_scope();
                let mut inner_f = Function {
                    // TODO remove fn_arity
                    arity: FunctionArity {
                        required: params.len() as u8,
                        ..Default::default()
                    },
                    ..Function::default()
                };

                for required in params {
                    self.symbol_table.define_local(&required);
                }

                self.compile_expr_chunk(&mut inner_f, *body)?;
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

            Expr::Call { f: caller, args } => {
                // TODO return err when args > 256
                let args_len = args.len();

                for arg in args {
                    self.compile_expr_chunk(f, arg)?;
                }
                self.compile_expr_chunk(f, *caller)?;
                f.bytecode.push(OpCode::Call as u8);
                f.bytecode.push(args_len as u8);
            }

            Expr::Let { name, value, body } => {
                let binding_index = self.symbol_table.define_local(&name);

                self.compile_expr_chunk(f, *value)?;
                f.bytecode.push(OpCode::SetLocal as u8);
                f.bytecode.push(binding_index);
                self.compile_expr_chunk(f, *body)?;
                self.symbol_table.remove_local(&name);
            }

            Expr::Do(left, right) => {
                self.compile_expr_chunk(f, *left)?;
                f.bytecode.push(OpCode::Pop as u8);
                self.compile_expr_chunk(f, *right)?;
            }

            Expr::Prefix(op, value) => match prefix_to_opcode(&op) {
                Some(opcode) => {
                    self.compile_expr_chunk(f, *value)?;
                    f.bytecode.push(opcode as u8);
                }
                None => return Err(format!("Invalid prefix op: {op}")),
            },

            Expr::Infix(op, left, right) => match infix_to_opcode(&op) {
                Some(opcode) => {
                    self.compile_expr_chunk(f, *right)?;
                    self.compile_expr_chunk(f, *left)?;
                    f.bytecode.push(opcode as u8);
                }
                None => return Err(format!("Invalid infix op: {op}")),
            },
        };

        Ok(())
    }

    fn compile_statement_chunk(
        &mut self,
        f: &mut Function,
        statement: Statement,
    ) -> Result<(), String> {
        match statement {
            Statement::Let { name, value } => {
                self.compile_expr_chunk(f, value)?;
                f.bytecode.push(OpCode::SetGlobal as u8);

                let index = self.symbol_table.define_global(&name);

                let (msb, lsb) = to_big_endian_u16(index);
                f.bytecode.push(msb);
                f.bytecode.push(lsb);
                Ok(())
            }
            Statement::Expr(expr) => self.compile_expr_chunk(f, expr),
        }
    }

    /// Compile an AST expression into a zero-arity function containing it's chunk of bytecode.
    pub fn compile_program(&mut self, program: Program) -> Result<Function, String> {
        let mut f = Function::default();
        for (i, statement) in program.into_iter().enumerate() {
            if i != 0 {
                f.bytecode.push(OpCode::Pop as u8)
            }

            self.compile_statement_chunk(&mut f, statement)?;
        }
        f.bytecode.push(OpCode::Return as u8);
        f.locals = self.symbol_table.count_locals();
        Ok(f)
    }

    pub fn compile_expr(&mut self, expr: Expr) -> Result<Function, String> {
        self.compile_program(vec![Statement::Expr(expr)])
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

        assert_eq!(f.constant_pool[0], Value::Int(42));

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
    fn if_expr_test() {
        let ast = Expr::If {
            condition: Box::new(true.into()),
            if_branch: Box::new(0.0.into()),
            else_branch: Box::new(1.0.into()),
        };

        let f = Compiler::new().compile_expr(ast).unwrap();

        assert_eq!(f.constant_pool, vec![Value::Int(0), Value::Int(1),]);

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
        let ast = Expr::Fn {
            params: vec![],
            body: Box::new(42.0.into()),
        };

        let f = Compiler::new().compile_expr(ast).unwrap();

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
