use super::symbol_table::{Scope, SymbolTable};
use crate::ast::ModuleName;
use crate::{
    ast::{Expr, Lit, Program, Statement},
    vm::{
        bytecode::OpCode,
        value::{Function, Value},
    },
};
use std::ops::Deref;
use std::rc::Rc;

fn prefix_to_opcode(op: &str) -> Option<OpCode> {
    match op {
        "!" => Some(OpCode::Not),
        "-" => Some(OpCode::Negate),
        _ => None,
    }
}

fn infix_to_opcode(op: &str) -> Option<OpCode> {
    match op {
        "==" => Some(OpCode::Eq),
        "!=" => Some(OpCode::NotEq),
        "+" => Some(OpCode::Add),
        "*" => Some(OpCode::Mult),
        "/" => Some(OpCode::Div),
        "%" => Some(OpCode::Modulo),
        "-" => Some(OpCode::Sub),
        ">" => Some(OpCode::Gt),
        ">=" => Some(OpCode::GtEq),
        "<" => Some(OpCode::Lt),
        "<=" => Some(OpCode::LtEq),

        _ => None,
    }
}

pub struct Compiler {
    symbol_table: SymbolTable,
    binding_name: Option<String>,
    current_function_arity: Option<usize>,
    module: ModuleName,
}

impl Compiler {
    pub fn new(module: ModuleName) -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            binding_name: None,
            current_function_arity: None,
            module,
        }
    }

    pub fn define_global(&mut self, name: &str) -> u16 {
        self.symbol_table.define_global(&self.module, name)
    }

    fn compile_expr_chunk(
        &mut self,
        f: &mut Function,
        ast: Expr,
        tail_position: bool,
    ) -> Result<(), String> {
        match ast {
            Expr::Lit(Lit::Nil) => f.bytecode.push(OpCode::ConstNil as u8),
            Expr::Lit(Lit::Bool(true)) => f.bytecode.push(OpCode::ConstTrue as u8),
            Expr::Lit(Lit::Bool(false)) => f.bytecode.push(OpCode::ConstFalse as u8),
            Expr::Lit(Lit::String(s)) => alloc_const(f, Value::String(Rc::new(s))),
            Expr::Lit(Lit::Num(n)) => alloc_const(f, Value::Num(n)),

            Expr::Ident(ident) => {
                let lookup = self.symbol_table.resolve(&self.module, &ident);

                match lookup {
                    Some(scope) => compile_symbol_lookup(f, scope),
                    None => return Err(format!("Lookup not found for {:?}", ident)),
                }
            }

            Expr::If {
                condition,
                if_branch,
                else_branch,
            } => {
                self.compile_expr_chunk(f, *condition, false)?;
                let first_jump_index = set_jump_placeholder(f, OpCode::JumpIfFalse);
                self.compile_expr_chunk(f, *if_branch, tail_position)?;
                let second_jump_index = set_jump_placeholder(f, OpCode::Jump);
                // TODO throw on overflow
                set_big_endian_u16(f, first_jump_index);
                self.compile_expr_chunk(f, *else_branch, tail_position)?;
                // TODO throw on overflow
                set_big_endian_u16(f, second_jump_index);
            }

            Expr::Fn { params, body } => {
                self.current_function_arity = Some(params.len());
                self.symbol_table.enter_scope(self.binding_name.clone());
                let mut inner_f = Function {
                    name: self.binding_name.clone(),
                    arity: params.len() as u8,
                    ..Function::default()
                };

                for required in params {
                    self.symbol_table.define_local(&required);
                }

                self.binding_name = None;
                self.compile_expr_chunk(&mut inner_f, *body, true)?;
                inner_f.bytecode.push(OpCode::Return as u8);
                inner_f.locals = self.symbol_table.count_locals() - &inner_f.arity;
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
                self.current_function_arity = None;
            }

            Expr::Call { f: caller, args } => {
                // TODO return err when args > 256
                let args_len = args.len();
                for arg in args {
                    self.compile_expr_chunk(f, arg, false)?;
                }

                let is_tailrec =
                    tail_position && self.check_if_tailrec(caller.deref(), args_len)?;
                if is_tailrec {
                    for index in (0..args_len).rev() {
                        f.bytecode.push(OpCode::SetLocal as u8);
                        f.bytecode.push(index as u8);
                    }
                    f.bytecode.push(OpCode::Jump as u8);
                    f.bytecode.push(0);
                    f.bytecode.push(0);
                } else {
                    self.compile_expr_chunk(f, *caller, false)?;
                    f.bytecode.push(OpCode::Call as u8);
                    f.bytecode.push(args_len as u8);
                }
            }

            Expr::Let { name, value, body } => {
                let binding_index = self.symbol_table.define_local(&name);
                self.binding_name = Some(name.clone());
                self.compile_expr_chunk(f, *value, false)?;
                self.binding_name = None;
                f.bytecode.push(OpCode::SetLocal as u8);
                f.bytecode.push(binding_index);
                self.compile_expr_chunk(f, *body, tail_position)?;
                self.symbol_table.remove_local(&name);
            }

            Expr::Do(left, right) => {
                self.compile_expr_chunk(f, *left, false)?;
                f.bytecode.push(OpCode::Pop as u8);
                self.compile_expr_chunk(f, *right, tail_position)?;
            }

            Expr::Prefix(op, value) => match prefix_to_opcode(&op) {
                Some(opcode) => {
                    self.compile_expr_chunk(f, *value, false)?;
                    f.bytecode.push(opcode as u8);
                }
                None => return Err(format!("Invalid prefix op: {op}")),
            },

            Expr::Infix(op, left, right) if op == "&&" => {
                self.compile_expr_chunk(f, *left, false)?;
                let jump_index = set_jump_placeholder(f, OpCode::JumpIfFalseElsePop);
                self.compile_expr_chunk(f, *right, tail_position)?;
                set_big_endian_u16(f, jump_index);
            }

            Expr::Infix(op, left, right) if op == "||" => {
                self.compile_expr_chunk(f, *left, false)?;
                let jump_index = set_jump_placeholder(f, OpCode::JumpIfTrueElsePop);
                self.compile_expr_chunk(f, *right, tail_position)?;
                set_big_endian_u16(f, jump_index);
            }

            Expr::Infix(op, left, right) if op == "|>" => {
                let desugared = desugar_pipe_right_macro(left, right)?;
                self.compile_expr_chunk(f, desugared, tail_position)?;
            }

            Expr::Infix(op, left, right) => {
                let opcode = infix_to_opcode(&op).ok_or(format!("Invalid infix op: {op}"))?;

                self.compile_expr_chunk(f, *left, false)?;
                self.compile_expr_chunk(f, *right, false)?;

                f.bytecode.push(opcode as u8);
            }
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
                self.binding_name = Some(name.clone());
                self.compile_expr_chunk(f, value, false)?;
                self.binding_name = None;

                f.bytecode.push(OpCode::SetGlobal as u8);

                let index = self.symbol_table.define_global(&self.module, &name);

                let (msb, lsb) = to_big_endian_u16(index);
                f.bytecode.push(msb);
                f.bytecode.push(lsb);
                Ok(())
            }
            Statement::Expr(expr) => self.compile_expr_chunk(f, expr, false),
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

    fn check_is_recursive_call(&mut self, caller: &Expr) -> bool {
        match caller {
            Expr::Ident(ident) => {
                self.symbol_table.resolve(&self.module, ident) == Some(Scope::Function)
            }
            _ => false,
        }
    }

    fn check_if_tailrec(&mut self, caller: &Expr, args_len: usize) -> Result<bool, String> {
        if self.check_is_recursive_call(caller) {
            let required_arity = self
                .current_function_arity
                .expect("Expected function context to have an arity");

            if args_len != required_arity {
                return  Err(format!("Invalid number of arguments: Required: {required_arity}, got {args_len} instead"));
            }

            Ok(true)
        } else {
            Ok(false)
        }
    }
}

fn desugar_pipe_right_macro(left: Box<Expr>, right: Box<Expr>) -> Result<Expr, String> {
    // TODO move this outside the compiler. It's just syntax sugar

    match *right {
        Expr::Call { f, args } => {
            let mut new_args = vec![*left];

            for arg in args {
                new_args.push(arg)
            }
            Ok(Expr::Call { f, args: new_args })
        }
        _ => Err(format!(
            "Invalid usage of `|>` macro: right element should be a function call"
        )),
    }
}

fn compile_symbol_lookup(f: &mut Function, scope: Scope) {
    match scope {
        Scope::Function => f.bytecode.push(OpCode::GetCurrentClosure as u8),
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

fn to_big_endian_u16(n: u16) -> (u8, u8) {
    let msb = n >> 8;
    (msb as u8, n as u8)
}

fn push_big_endian_u16(f: &mut Function, value: u16) {
    let (msb, lsb) = to_big_endian_u16(value);
    f.bytecode.push(msb);
    f.bytecode.push(lsb);
}

fn set_big_endian_u16(f: &mut Function, index: usize) {
    let value = f.bytecode.len() as u16;
    let (msb, lsb) = to_big_endian_u16(value);

    f.bytecode[index] = msb;
    f.bytecode[index + 1] = lsb;
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
