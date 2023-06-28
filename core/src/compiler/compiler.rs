use super::symbol_table::{Scope, SymbolTable};
use crate::ast::{Expr, Ident, Import, Lit, Namespace, Pattern, Program, Statement};
use crate::vm::{
    bytecode::OpCode,
    value::{Function, Value},
};
use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

fn get_unique_var() -> String {
    static COUNTER: AtomicUsize = AtomicUsize::new(1);
    let c = COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("[[unique-{c}]]")
}

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

// Note: Clone is expensive because of HashMap. A persistent data structure would be a better fit
#[derive(Clone)]
struct ModuleContext {
    pub ns: Namespace,
    pub visible_modules: HashMap<Namespace, Namespace>,
}

impl ModuleContext {
    pub fn new(ns: Namespace) -> Self {
        Self {
            ns,
            visible_modules: HashMap::new(),
        }
    }
}

pub struct Compiler {
    symbol_table: SymbolTable,
    binding_name: Option<String>,
    current_function_arity: Option<usize>,
    module_context: ModuleContext,
    unimported_modules: HashMap<Namespace, Program>,
    imported_modules: HashSet<Namespace>,
}

impl Compiler {
    pub fn new(ns: Namespace) -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            binding_name: None,
            current_function_arity: None,
            module_context: ModuleContext::new(ns),
            unimported_modules: HashMap::new(),
            imported_modules: HashSet::new(),
        }
    }

    pub fn add_module(&mut self, ns: Namespace, program: Program) {
        self.unimported_modules.insert(ns, program);
    }

    pub fn define_global(&mut self, ns: &Namespace, name: &str) -> u16 {
        self.symbol_table.define_global(true, ns, name)
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
            Expr::Lit(l) => push_const(f, l.into()),

            Expr::Ident(ident) => {
                let Ident(ref ns, ref name) = ident;
                let ns = match ns {
                    None => None,
                    Some(ns) if ns == &self.module_context.ns => None,
                    Some(ns) => match self.module_context.visible_modules.get(ns) {
                        None => return Err(format!("Namespace was not imported: {ns}")),
                        Some(alias) => Some(alias.clone()),
                    },
                };

                let lookup = self
                    .symbol_table
                    .resolve(&self.module_context.ns, &Ident(ns, name.clone()));

                match lookup {
                    Some(scope) => compile_symbol_lookup(f, scope),
                    None => return Err(format!("Lookup not found for {ident}")),
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
                inner_f.locals = self.symbol_table.count_locals() - inner_f.arity;
                let free_vars = &self.symbol_table.free();
                self.symbol_table.exit_scope();

                if free_vars.is_empty() {
                    push_const(f, Value::Function(Rc::new(inner_f)));
                } else {
                    for scope in free_vars {
                        compile_symbol_lookup(f, *scope);
                    }
                    let ident = alloc_const(f, Value::Function(Rc::new(inner_f)));
                    f.bytecode.push(OpCode::MakeClosure as u8);
                    f.bytecode.push(free_vars.len() as u8);
                    f.bytecode.push(ident);
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

            Expr::Infix(op, left, right) => {
                let opcode = infix_to_opcode(&op).ok_or(format!("Invalid infix op: {op}"))?;

                self.compile_expr_chunk(f, *left, false)?;
                self.compile_expr_chunk(f, *right, false)?;

                f.bytecode.push(opcode as u8);
            }

            Expr::Match(expr, clauses) => {
                self.binding_name = match clauses.as_slice() {
                    [(Pattern::Identifier(name), _)] => Some(name.to_string()),
                    _ => None,
                };

                self.compile_expr_chunk(f, *expr, false)?;
                self.binding_name = None;

                let mut jump_indexes = vec![];

                let always_succeeds = matches!(clauses.last(), Some((Pattern::Identifier(_), _)));

                let root_var = get_unique_var();
                let root_ident = self.symbol_table.define_local(&root_var);
                f.bytecode.push(OpCode::SetLocal as u8);
                f.bytecode.push(root_ident);

                for (pattern, expr) in clauses {
                    let mut bound_locals = vec![];

                    let next_clause_indexes =
                        self.compile_pattern(f, pattern.clone(), root_ident, |ident| {
                            bound_locals.push(ident);
                        });

                    self.compile_expr_chunk(f, expr, tail_position)?;

                    self.symbol_table.remove_local(&root_var);

                    for local in bound_locals {
                        self.symbol_table.remove_local(&local);
                    }

                    if !matches!(&pattern, Pattern::Identifier(_)) {
                        let j_index = set_jump_placeholder(f, OpCode::Jump);
                        jump_indexes.push(j_index);
                    }

                    for index in &next_clause_indexes {
                        set_big_endian_u16(f, *index);
                    }
                }

                if !always_succeeds {
                    f.bytecode.push(OpCode::PanicNoMatch as u8);
                }

                for jump_index in jump_indexes {
                    set_big_endian_u16(f, jump_index);
                }
            }
        };

        Ok(())
    }

    fn compile_pattern<F>(
        &mut self,
        f: &mut Function,
        pattern: Pattern,
        root_ident: u8,
        mut handle_ident: F,
    ) -> Vec<usize>
    where
        F: FnMut(String),
    {
        let mut patterns: Vec<(u8, Pattern)> = vec![(root_ident, pattern)];

        let mut next_clause_indexes = vec![];

        loop {
            match patterns.pop() {
                None => break,
                Some((ident_id, pattern)) => match pattern {
                    Pattern::Identifier(pattern_ident) => {
                        self.symbol_table
                            .define_local_alias(&pattern_ident, ident_id);

                        handle_ident(pattern_ident);
                    }

                    Pattern::Lit(l) => {
                        let j_index = set_jump_placeholder(f, OpCode::MatchConstElseJump);
                        next_clause_indexes.push(j_index);

                        let const_index = alloc_const(f, l.into());
                        f.bytecode.push(const_index);
                        f.bytecode.push(ident_id)
                    }

                    Pattern::EmptyList => {
                        let j_index = set_jump_placeholder(f, OpCode::MatchEmptyListElseJump);
                        f.bytecode.push(ident_id);
                        next_clause_indexes.push(j_index);
                    }

                    Pattern::Cons(hd, tl) => {
                        let j_index = set_jump_placeholder(f, OpCode::MatchConsElseJump);
                        next_clause_indexes.push(j_index);
                        f.bytecode.push(ident_id);

                        let mut reversed_patterns = vec![];

                        for pattern in vec![hd, tl] {
                            let var_name = get_unique_var();
                            let ident_id = self.symbol_table.define_local(&var_name);

                            f.bytecode.push(OpCode::SetLocal as u8);
                            f.bytecode.push(ident_id);

                            reversed_patterns.push((ident_id, pattern.deref().clone()));
                        }

                        for t in reversed_patterns.into_iter().rev() {
                            patterns.push(t)
                        }
                    }

                    Pattern::EmptyMap => {
                        let j_index = set_jump_placeholder(f, OpCode::MatchEmptyMapElseJump);
                        f.bytecode.push(ident_id);
                        next_clause_indexes.push(j_index);
                    }

                    Pattern::ConsMap((k, v), rest) => {
                        let j_index = set_jump_placeholder(f, OpCode::MatchConsMapElseJump);
                        next_clause_indexes.push(j_index);

                        let const_index = alloc_const(f, k.into());
                        f.bytecode.push(const_index);

                        f.bytecode.push(ident_id);

                        let mut reversed_patterns = vec![];

                        for pattern in vec![v, rest] {
                            let var_name = get_unique_var();
                            let ident_id = self.symbol_table.define_local(&var_name);

                            f.bytecode.push(OpCode::SetLocal as u8);
                            f.bytecode.push(ident_id);

                            reversed_patterns.push((0, pattern.deref().clone()));
                        }

                        for t in reversed_patterns.into_iter().rev() {
                            patterns.push(t)
                        }
                    }

                    Pattern::Tuple(tuple_patterns) => {
                        let j_index = set_jump_placeholder(f, OpCode::MatchTuple2ElseJump);
                        next_clause_indexes.push(j_index);
                        f.bytecode.push(ident_id);

                        let mut reversed_patterns = vec![];
                        for pattern in tuple_patterns.into_iter() {
                            let var_name = get_unique_var();

                            let ident_id = self.symbol_table.define_local(&var_name);

                            f.bytecode.push(OpCode::SetLocal as u8);
                            f.bytecode.push(ident_id);

                            reversed_patterns.push((ident_id, pattern.clone()));
                        }

                        for t in reversed_patterns.into_iter().rev() {
                            patterns.push(t)
                        }
                    }
                },
            };
        }

        next_clause_indexes
    }

    pub fn import_module(&mut self, ns: Namespace, rename: Option<Namespace>) {
        self.module_context
            .visible_modules
            .insert(rename.unwrap_or(ns.clone()), ns);
    }

    fn compile_statement_chunk(
        &mut self,
        f: &mut Function,
        statement: Statement,
    ) -> Result<(), String> {
        match statement {
            Statement::Import(Import { ns, rename }) => {
                self.import_module(ns.clone(), rename);
                if self.imported_modules.contains(&ns) {
                    f.bytecode.push(OpCode::ConstNil as u8);
                    Ok(())
                } else {
                    match self.unimported_modules.remove(&ns) {
                        None => Err(format!("Module not found: {ns}")),
                        Some(program) => {
                            self.imported_modules.insert(ns.clone());

                            let this_ctx = self.module_context.clone();

                            self.module_context = ModuleContext::new(ns);
                            self.compile_program_chunk(f, program)?;
                            self.module_context = this_ctx;
                            Ok(())
                        }
                    }
                }
            }
            Statement::Let {
                pattern,
                value,
                public,
            } => {
                let name = match pattern {
                    Pattern::Identifier(name) => name,
                    _ => todo!("let statement with pattern"),
                };

                self.binding_name = Some(name.clone());
                self.compile_expr_chunk(f, value, false)?;
                self.binding_name = None;

                f.bytecode.push(OpCode::SetGlobal as u8);

                let index = self
                    .symbol_table
                    .define_global(public, &self.module_context.ns, &name);

                let (msb, lsb) = to_big_endian_u16(index);
                f.bytecode.push(msb);
                f.bytecode.push(lsb);

                f.bytecode.push(OpCode::ConstNil as u8);

                Ok(())
            }
            Statement::Expr(expr) => self.compile_expr_chunk(f, expr, false),
        }
    }

    fn compile_program_chunk(&mut self, f: &mut Function, program: Program) -> Result<(), String> {
        if program.is_empty() {
            f.bytecode.push(OpCode::ConstNil as u8);
            return Ok(());
        }

        for (i, statement) in program.into_iter().enumerate() {
            if i != 0 {
                f.bytecode.push(OpCode::Pop as u8)
            }
            self.compile_statement_chunk(f, statement)?;
        }

        Ok(())
    }

    /// Compile an AST expression into a zero-arity function containing it's chunk of bytecode.
    pub fn compile_program(&mut self, program: Program, name: &str) -> Result<Function, String> {
        let mut f = Function {
            name: Some(name.to_string()),
            ..Default::default()
        };

        self.compile_program_chunk(&mut f, program)?;

        f.bytecode.push(OpCode::Return as u8);
        f.locals = self.symbol_table.count_locals();
        Ok(f)
    }

    #[cfg(test)]
    pub fn compile_expr(&mut self, expr: Expr) -> Result<Function, String> {
        self.compile_program(vec![Statement::Expr(expr)], "main")
    }

    fn check_is_recursive_call(&mut self, caller: &Expr) -> bool {
        match caller {
            Expr::Ident(ident) => {
                self.symbol_table.resolve(&self.module_context.ns, ident) == Some(Scope::Function)
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

fn alloc_const(f: &mut Function, value: Value) -> u8 {
    let cached = f.constant_pool.iter().enumerate().find(|(_, v)| match v {
        Value::String(_) | Value::Num(_) => v == &&value,
        _ => false,
    });

    match cached {
        None => {
            let current_index = f.constant_pool.len();
            f.constant_pool.push(value);
            current_index as u8
        }

        Some((cached_index, _)) => cached_index as u8,
    }
}

fn push_const(f: &mut Function, value: Value) {
    let index = alloc_const(f, value);
    f.bytecode.push(OpCode::Const as u8);
    f.bytecode.push(index);
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
