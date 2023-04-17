use crate::ast::Namespace;
use crate::compiler::compiler::Compiler;
use crate::parser::{parse_ast, ParsingError};
use crate::std_lib;
use crate::vm::value::{Function, NativeFunction, Value};
use crate::vm::vm::{RuntimeErr, Vm};
use std::fmt::{Display, Formatter};
use std::rc::Rc;

pub struct Interpreter {
    compiler: Compiler,
    vm: Vm,
}

#[derive(Debug)]
pub enum Error {
    Parsing(ParsingError),
    Compilation(String),
    Runtime(RuntimeErr),
}

pub struct ErrorFmt {
    // TODO add context info (like src, ns, ...)
    pub error: Error,
}

impl Display for ErrorFmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.error {
            Error::Parsing(e) => {
                write!(f, "Parsing error:\n{e}")
            }
            Error::Compilation(e) => {
                write!(f, "Compilation error:\n{e}")
            }
            Error::Runtime(e) => {
                write!(f, "Execution error:\n{e}")
            }
        }
    }
}

pub fn eval(name: &str, src: &str) -> Result<Value, Error> {
    Interpreter::new().run(name, src)
}

pub fn eval_dis(name: &str, src: &str) -> Result<(Value, Rc<Function>), Error> {
    Interpreter::new().run_debug_fn(name, src)
}

impl Interpreter {
    pub fn new() -> Self {
        let ns = Namespace(vec!["User".to_string()]);

        let mut instance = Self {
            compiler: Compiler::new(ns),
            vm: Vm::default(),
        };

        std_lib::load(&mut instance);

        instance
    }

    pub fn add_module(&mut self, ns: Namespace, src: &str) -> Result<(), Error> {
        let program = parse_ast(src).map_err(Error::Parsing)?;
        self.compiler.add_module(ns, program);
        Ok(())
    }

    pub fn define_native<F>(&mut self, ns: &Namespace, name: &str, args_number: u8, body: F)
    where
        F: Fn(&[Value]) -> Result<Value, String> + 'static,
    {
        let id = self.compiler.define_global(&ns, name);

        let native_f = Rc::new(NativeFunction {
            name: name.to_string(),
            body: Box::new(body),
            args_number,
        });

        self.vm.define_global(id, Value::NativeFunction(native_f));
    }

    pub fn run(&mut self, name: &str, src: &str) -> Result<Value, Error> {
        let (value, _) = self.run_debug_fn(name, src)?;
        Ok(value)
    }

    pub fn run_debug_fn(&mut self, name: &str, src: &str) -> Result<(Value, Rc<Function>), Error> {
        let program = parse_ast(src).map_err(Error::Parsing)?;
        let main = self
            .compiler
            .compile_program(program, name)
            .map_err(Error::Compilation)?;

        let main = Rc::new(main);

        let value = self.vm.run_main(Rc::clone(&main)).map_err(Error::Runtime)?;

        Ok((value, main))
    }
}
