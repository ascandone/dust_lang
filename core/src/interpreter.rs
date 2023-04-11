use crate::ast::Namespace;
use crate::compiler::compiler::Compiler;
use crate::parser::{parse_ast, ParsingError};
use crate::vm::value::{NativeFunction, Value};
use crate::vm::vm::Vm;
use std::rc::Rc;

pub struct Interpreter {
    compiler: Compiler,
    vm: Vm,
}

#[derive(Debug)]
pub enum Error {
    Parsing(ParsingError),
    Compilation(String),
    Runtime(String),
}

pub fn eval(src: &str) -> Result<Value, Error> {
    Interpreter::new().run(src)
}

impl Interpreter {
    pub fn new() -> Self {
        let ns = Namespace(vec!["User".to_string()]);

        Self {
            compiler: Compiler::new(ns),
            vm: Vm::default(),
        }
    }

    pub fn add_module(&mut self, ns: Namespace, src: &str) -> Result<(), Error> {
        let program = parse_ast(src).map_err(Error::Parsing)?;
        self.compiler.add_module(ns, program);
        Ok(())
    }

    pub fn define_native<F>(&mut self, name: &str, args_number: u8, body: F)
    where
        F: Fn(&[Value]) -> Result<Value, String> + 'static,
    {
        let id = self.compiler.define_global(name);

        let native_f = Rc::new(NativeFunction {
            name: name.to_string(),
            body: Box::new(body),
            args_number,
        });

        self.vm.define_global(id, Value::NativeFunction(native_f));
    }

    pub fn run(&mut self, src: &str) -> Result<Value, Error> {
        let program = parse_ast(src).map_err(Error::Parsing)?;
        let main = self
            .compiler
            .compile_program(program)
            .map_err(Error::Compilation)?;

        let value = self.vm.run_main(Rc::new(main)).map_err(Error::Runtime)?;

        Ok(value)
    }
}
