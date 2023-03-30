use crate::compiler::compiler::Compiler;
use crate::parser::{parse, ParsingError};
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
        Self {
            compiler: Compiler::new(),
            vm: Vm::default(),
        }
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
        let program = parse(src).map_err(Error::Parsing)?;
        let main = self
            .compiler
            .compile_program(program)
            .map_err(Error::Compilation)?;

        let value = self.vm.run_main(Rc::new(main)).map_err(Error::Runtime)?;

        Ok(value)
    }
}