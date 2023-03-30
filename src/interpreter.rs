use crate::compiler::compiler::Compiler;
use crate::parser::{parse, ParsingError};
use crate::vm::value::Value;
use crate::vm::vm::Vm;

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

    pub fn run(&mut self, src: &str) -> Result<Value, Error> {
        let program = parse(src).map_err(Error::Parsing)?;
        let main = self
            .compiler
            .compile_program(program)
            .map_err(Error::Compilation)?;

        let value = self
            .vm
            .run_main(std::rc::Rc::new(main))
            .map_err(Error::Runtime)?;

        Ok(value)
    }
}
