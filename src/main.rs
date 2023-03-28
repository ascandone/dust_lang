use monkey_lang_rust_interpreter::compiler::compiler::Compiler;
use monkey_lang_rust_interpreter::parser::parse;
use monkey_lang_rust_interpreter::vm::vm::Vm;
use std::{env, fs, rc::Rc};

fn main() {
    let args: Vec<String> = env::args().collect();

    let &[_, ref file_path ,..] = args.as_slice() else {
        println!("Missing file args");
        return;
    };

    let content = fs::read_to_string(file_path).expect("Cannot read file");

    let program = match parse(&content) {
        Ok(parsed) => parsed,
        Err(err) => {
            println!("Parsing error: {:?}", err);
            return;
        }
    };

    let mut compiler = Compiler::new();

    let compiled_fn = compiler.compile_program(program).unwrap();

    let mut vm = Vm::default();

    let value = vm
        .run_main(Rc::new(compiled_fn))
        .expect("Error during execution");

    println!("{value}");
}
