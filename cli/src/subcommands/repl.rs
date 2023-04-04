use crate::project_interpreter::project_interpreter;
use argh::FromArgs;
use colored::Colorize;
use core::interpreter;
use std::io;
use std::io::Write;

#[derive(FromArgs, PartialEq)]
/// Run the dust repl
#[argh(subcommand, name = "repl")]
pub struct Repl {}

impl Repl {
    pub fn run(&self) {
        let mut interpreter = project_interpreter().unwrap_or_else(|e| {
            print_interpreter_err(e);
            std::process::exit(1);
        });

        loop {
            print!("{} ", ">".green());
            let () = io::stdout().flush().unwrap();

            let stdin = io::stdin();
            let input = {
                let mut user_input = String::new();
                stdin.read_line(&mut user_input).unwrap();
                user_input
            };

            match interpreter.run(&input) {
                Err(e) => print_interpreter_err(e),
                Ok(value) => println!("{}", value),
            };
        }
    }
}

fn print_interpreter_err(err: interpreter::Error) {
    let e = match err {
        interpreter::Error::Parsing(e) => format!("Parsing error: {:?}", e),
        interpreter::Error::Compilation(e) => format!("Compilation error: {e}"),
        interpreter::Error::Runtime(e) => format!("{e}"),
    };

    eprintln!("{}", e.red());
}
