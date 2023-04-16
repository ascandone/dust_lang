use crate::project_interpreter::project_interpreter;
use argh::FromArgs;
use colored::{ColoredString, Colorize};
use core::interpreter;
use core::vm::value::Value;
use std::io;
use std::io::Write;

#[derive(FromArgs, PartialEq)]
/// Run the Dust repl
#[argh(subcommand, name = "repl")]
pub struct Repl {
    /// show bytecode of interpreted code before evaluating it
    #[argh(switch)]
    debug_bytecode: bool,
}

fn colored_value(value: Value) -> ColoredString {
    match value {
        Value::Nil => value.to_string().white().dimmed(),
        Value::Bool(_) => value.to_string().yellow().bold(),
        Value::Num(_) => value.to_string().blue(),
        Value::String(_) => value.to_string().bright_green(),
        Value::NativeFunction(_) | Value::Closure(_) | Value::Function(_) => {
            value.to_string().cyan()
        }
    }
}

impl Repl {
    pub fn run(&self) {
        let mut interpreter = project_interpreter().unwrap_or_else(|e| {
            print_interpreter_err(e);
            std::process::exit(1);
        });

        loop {
            print!("> ");
            let () = io::stdout().flush().unwrap();

            let stdin = io::stdin();
            let input = {
                let mut user_input = String::new();
                stdin.read_line(&mut user_input).unwrap();
                user_input
            };

            match interpreter.run_debug_fn("<repl>", &input) {
                Err(e) => print_interpreter_err(e),
                Ok((value, f)) => {
                    if self.debug_bytecode {
                        println!("{f}");
                    };
                    println!("{}", colored_value(value))
                }
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
