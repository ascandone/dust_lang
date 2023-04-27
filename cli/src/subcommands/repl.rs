use crate::project_interpreter::project_interpreter;
use argh::FromArgs;
use colored::{ColoredString, Colorize};
use core::interpreter::ErrorFmt;
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

fn colored_value(value: &Value) -> ColoredString {
    match value {
        Value::Nil => value.to_string().white().dimmed(),
        Value::Bool(_) => value.to_string().yellow().bold(),
        Value::Num(_) => value.to_string().blue(),
        Value::String(_) => value.to_string().bright_green(),
        Value::Tuple2(x, y) => format!("#({}, {})", colored_value(&x), colored_value(&y)).white(),
        Value::Tuple3(x, y, z) => format!(
            "#({}, {}, {})",
            colored_value(x),
            colored_value(y),
            colored_value(z)
        )
        .white(),

        // TODO colored list
        Value::List(_) => value.to_string().white(),
        Value::NativeFunction(_) | Value::Closure(_) | Value::Function(_) => {
            value.to_string().cyan()
        }
    }
}

impl Repl {
    pub fn run(&self) {
        let mut interpreter = project_interpreter().unwrap_or_else(|error| {
            let err_fmt = ErrorFmt { error };
            eprintln!("{}", err_fmt.to_string().red());
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
                Err(error) => {
                    let err_fmt = ErrorFmt { error };
                    eprintln!("{}", err_fmt.to_string().red())
                }
                Ok((value, f)) => {
                    if self.debug_bytecode {
                        println!("{f}");
                    };
                    println!("{}", colored_value(&value))
                }
            };
        }
    }
}
