use crate::project_interpreter::project_interpreter;
use argh::FromArgs;
use core::interpreter;
use std::fs;

#[derive(FromArgs, PartialEq)]
/// Run a dust script
#[argh(subcommand, name = "run")]
pub struct Run {
    #[argh(positional)]
    /// file path
    path: String,
}

impl Run {
    pub fn run(&self) {
        let content = fs::read_to_string(self.path.clone()).unwrap_or_else(|_| {
            eprintln!(
                "Cannot open file `{}` (no such file or directory)",
                self.path
            );
            std::process::exit(1)
        });

        let mut interpreter = project_interpreter().unwrap_or_else(|e| {
            handle_interpreter_err(e);
        });

        let value = interpreter.run(&self.path, &content).unwrap_or_else(|e| {
            handle_interpreter_err(e);
        });

        println!("{value}");
    }
}

fn handle_interpreter_err(err: interpreter::Error) -> ! {
    match err {
        interpreter::Error::Parsing(e) => {
            eprintln!("Parsing error: {:?}", e);
        }
        interpreter::Error::Compilation(e) => {
            eprintln!("Compilation error: {e}");
        }
        interpreter::Error::Runtime(e) => eprintln!("{e}"),
    }

    std::process::exit(1)
}
