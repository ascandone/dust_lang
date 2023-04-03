use argh::FromArgs;
use core::interpreter::{Error, Interpreter};
use std::fs;

#[derive(FromArgs, PartialEq, Debug)]
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

        let mut interpreter = Interpreter::new();
        let value = interpreter.run(&content).unwrap_or_else(|e| {
            match e {
                Error::Parsing(e) => {
                    eprintln!("Parsing error: {:?}", e);
                }
                Error::Compilation(e) => {
                    eprintln!("Compilation error: {e}");
                }
                Error::Runtime(e) => eprintln!("{e}"),
            }

            std::process::exit(1)
        });

        println!("{value}");
    }
}
