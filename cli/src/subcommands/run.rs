use crate::dust_json::DustJson;
use argh::FromArgs;
use core::ast::Namespace;
use core::interpreter;
use core::interpreter::Interpreter;
use glob::glob;
use std::{env, fs};

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

        let mut interpreter = Interpreter::new();

        if let Some(dust_json) = DustJson::read() {
            let current_dir = env::current_dir().unwrap();
            for source_dir in &dust_json.source_directories {
                let pattern = format!("{}/{}/**/*.ds", current_dir.display(), source_dir);
                for entry in glob(&pattern).unwrap() {
                    let path = entry.unwrap().display().to_string();
                    let content = fs::read_to_string(path.clone()).unwrap_or_else(|_| {
                        eprintln!("Cannot open file `{}` (no such file or directory)", &path);
                        std::process::exit(1)
                    });

                    let split: Vec<_> = path.split("/").collect();
                    let (index, _) = split
                        .iter()
                        .enumerate()
                        .find(|(_, s)| s == &source_dir)
                        .unwrap();

                    let ns = &split[index + 1..]
                        .iter()
                        .map(|c| c.split(".").nth(0).unwrap().to_string())
                        .collect::<Vec<_>>();

                    interpreter
                        .add_module(Namespace(ns.clone()), &content)
                        .unwrap_or_else(|e| {
                            handle_interpreter_err(e);
                        });
                }
            }
        };

        let value = interpreter.run(&content).unwrap_or_else(|e| {
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
