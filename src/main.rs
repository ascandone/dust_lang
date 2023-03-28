use crate::parser::parse;
use std::{env, fs};

pub mod ast;
mod compiler;
mod parser;
mod vm;

fn main() {
    let args: Vec<String> = env::args().collect();

    let &[_, ref file_path ,..] = args.as_slice() else {
        println!("Missing file args");
        return;
    };

    let Ok(content) = fs::read_to_string(file_path) else {
        println!("Cannot read file");
        return;
    };

    let declarations = match parse(&content) {
        Ok(parsed) => parsed,
        Err(err) => {
            println!("Parsing error: {:?}", err);
            return;
        }
    };

    println!("{:?}", declarations);
}
