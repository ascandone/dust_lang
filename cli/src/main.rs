use core::interpreter::eval;
use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();

    let &[_, ref file_path ,..] = args.as_slice() else {
        println!("Missing file args");
        return;
    };

    let content = fs::read_to_string(file_path).expect("Cannot read file");

    let value = eval(&content).unwrap();

    println!("{value}");
}
