use crate::ast::Namespace;
use crate::interpreter::Interpreter;

mod string;
mod utils;

pub fn load(interpreter: &mut Interpreter) {
    string::load(&Namespace::from(&["String"]), interpreter);
}
