use crate::ast::Namespace;
use crate::interpreter::Interpreter;

mod list;
mod string;
mod utils;

pub fn load(interpreter: &mut Interpreter) {
    string::load(&Namespace::from(&["String"]), interpreter);
    list::load(&Namespace::from(&["List"]), interpreter);
}
