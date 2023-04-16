use crate::ast::Namespace;
use crate::interpreter::Interpreter;

mod dis;
mod string;
mod utils;

pub fn load(interpreter: &mut Interpreter) {
    string::load(&Namespace::from(&["String"]), interpreter);
    dis::load(&Namespace::from(&["Dis"]), interpreter);
}
