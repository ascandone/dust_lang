use crate::ast::Namespace;
use crate::interpreter::Interpreter;

mod string;

pub fn load(interpreter: &mut Interpreter) {
    string::load(&Namespace::from(&["String"]), interpreter);
}
