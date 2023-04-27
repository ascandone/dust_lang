use crate::ast::Namespace;
use crate::interpreter::Interpreter;

mod list;
mod map;
mod string;
mod tuple;
mod utils;

pub fn load(interpreter: &mut Interpreter) {
    string::load(&Namespace::from(&["String"]), interpreter);
    list::load(&Namespace::from(&["List"]), interpreter);
    tuple::load(&Namespace::from(&["Tuple"]), interpreter);
    map::load(&Namespace::from(&["Map"]), interpreter);
}
