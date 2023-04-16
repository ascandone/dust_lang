use crate::ast::Namespace;
use crate::interpreter::Interpreter;
use crate::std_lib::utils::arity_1;
use crate::vm::value::Value;

fn dis(values: &[Value]) -> Result<Value, String> {
    let f = arity_1(values).as_fn()?;
    let f = &(*f);

    print!("{f}");

    Ok(Value::Nil)
}

pub fn load(ns: &Namespace, interpreter: &mut Interpreter) {
    interpreter.define_native(ns, "dis", 1, dis);
}
