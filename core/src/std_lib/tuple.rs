use crate::ast::Namespace;
use crate::interpreter::Interpreter;
use crate::std_lib::utils::{arity_2, arity_3};
use crate::vm::value::Value;
use std::rc::Rc;

pub fn tuple2(values: &[Value]) -> Result<Value, String> {
    let (x, y) = arity_2(values);
    Ok(Value::Tuple2(Rc::new(x.clone()), Rc::new(y.clone())))
}

pub fn tuple3(values: &[Value]) -> Result<Value, String> {
    let (x, y, z) = arity_3(values);
    Ok(Value::Tuple3(
        Rc::new(x.clone()),
        Rc::new(y.clone()),
        Rc::new(z.clone()),
    ))
}

pub fn load(ns: &Namespace, interpreter: &mut Interpreter) {
    interpreter.define_native(ns, "tuple2", 2, tuple2);
    interpreter.define_native(ns, "tuple3", 3, tuple3);
    interpreter.add_module(ns.clone(), "").unwrap();
}
