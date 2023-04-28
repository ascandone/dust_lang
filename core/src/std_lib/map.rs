use crate::ast::Namespace;
use crate::interpreter::Interpreter;
use crate::std_lib::utils::arity_3;
use crate::vm::value::Value;
use im_rc::HashMap;
use std::ops::Deref;

fn empty(_: &[Value]) -> Result<Value, String> {
    Ok(Value::Map(HashMap::new()))
}

fn insert(values: &[Value]) -> Result<Value, String> {
    let (map, key, value) = arity_3(values);
    let map = map.as_map()?;
    let key = key.as_string()?.deref().clone();

    let mut map_copy = map.clone();
    map_copy.insert(key, value.clone());

    Ok(Value::Map(map_copy))
}

pub fn load(ns: &Namespace, interpreter: &mut Interpreter) {
    interpreter.define_native(ns, "empty", 0, empty);
    interpreter.define_native(ns, "insert", 3, insert);

    interpreter.add_module(ns.clone(), "", true).unwrap();
}
