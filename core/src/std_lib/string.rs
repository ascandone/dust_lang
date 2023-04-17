use crate::ast::Namespace;
use crate::interpreter::Interpreter;
use crate::std_lib::utils::{arity_1, arity_2};
use crate::vm::value::Value;
use std::rc::Rc;

fn length(values: &[Value]) -> Result<Value, String> {
    let str = arity_1(values).as_string()?;
    Ok(Value::Num(str.len() as f64))
}

fn show(values: &[Value]) -> Result<Value, String> {
    let value = arity_1(values);
    let str = format!("{value}");
    Ok(Value::String(Rc::new(str)))
}

fn concat(values: &[Value]) -> Result<Value, String> {
    let (left, right) = arity_2(values);

    let mut s = left.as_string()?.to_string();
    s.push_str(&right.as_string()?);
    Ok(Value::String(Rc::new(s)))
}

const MOD: &'static str = include_str!("String.ds");

pub fn load(ns: &Namespace, interpreter: &mut Interpreter) {
    interpreter.define_native(ns, "length", 1, length);
    interpreter.define_native(ns, "show", 1, show);
    interpreter.define_native(ns, "concat", 2, concat);

    interpreter.add_module(ns.clone(), MOD).unwrap();
}

#[cfg(test)]
mod tests {
    use crate::std_lib::string::*;

    #[test]
    fn len_test() {
        assert_eq!(length(&[Value::str("abc")]), Ok(Value::Num(3.0)));
        assert_eq!(length(&[Value::str("")]), Ok(Value::Num(0.0)));
    }
}
