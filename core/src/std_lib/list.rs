use crate::ast::Namespace;
use crate::interpreter::Interpreter;
use crate::std_lib::utils::{arity_1, arity_2};
use crate::vm::list::List;
use crate::vm::value::Value;
use std::ops::Deref;
use std::rc::Rc;

fn empty(_: &[Value]) -> Result<Value, String> {
    Ok(Value::List(Rc::new(List::Empty)))
}

fn cons(values: &[Value]) -> Result<Value, String> {
    let (head, tail) = arity_2(values);
    let tail = tail.as_list()?;
    let lst = List::Cons(head.clone(), tail);
    Ok(Value::List(Rc::new(lst)))
}

fn head(values: &[Value]) -> Result<Value, String> {
    let lst = arity_1(values).as_list()?;
    Ok(match lst.deref() {
        List::Cons(hd, _) => hd.clone(),
        List::Empty => Value::Nil,
    })
}

fn tail(values: &[Value]) -> Result<Value, String> {
    let lst = arity_1(values).as_list()?;
    Ok(match lst.deref() {
        List::Cons(_, tl) => Value::List(Rc::clone(tl)),
        List::Empty => Value::Nil,
    })
}

fn is_empty(values: &[Value]) -> Result<Value, String> {
    let lst = arity_1(values).as_list()?;
    Ok(matches!(lst.deref(), List::Empty).into())
}

const MOD: &str = include_str!("List.ds");

pub fn load(ns: &Namespace, interpreter: &mut Interpreter) {
    interpreter.define_native(ns, "empty", 0, empty);
    interpreter.define_native(ns, "cons", 2, cons);
    interpreter.define_native(ns, "head", 1, head);
    interpreter.define_native(ns, "tail", 1, tail);
    interpreter.define_native(ns, "is_empty", 1, is_empty);
    interpreter.add_module(ns.clone(), MOD).unwrap();
}
