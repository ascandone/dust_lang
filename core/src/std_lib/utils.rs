use crate::vm::value::Value;

pub fn arity_1(values: &[Value]) -> &Value {
    match &values {
        &[value, ..] => value,
        _ => panic!("Arity error"),
    }
}

pub fn arity_2(values: &[Value]) -> (&Value, &Value) {
    match &values {
        &[a, b, ..] => (b, a),
        _ => panic!("Arity error"),
    }
}

pub fn arity_3(values: &[Value]) -> (&Value, &Value, &Value) {
    match &values {
        &[a, b, c, ..] => (c, b, a),
        _ => panic!("Arity error"),
    }
}
