use crate::vm::list::List;
use std::{fmt::Display, rc::Rc};

/// A compiled function
#[derive(Clone, PartialEq, Debug, Default)]
pub struct Function {
    pub arity: u8,
    pub name: Option<String>,
    pub bytecode: Vec<u8>,
    pub locals: u8,
    pub constant_pool: Vec<Value>,
}

impl Function {
    pub fn display_name(&self) -> String {
        self.name.clone().unwrap_or("<anonymous>".to_string())
    }
}

#[derive(Clone, PartialEq, Debug, Default)]
pub struct Closure {
    pub function: Rc<Function>,
    pub free: Vec<Value>,
}

pub struct NativeFunction {
    pub name: String,
    pub args_number: u8,
    pub body: Box<dyn Fn(&[Value]) -> Result<Value, String>>,
}

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NativeFunction<{}>", self.name)
    }
}

impl PartialEq for NativeFunction {
    // TODO improve
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

/// Runtime value representation
#[derive(Debug, Clone, PartialEq, Default)]
pub enum Value {
    #[default]
    Nil,
    Bool(bool),
    Num(f64),
    String(Rc<String>),
    List(List<Value>),
    Tuple2(Rc<Value>, Rc<Value>),
    Tuple3(Rc<Value>, Rc<Value>, Rc<Value>),
    Function(Rc<Function>),
    Closure(Rc<Closure>),
    NativeFunction(Rc<NativeFunction>),
}

impl Value {
    #[cfg(test)]
    pub fn str(str: &str) -> Value {
        Value::String(Rc::new(str.to_string()))
    }

    fn type_err<T>(&self, expected: &str) -> Result<T, String> {
        Err(format!(
            "Type error: expected {expected}, got {self} instead"
        ))
    }
}

impl Value {
    pub fn as_bool(&self) -> Result<bool, String> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => self.type_err("a bool"),
        }
    }

    pub fn as_string(&self) -> Result<Rc<String>, String> {
        match self {
            Value::String(s) => Ok(Rc::clone(s)),
            _ => self.type_err("a string"),
        }
    }

    pub fn as_fn(&self) -> Result<Rc<Function>, String> {
        match self {
            Value::Function(f) => Ok(Rc::clone(f)),
            _ => self.type_err("a function"),
        }
    }

    pub fn as_list(&self) -> Result<&List<Value>, String> {
        match self {
            Value::List(l) => Ok(l),
            _ => self.type_err("a list"),
        }
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Num(value)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Num(value as f64)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        value.to_string().into()
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(Rc::new(value))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(true) => write!(f, "true"),
            Value::Bool(false) => write!(f, "false"),
            Value::Num(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::List(l) => write!(f, "{l}"),
            Value::Tuple2(x, y) => write!(f, "#({x}, {y})"),
            Value::Tuple3(x, y, z) => write!(f, "#({x}, {y}, {z})"),

            Value::Function(r) => {
                write!(f, "#[function {} at {:?}]", r.display_name(), Rc::as_ptr(r))
            }
            Value::Closure(clo) => {
                write!(
                    f,
                    "#[function (closure) {} at {:?}]",
                    clo.function.display_name(),
                    Rc::as_ptr(clo)
                )
            }
            Value::NativeFunction(native) => {
                write!(
                    f,
                    "#[function (built-in) {:?} at {:?}]",
                    native.name,
                    Rc::as_ptr(native)
                )
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use super::*;

    #[test]
    fn format_test() {
        assert_eq!(format!("{}", Value::Nil), "nil".to_string());
        assert_eq!(format!("{}", Value::Num(42.0)), "42".to_string());
        assert_eq!(format!("{}", Value::Bool(true)), "true".to_string());
        assert_eq!(format!("{}", Value::Bool(false)), "false".to_string());
        assert_eq!(
            format!("{}", Value::String(Rc::new("abc".to_string()))),
            "\"abc\"".to_string()
        );

        assert_eq!(
            format!(
                "{}",
                Value::List(List::from_vec(vec![
                    Value::Nil,
                    Value::Num(42.0),
                    Value::List(List::Empty)
                ]))
            ),
            "[nil, 42, []]".to_string()
        );
    }
}
