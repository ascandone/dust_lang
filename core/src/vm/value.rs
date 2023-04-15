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
    Function(Rc<Function>),
    Closure(Rc<Closure>),
    NativeFunction(Rc<NativeFunction>),
}

impl Value {
    #[cfg(test)]
    pub fn str(str: &str) -> Value {
        Value::String(Rc::new(str.to_string()))
    }
}

impl TryInto<bool> for &Value {
    type Error = String;

    fn try_into(self) -> Result<bool, Self::Error> {
        match &self {
            Value::Bool(b) => Ok(*b),
            _ => Err(format!("Type error: expected a bool, got {self} instead")),
        }
    }
}

impl Value {
    pub fn as_bool(&self) -> Result<bool, String> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(format!("Type error: expected a bool, got {self} instead")),
        }
    }

    pub fn as_string(&self) -> Result<Rc<String>, String> {
        match self {
            Value::String(s) => Ok(Rc::clone(s)),
            _ => Err(format!("Type error: expected a string, got {self} instead")),
        }
    }

    pub fn as_fn(&self) -> Rc<Function> {
        match self {
            Value::Function(f) => f.clone(),
            _ => panic!("Type error: expected a function, got {self} instead"),
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
    }
}
