use std::{borrow::Borrow, fmt::Display, rc::Rc};

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
    Cons(Rc<Value>, Rc<Value>),
    String(Rc<String>),
    Function(Rc<Function>),
    Closure(Rc<Closure>),
    NativeFunction(Rc<NativeFunction>),
}

impl Value {
    pub fn as_str(&self) -> &String {
        match self {
            Value::String(str) => str,
            _ => panic!("Type error: expected a String. Got {self} instead"),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => panic!("Type error: expected a bool. Got {self} instead"),
        }
    }

    pub fn as_fn(&self) -> Rc<Function> {
        match self {
            Value::Function(f) => f.clone(),
            _ => panic!("Type error: expected a function. Got {self} instead"),
        }
    }
}

impl From<Vec<Value>> for Value {
    fn from(xs: Vec<Value>) -> Self {
        xs.into_iter()
            .rfold(Value::Nil, |tl, hd| Value::Cons(Rc::new(hd), Rc::new(tl)))
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

            Value::Cons(hd, tl) => {
                write!(f, "(")?;

                hd.fmt(f)?;

                let mut tl = tl;

                loop {
                    match tl.borrow() {
                        Value::Nil => {
                            write!(f, ")")?;
                            return Ok(());
                        }

                        Value::Cons(hd, tl2) => {
                            write!(f, " ")?;
                            hd.fmt(f)?;
                            tl = tl2;
                        }

                        el => {
                            el.fmt(f)?;
                            write!(f, ")")?;
                            return Ok(());
                        }
                    }
                }
            }

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

        assert_eq!(
            format!(
                "{}",
                Value::Cons(Rc::new(Value::Num(42.0)), Rc::new(Value::Nil),)
            ),
            "(42)".to_string()
        );

        assert_eq!(
            format!(
                "{}",
                Value::Cons(
                    Rc::new(Value::Num(1.0)),
                    Rc::new(Value::Cons(
                        Rc::new(Value::Num(2.0)),
                        Rc::new(Value::Cons(Rc::new(Value::Num(3.0)), Rc::new(Value::Nil)))
                    )),
                )
            ),
            "(1 2 3)".to_string()
        );
    }
}
