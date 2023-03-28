use std::{borrow::Borrow, fmt::Display, rc::Rc};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct FunctionArity {
    pub required: u8,
    pub optional: u8,
    pub rest: bool,
}

/// A compiled function
#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct Function {
    pub arity: FunctionArity,
    pub name: Option<String>,
    pub bytecode: Vec<u8>,
    pub locals: u8,
    pub constant_pool: Vec<Value>,
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct Closure {
    pub function: Rc<Function>,
    pub free: Vec<Value>,
}

/// Runtime value representation
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum Value {
    #[default]
    Nil,
    Bool(bool),
    Int(i64),
    Cons(Rc<Value>, Rc<Value>),
    String(Rc<String>),
    Symbol(Rc<String>),
    Function(Rc<Function>),
    Closure(Rc<Closure>),
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

            Value::Int(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Function(r) => {
                let name = match r.name.clone() {
                    Some(n) => n,
                    None => "<anonymous>".to_string(),
                };

                write!(f, "#[Function {name} at {:?}]", Rc::as_ptr(r))
            }
            Value::Closure(clo) => {
                write!(f, "#[Function <closure> at {:?}]", Rc::as_ptr(clo))
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
        assert_eq!(format!("{}", Value::Int(42)), "42".to_string());
        assert_eq!(format!("{}", Value::Bool(true)), "true".to_string());
        assert_eq!(format!("{}", Value::Bool(false)), "false".to_string());
        assert_eq!(
            format!("{}", Value::String(Rc::new("abc".to_string()))),
            "\"abc\"".to_string()
        );
        assert_eq!(
            format!("{}", Value::Symbol(Rc::new("abc".to_string()))),
            "abc".to_string()
        );

        assert_eq!(
            format!(
                "{}",
                Value::Cons(Rc::new(Value::Int(42)), Rc::new(Value::Nil),)
            ),
            "(42)".to_string()
        );

        assert_eq!(
            format!(
                "{}",
                Value::Cons(
                    Rc::new(Value::Int(1)),
                    Rc::new(Value::Cons(
                        Rc::new(Value::Int(2)),
                        Rc::new(Value::Cons(Rc::new(Value::Int(3)), Rc::new(Value::Nil)))
                    )),
                )
            ),
            "(1 2 3)".to_string()
        );
    }
}
