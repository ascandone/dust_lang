use crate::vm::value::Value;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

pub type Program = Vec<Statement>;

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct Namespace(pub Vec<String>);

impl Namespace {
    pub fn from(ns: &[&str]) -> Self {
        let ns = ns.iter().map(|c| c.to_string()).collect();
        Namespace(ns)
    }
}

impl Display for Namespace {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (index, ident) in self.0.iter().enumerate() {
            if index != 0 {
                write!(f, ".")?;
            }

            write!(f, "{ident}")?;
        }

        Ok(())
    }
}

#[derive(PartialEq, Debug)]
pub struct Import {
    pub ns: Namespace,
    pub rename: Option<Namespace>,
}

impl Import {
    #[cfg(test)]
    pub fn new(ns: Namespace) -> Self {
        Self { ns, rename: None }
    }
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Let {
        public: bool,
        name: String,
        value: Expr,
    },
    Import(Import),
    Expr(Expr),
}

#[derive(PartialEq, Clone)]
pub enum Lit {
    Nil,
    Bool(bool),
    Num(f64),
    String(String),
}

impl From<Lit> for Value {
    fn from(lit: Lit) -> Self {
        match lit {
            Lit::Nil => Value::Nil,
            Lit::Bool(b) => Value::Bool(b),
            Lit::Num(n) => Value::Num(n),
            Lit::String(s) => Value::String(Rc::new(s)),
        }
    }
}

impl Display for Lit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Nil => write!(f, "nil"),
            Lit::Bool(true) => write!(f, "true"),
            Lit::Bool(false) => write!(f, "false"),
            Lit::Num(n) => write!(f, "{n}"),
            Lit::String(str) => write!(f, "\"{str}\""),
        }
    }
}

impl fmt::Debug for Lit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Ident(pub Option<Namespace>, pub String);

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Ident(ns, name) = self;

        match ns {
            None => (),
            Some(ns) => write!(f, "{ns}.")?,
        }

        write!(f, "{name}")?;
        Ok(())
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Ident(Ident),
    Lit(Lit),
    Do(Box<Expr>, Box<Expr>),
    If {
        condition: Box<Expr>,
        if_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    Prefix(String, Box<Expr>),
    Infix(String, Box<Expr>, Box<Expr>),
    Call {
        f: Box<Expr>,
        args: Vec<Expr>,
    },
    Fn {
        params: Vec<String>,
        body: Box<Expr>,
    },
    Match(Box<Expr>, Vec<(Pattern, Expr)>),
}

#[cfg(test)]
pub struct Let {
    pub name: String,
    pub value: Box<Expr>,
    pub body: Box<Expr>,
}

#[cfg(test)]
impl Let {
    pub fn as_match(&self) -> Expr {
        use std::ops::Deref;

        Expr::Match(
            self.value.clone(),
            vec![(
                Pattern::Identifier(self.name.clone()),
                self.body.deref().clone(),
            )],
        )
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Pattern {
    Identifier(String),
    Lit(Lit),
    Tuple(Vec<Pattern>),
    EmptyList,
    Cons(Box<Pattern>, Box<Pattern>),
    EmptyMap,
    ConsMap((String, Box<Pattern>), Box<Pattern>),
}

#[cfg(test)]
impl From<&str> for Pattern {
    fn from(name: &str) -> Self {
        Pattern::Identifier(name.to_string())
    }
}

/// utility to create an unqualified identifier
pub fn ident(name: &str) -> Expr {
    Expr::Ident(Ident(None, name.to_string()))
}

pub const NIL: Expr = Expr::Lit(Lit::Nil);

impl From<bool> for Expr {
    fn from(b: bool) -> Self {
        Expr::Lit(Lit::Bool(b))
    }
}

impl From<f64> for Expr {
    fn from(n: f64) -> Self {
        Expr::Lit(Lit::Num(n))
    }
}

impl From<&str> for Expr {
    fn from(s: &str) -> Self {
        Expr::Lit(Lit::String(s.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Ident, Namespace};

    #[test]
    fn display_ns() {
        assert_eq!("A", format!("{}", Namespace(vec!["A".to_string()])));
        assert_eq!(
            "A.B",
            format!("{}", Namespace(vec!["A".to_string(), "B".to_string(),]))
        );
    }

    #[test]
    fn display_ident() {
        {
            let ident = Ident(None, "x".to_string());
            assert_eq!("x", format!("{ident}",))
        }

        {
            let ident = Ident(Some(Namespace(vec!["A".to_string()])), "x".to_string());
            assert_eq!("A.x", format!("{ident}",))
        }

        {
            let ident = Ident(
                Some(Namespace(vec!["A".to_string(), "B".to_string()])),
                "x".to_string(),
            );
            assert_eq!("A.B.x", format!("{ident}",))
        }
    }
}
