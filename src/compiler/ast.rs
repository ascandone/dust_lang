use std::rc::Rc;

use crate::vm::value::{FunctionArity, Value};

use super::sexpr::SExpr;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Params {
    pub required: Vec<String>,
    pub optional: Vec<String>,
    pub rest: Option<String>,
}

impl Params {
    pub fn from_raw_params(params: Vec<String>) -> Self {
        let acc = &mut Params::default();

        // pre: &rest already encountered
        fn parse_rest(acc: &mut Params, rest_params: &[String]) {
            acc.rest = Some(rest_params[0].clone());
            // TODO check other invalid params
        }

        // pre: &opt already encountered
        fn parse_opt(acc: &mut Params, opt_params: &[String]) {
            for (i, opt_param) in opt_params.iter().enumerate() {
                match opt_param.as_str() {
                    "&opt" => panic!("Invalid duplicate opt param"),
                    "&rest" => {
                        parse_rest(acc, &opt_params[i + 1..]);
                        break;
                    }
                    _ => acc.optional.push(opt_param.clone()),
                }
            }
        }

        for (i, param) in params.iter().enumerate() {
            match param.as_str() {
                "&opt" => {
                    parse_opt(acc, &params[i + 1..]);
                    break;
                }
                "&rest" => {
                    parse_rest(acc, &params[i + 1..]);
                    break;
                }
                _ => acc.required.push(param.clone()),
            }
        }

        acc.clone()
    }
}

impl From<&Params> for FunctionArity {
    fn from(params: &Params) -> Self {
        Self {
            required: params.required.len() as u8,
            optional: params.optional.len() as u8,
            rest: params.rest.is_some(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ast {
    Const(Value),
    Symbol(String),
    Call(Box<Ast>, Vec<Ast>),

    // Special forms:
    Do(Vec<Ast>),
    Def(String, Box<Ast>),
    If(Box<Ast>, Box<Ast>, Box<Ast>),
    Let1(String, Box<Ast>, Box<Ast>),
    Lambda(Params, Box<Ast>),
}

impl TryFrom<&SExpr> for Ast {
    type Error = String;

    fn try_from(sexpr: &SExpr) -> Result<Self, Self::Error> {
        match sexpr {
            SExpr::Number(_) | SExpr::Bool(_) | SExpr::String(_) => {
                Ok(Ast::Const(sexpr_to_value(sexpr)))
            }

            SExpr::Symbol(s) => Ok(Ast::Symbol(s.clone())),

            SExpr::List(exprs) => match exprs.first() {
                None => Ok(Ast::Const(Value::Nil)),
                Some(SExpr::Symbol(s)) => match s.as_str() {
                    "quote" => {
                        let sexpr = match &exprs[1..] {
                            [value] => value,
                            _ => return Err("Invalid quote body".to_string()),
                        };

                        Ok(Ast::Const(sexpr_to_value(sexpr)))
                    }

                    "do" => {
                        let mapped = traverse_exprs(&exprs[1..])?;
                        Ok(Ast::Do(mapped))
                    }

                    "def" => match &exprs[1..] {
                        [SExpr::Symbol(binding), value] => {
                            Ok(Ast::Def(binding.clone(), Box::new(value.try_into()?)))
                        }

                        _ => Err("Invalid def form".to_string()),
                    },

                    "if" => match &exprs[1..] {
                        [cond, branch_1, branch_2] => Ok(Ast::If(
                            Box::new(cond.try_into()?),
                            Box::new(branch_1.try_into()?),
                            Box::new(branch_2.try_into()?),
                        )),
                        _ => Err("Invalid if form".to_string()),
                    },

                    "let1" => match &exprs[1..] {
                        [SExpr::List(pair), body] => match &pair[..] {
                            [SExpr::Symbol(binding), value] => Ok(Ast::Let1(
                                binding.clone(),
                                Box::new(value.try_into()?),
                                Box::new(body.try_into()?),
                            )),

                            _ => Err("Invalid let1 form (expected a pair of bindings)".to_string()),
                        },

                        _ => Err("Invalid let1 form (expected a list as first param)".to_string()),
                    },

                    "lambda*" => match &exprs[1..] {
                        [params, body] => {
                            let params = match params {
                                SExpr::List(raw_params) => {
                                    let params = raw_params
                                        .iter()
                                        .map(|sexpr| match sexpr {
                                            SExpr::Symbol(s) => s.clone(),
                                            _ => panic!("Invalid symbol in lambda*params list"),
                                        })
                                        .collect();

                                    Params::from_raw_params(params)
                                }
                                _ => return Err("Invalid lambda* params list".to_string()),
                            };

                            Ok(Ast::Lambda(params, Box::new(body.try_into()?)))
                        }

                        _ => Err("Invalid lambda* form".into()),
                    },

                    sym => {
                        let f = Box::new(Ast::Symbol(sym.to_string()));
                        let args = traverse_exprs(&exprs[1..])?;

                        Ok(Ast::Call(f, args))
                    }
                },

                Some(sexpr) => {
                    let hd = sexpr.try_into()?;
                    let args = traverse_exprs(&exprs[1..])?;

                    Ok(Ast::Call(Box::new(hd), args))
                }
            },
        }
    }
}

pub fn traverse_exprs(exprs: &[SExpr]) -> Result<Vec<Ast>, String> {
    let mut acc = vec![];

    for expr in exprs {
        let ast = expr.try_into()?;
        acc.push(ast);
    }

    Ok(acc)
}

// TODO use into trait
fn sexpr_to_value(sexpr: &SExpr) -> Value {
    match sexpr {
        SExpr::Number(n) => Value::Int(*n),
        SExpr::Symbol(s) => Value::Symbol(Rc::new(s.clone())),
        SExpr::Bool(b) => Value::Bool(*b),
        SExpr::String(s) => Value::String(Rc::new(s.clone())),
        SExpr::List(xs) => xs.iter().map(sexpr_to_value).collect::<Vec<_>>().into(),
    }
}

#[cfg(test)]
mod try_from_sexpr_tests {
    use std::rc::Rc;

    use super::*;

    #[test]
    fn from_const_test() {
        assert_eq!(
            (&SExpr::Number(42)).try_into(),
            Ok(Ast::Const(Value::Int(42)))
        );
        assert_eq!(
            (&SExpr::Bool(true)).try_into(),
            Ok(Ast::Const(Value::Bool(true)))
        );
        assert_eq!(
            (&SExpr::Bool(false)).try_into(),
            Ok(Ast::Const(Value::Bool(false)))
        );
        assert_eq!(
            (&SExpr::String("abc".to_string())).try_into(),
            Ok(Ast::Const(Value::String(Rc::new("abc".to_string()))))
        );

        assert_eq!(
            (&SExpr::List(vec![])).try_into(),
            Ok(Ast::Const(Value::Nil))
        );
    }

    #[test]
    fn from_sym_test() {
        assert_eq!(
            (&SExpr::Symbol("abc".to_string())).try_into(),
            Ok(Ast::Symbol("abc".to_string()))
        );
    }

    #[test]
    fn to_do_expr_test() {
        let expr = &SExpr::List(vec![
            SExpr::Symbol("do".to_string()),
            SExpr::Number(1),
            SExpr::Bool(true),
        ]);

        assert_eq!(
            expr.try_into(),
            Ok(Ast::Do(vec![
                Ast::Const(Value::Int(1)),
                Ast::Const(Value::Bool(true)),
            ]))
        )
    }

    #[test]
    fn if_expr_test() {
        let expr = &SExpr::List(vec![
            SExpr::Symbol("if".to_string()),
            SExpr::Bool(true),
            SExpr::Number(1),
            SExpr::Number(0),
        ]);

        assert_eq!(
            expr.try_into(),
            Ok(Ast::If(
                Box::new(Ast::Const(Value::Bool(true))),
                Box::new(Ast::Const(Value::Int(1))),
                Box::new(Ast::Const(Value::Int(0))),
            ))
        )
    }

    #[test]
    fn def_expr_test() {
        let expr = &SExpr::List(vec![
            SExpr::Symbol("def".to_string()),
            SExpr::Symbol("x".to_string()),
            SExpr::Number(42),
        ]);

        assert_eq!(
            expr.try_into(),
            Ok(Ast::Def(
                "x".to_string(),
                Box::new(Ast::Const(Value::Int(42)))
            ))
        )
    }

    #[test]
    fn let1_expr_test() {
        let expr = &SExpr::List(vec![
            SExpr::Symbol("let1".to_string()),
            SExpr::List(vec![SExpr::Symbol("x".to_string()), SExpr::Number(42)]),
            SExpr::Symbol("body".to_string()),
        ]);

        assert_eq!(
            expr.try_into(),
            Ok(Ast::Let1(
                "x".to_string(),
                Box::new(Ast::Const(Value::Int(42))),
                Box::new(Ast::Symbol("body".to_string()))
            ))
        )
    }

    #[test]
    fn lambda_expr_test() {
        let expr = &SExpr::List(vec![
            SExpr::Symbol("lambda*".to_string()),
            SExpr::List(vec![
                SExpr::Symbol("x".to_string()),
                SExpr::Symbol("y".to_string()),
            ]),
            SExpr::Symbol("body".to_string()),
        ]);

        assert_eq!(
            expr.try_into(),
            Ok(Ast::Lambda(
                Params {
                    required: vec!["x".to_string(), "y".to_string()],
                    optional: vec![],
                    rest: None
                },
                Box::new(Ast::Symbol("body".to_string()))
            ))
        )
    }

    #[test]
    fn quoted_int_test() {
        let expr = &SExpr::List(vec![SExpr::Symbol("quote".to_string()), SExpr::Number(42)]);
        assert_eq!(expr.try_into(), Ok(Ast::Const(Value::Int(42))))
    }

    #[test]
    fn quoted_bool_test() {
        let expr = &SExpr::List(vec![SExpr::Symbol("quote".to_string()), SExpr::Bool(true)]);
        assert_eq!(expr.try_into(), Ok(Ast::Const(Value::Bool(true))))
    }

    #[test]
    fn quoted_string_test() {
        let expr = &SExpr::List(vec![
            SExpr::Symbol("quote".to_string()),
            SExpr::String("abc".to_string()),
        ]);
        assert_eq!(
            expr.try_into(),
            Ok(Ast::Const(Value::String(Rc::new("abc".to_string()))))
        )
    }

    #[test]
    fn quoted_empty_list_test() {
        let expr = &SExpr::List(vec![
            SExpr::Symbol("quote".to_string()),
            SExpr::List(vec![]),
        ]);
        assert_eq!(expr.try_into(), Ok(Ast::Const(Value::Nil)))
    }

    #[test]
    fn quoted_list_test() {
        let expr = &SExpr::List(vec![
            SExpr::Symbol("quote".to_string()),
            SExpr::List(vec![SExpr::Number(0), SExpr::Number(1)]),
        ]);

        let value = Value::Cons(
            Rc::new(Value::Int(0)),
            Rc::new(Value::Cons(Rc::new(Value::Int(1)), Rc::new(Value::Nil))),
        );

        assert_eq!(expr.try_into(), Ok(Ast::Const(value)));
    }

    #[test]
    fn quoted_sym_test() {
        let expr = &SExpr::List(vec![
            SExpr::Symbol("quote".to_string()),
            SExpr::Symbol("abc".to_string()),
        ]);
        assert_eq!(
            expr.try_into(),
            Ok(Ast::Const(Value::Symbol(Rc::new("abc".to_string())))),
        )
    }

    #[test]
    fn call_symbol_test() {
        // (f x)
        let expr = &SExpr::List(vec![
            SExpr::Symbol("f".to_string()),
            SExpr::Symbol("x".to_string()),
        ]);

        assert_eq!(
            expr.try_into(),
            Ok(Ast::Call(
                Box::new(Ast::Symbol("f".to_string())),
                vec![Ast::Symbol("x".to_string()),],
            )),
        )
    }

    #[test]
    fn call_value_test() {
        // (1 x)
        let expr = &SExpr::List(vec![SExpr::Number(42), SExpr::Symbol("x".to_string())]);

        assert_eq!(
            expr.try_into(),
            Ok(Ast::Call(
                Box::new(Ast::Const(Value::Int(42))),
                vec![Ast::Symbol("x".to_string()),],
            )),
        )
    }
}

#[cfg(test)]
mod arity_tests {
    use super::*;

    #[test]
    fn test_empty() {
        let params = vec![];

        assert_eq!(
            Params::from_raw_params(params),
            Params {
                required: vec![],
                ..Params::default()
            }
        )
    }

    #[test]
    fn test_required() {
        let params = vec!["a".to_string(), "b".to_string()];

        assert_eq!(
            Params::from_raw_params(params),
            Params {
                required: vec!["a".to_string(), "b".to_string()],
                ..Params::default()
            }
        )
    }

    #[test]
    fn test_optional_only() {
        let params = vec!["&opt".to_string(), "a".to_string(), "b".to_string()];

        assert_eq!(
            Params::from_raw_params(params),
            Params {
                required: vec![],
                optional: vec!["a".to_string(), "b".to_string()],
                ..Params::default()
            }
        )
    }

    #[test]
    fn test_required_optional() {
        let params = vec!["a".to_string(), "&opt".to_string(), "b".to_string()];

        assert_eq!(
            Params::from_raw_params(params),
            Params {
                required: vec!["a".to_string()],
                optional: vec!["b".to_string()],
                ..Params::default()
            }
        )
    }

    #[test]
    fn test_rest() {
        let params = vec!["&rest".to_string(), "a".to_string()];

        assert_eq!(
            Params::from_raw_params(params),
            Params {
                rest: Some("a".to_string()),
                ..Params::default()
            }
        )
    }

    #[test]
    fn test_required_rest() {
        let params = vec!["a".to_string(), "&rest".to_string(), "b".to_string()];

        assert_eq!(
            Params::from_raw_params(params),
            Params {
                required: vec!["a".to_string()],
                rest: Some("b".to_string()),
                ..Params::default()
            }
        )
    }

    #[test]
    fn test_required_optional_rest() {
        let params = vec![
            "a".to_string(),
            "b".to_string(),
            "&opt".to_string(),
            "c".to_string(),
            "d".to_string(),
            "&rest".to_string(),
            "e".to_string(),
        ];

        assert_eq!(
            Params::from_raw_params(params),
            Params {
                required: vec!["a".to_string(), "b".to_string()],
                optional: vec!["c".to_string(), "d".to_string()],
                rest: Some("e".to_string()),
            }
        )
    }
}
