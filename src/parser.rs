use crate::ast::{Declaration, Expr, Program, NIL};
use crate::lexer::Lexer;
use crate::token::Token;

const LOWEST_PREC: u8 = 0;
const PREFIX_PREC: u8 = 15;

#[derive(Debug)]
pub enum ParsingError {
    UnexpectedToken(Token),
}

pub fn parse(input: &str) -> Result<Program, ParsingError> {
    Parser::new(input).parse_program()
}

pub fn parse_expr(input: &str) -> Result<Expr, ParsingError> {
    Parser::new(input).parse_expr(LOWEST_PREC)
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
}

fn token_to_pred(token: &Token) -> u8 {
    match token {
        Token::Eq | Token::NotEq => 8,
        Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => 9,
        Token::Plus | Token::Minus => 11,
        Token::Mult => 12,
        Token::LParen => 17,
        _ => 0,
    }
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        let mut parser = Self {
            lexer: Lexer::new(src),
            current_token: Token::Eof,
            peek_token: Token::Eof,
        };

        parser.advance_token();
        parser.advance_token();

        parser
    }

    fn advance_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Result<Program, ParsingError> {
        match self.current_token {
            Token::Let => Ok(vec![self.parse_let_decl()?]),
            _ => todo!("TODO: {:?}", self.current_token),
        }
    }

    pub fn parse_expr(&mut self, min_prec: u8) -> Result<Expr, ParsingError> {
        let mut left = match self.current_token {
            // Simple literals
            Token::Nil => Ok(NIL),
            Token::True => Ok(true.into()),
            Token::False => Ok(false.into()),
            Token::Num(n) => Ok(n.into()),
            Token::Ident(ref name) => Ok(Expr::Ident(name.clone())),

            // Prefix
            Token::Not => self.parse_prefix("!"),
            Token::Minus => self.parse_prefix("-"),

            // Complex expressions
            Token::LParen => self.parse_parens_expr(),
            Token::Fn => self.parse_fn_expr(),

            _ => Err(ParsingError::UnexpectedToken(self.current_token.clone())),
        }?;

        while min_prec < token_to_pred(&self.peek_token) {
            self.advance_token();
            left = match self.current_token {
                Token::Plus => self.parse_infix(left, "+")?,
                Token::Mult => self.parse_infix(left, "*")?,
                Token::Less => self.parse_infix(left, "<")?,
                Token::LessEqual => self.parse_infix(left, "<=")?,
                Token::Greater => self.parse_infix(left, ">")?,
                Token::GreaterEqual => self.parse_infix(left, ">=")?,
                Token::Eq => self.parse_infix(left, "==")?,
                Token::NotEq => self.parse_infix(left, "!=")?,

                Token::LParen => self.parse_call_expr(left)?,

                Token::Eof => break,
                _ => panic!("Expected an infix operator (got {:?})", self.current_token),
            };
        }

        Ok(left)
    }

    fn parse_infix(&mut self, left: Expr, operator: &str) -> Result<Expr, ParsingError> {
        let precedence = token_to_pred(&self.current_token);
        self.advance_token();

        let right = self.parse_expr(precedence)?;

        Ok(Expr::Infix(
            operator.to_string(),
            Box::new(left),
            Box::new(right),
        ))
    }

    fn parse_prefix(&mut self, operator: &str) -> Result<Expr, ParsingError> {
        self.advance_token();

        let expr = self.parse_expr(PREFIX_PREC)?;

        Ok(Expr::Prefix(operator.to_string(), Box::new(expr)))
    }

    fn parse_call_expr(&mut self, left: Expr) -> Result<Expr, ParsingError> {
        self.advance_token();

        let mut args = vec![];

        loop {
            match self.current_token {
                Token::RParen => break,
                Token::Comma => self.advance_token(),
                _ => {
                    let expr = self.parse_expr(LOWEST_PREC)?;
                    args.push(expr);
                    self.advance_token();
                }
            }
        }

        Ok(Expr::Call {
            f: Box::new(left),
            args,
        })
    }

    fn parse_parens_expr(&mut self) -> Result<Expr, ParsingError> {
        self.advance_token();
        let expr = self.parse_expr(LOWEST_PREC)?;

        self.advance_token();

        match &self.current_token {
            Token::RParen => Ok(expr),
            tk => Err(ParsingError::UnexpectedToken(tk.clone())),
        }
    }

    /// Pre: let token has been encountered
    fn parse_let_decl(&mut self) -> Result<Declaration, ParsingError> {
        self.advance_token();
        let Token::Ident(ref name) = self.current_token.clone() else {
          return Err(ParsingError::UnexpectedToken(self.current_token.clone()))
        };

        self.advance_token();
        let () = self.expect_token(Token::Assign)?;

        let value = self.parse_expr(LOWEST_PREC)?;

        Ok(Declaration::Let {
            name: name.clone(),
            value,
        })
    }

    fn parse_fn_expr(&mut self) -> Result<Expr, ParsingError> {
        let () = self.expect_token(Token::Fn)?;

        let mut params = vec![];

        loop {
            match &self.current_token {
                Token::Ident(name) => {
                    params.push(name.clone());
                    self.advance_token();
                }
                Token::Comma => {
                    self.advance_token();
                }
                Token::LBrace => {
                    self.advance_token();
                    break;
                }
                _ => return Err(ParsingError::UnexpectedToken(self.current_token.clone())),
            }
        }

        let expr = self.parse_expr(LOWEST_PREC)?;
        self.advance_token();

        let () = self.expect_token(Token::RBrace)?;

        Ok(Expr::Fn {
            params,
            body: Box::new(expr),
        })
    }

    fn expect_token(&mut self, expected_token: Token) -> Result<(), ParsingError> {
        if &self.current_token == &expected_token {
            self.advance_token();
            Ok(())
        } else {
            Err(ParsingError::UnexpectedToken(self.current_token.clone()))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::*;

    #[test]
    fn parse_nil() {
        assert_eq!(parse_expr("nil").unwrap(), NIL)
    }

    #[test]
    fn parse_bool() {
        assert_eq!(parse_expr("true").unwrap(), true.into());
        assert_eq!(parse_expr("false").unwrap(), false.into());
    }

    #[test]
    fn parse_num() {
        assert_eq!(parse_expr("0").unwrap(), 0.0.into());
        assert_eq!(parse_expr("10").unwrap(), 10.0.into());
    }

    #[test]
    fn parse_let_decl() {
        assert_eq!(
            parse("let x = nil").unwrap(),
            vec![Declaration::Let {
                name: "x".to_string(),
                value: NIL
            }]
        )
    }

    #[test]
    fn parse_prefix() {
        assert_eq!(
            parse_expr("! true").unwrap(),
            Expr::Prefix("!".to_string(), Box::new(true.into()))
        );

        assert_eq!(
            parse_expr("!true").unwrap(),
            Expr::Prefix("!".to_string(), Box::new(true.into()))
        );

        assert_eq!(
            parse_expr("!!true").unwrap(),
            Expr::Prefix(
                "!".to_string(),
                Box::new(Expr::Prefix("!".to_string(), Box::new(true.into()))),
            )
        );

        assert_eq!(
            parse_expr("- 1").unwrap(),
            Expr::Prefix("-".to_string(), Box::new(1.0.into()))
        );
    }

    #[test]
    fn parse_infix() {
        assert_eq!(
            parse_expr("1 + 2").unwrap(),
            Expr::Infix("+".to_string(), Box::new(1.0.into()), Box::new(2.0.into()))
        );
    }

    #[test]
    fn parse_infix_twice() {
        // (+ (* 1 2) 3)
        assert_eq!(
            parse_expr("1 * 2 + 3").unwrap(),
            Expr::Infix(
                "+".to_string(),
                Box::new(Expr::Infix(
                    "*".to_string(),
                    Box::new(1.0.into()),
                    Box::new(2.0.into())
                )),
                Box::new(3.0.into()),
            )
        );
    }

    #[test]
    fn parse_infix_mixed() {
        assert_eq!(
            parse_expr("a + b * Z + c").unwrap(),
            Expr::Infix(
                "+".to_string(),
                Box::new(Expr::Infix(
                    "+".to_string(),
                    Box::new(Expr::Ident("a".to_string())),
                    Box::new(Expr::Infix(
                        "*".to_string(),
                        Box::new(Expr::Ident("b".to_string())),
                        Box::new(Expr::Ident("Z".to_string())),
                    )),
                )),
                Box::new(Expr::Ident("c".to_string())),
            )
        );
    }

    #[test]
    fn parse_parenthesized_expr() {
        assert_eq!(
            parse_expr("(1 + 2) * 3").unwrap(),
            Expr::Infix(
                "*".to_string(),
                Box::new(Expr::Infix(
                    "+".to_string(),
                    Box::new(1.0.into()),
                    Box::new(2.0.into()),
                )),
                Box::new(3.0.into()),
            )
        );
    }

    #[test]
    fn parse_call_expr_no_args() {
        assert_eq!(
            parse_expr("f()").unwrap(),
            Expr::Call {
                f: Box::new(Expr::Ident("f".to_string())),
                args: vec![]
            }
        );
    }

    #[test]
    fn parse_call_expr_one_arg() {
        assert_eq!(
            parse_expr("f(42)").unwrap(),
            Expr::Call {
                f: Box::new(Expr::Ident("f".to_string())),
                args: vec![42.0.into()]
            }
        );
    }

    #[test]
    fn parse_call_expr_two_args() {
        assert_eq!(
            parse_expr("f(0, 1)").unwrap(),
            Expr::Call {
                f: Box::new(Expr::Ident("f".to_string())),
                args: vec![0.0.into(), 1.0.into()]
            }
        );
    }

    #[test]
    fn parse_fn_expr_no_params() {
        assert_eq!(
            parse_expr("fn { nil }").unwrap(),
            Expr::Fn {
                params: vec![],
                body: Box::new(NIL)
            }
        );
    }

    #[test]
    fn parse_fn_expr_one_params() {
        assert_eq!(
            parse_expr("fn x { nil }").unwrap(),
            Expr::Fn {
                params: vec!["x".to_string()],
                body: Box::new(NIL)
            }
        );
    }

    #[test]
    fn parse_fn_expr_two_params() {
        assert_eq!(
            parse_expr("fn x, y { nil }").unwrap(),
            Expr::Fn {
                params: vec!["x".to_string(), "y".to_string()],
                body: Box::new(NIL)
            }
        );
    }
}
