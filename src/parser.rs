use crate::ast::{Declaration, Expr, Program, NIL};
use crate::lexer::Lexer;
use crate::token::Token;

const LOWEST_PREC: u8 = 0;

#[derive(Debug)]
pub enum ParsingError {
    UnexpectedToken(Token, String),
}

pub fn parse(input: &str) -> Result<Program, ParsingError> {
    let mut parser = Parser::new(input);
    let program = parser.parse_program();
    assert_eq!(parser.current_token, Token::Eof);
    program
}

pub fn parse_expr(input: &str) -> Result<Expr, ParsingError> {
    let mut parser = Parser::new(input);
    let expr = parser.parse_expr(LOWEST_PREC, false);
    assert_eq!(parser.current_token, Token::Eof);
    expr
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

    fn consume_expr(&mut self, expr: Expr) -> Result<Expr, ParsingError> {
        self.advance_token();
        Ok(expr)
    }

    pub fn parse_expr(&mut self, min_prec: u8, inside_block: bool) -> Result<Expr, ParsingError> {
        let mut left = match self.current_token {
            // Simple literals
            Token::Nil => self.consume_expr(NIL),
            Token::True => self.consume_expr(true.into()),
            Token::False => self.consume_expr(false.into()),
            Token::Num(n) => self.consume_expr(n.into()),
            Token::Ident(ref name) => self.consume_expr(Expr::Ident(name.clone())),

            // Prefix
            Token::Not => self.parse_prefix("!"),
            Token::Minus => self.parse_prefix("-"),

            // Complex expressions
            Token::LParen => self.parse_parens_expr(),
            Token::Fn => self.parse_fn_expr(),
            Token::If => self.parse_if_expr(),
            Token::Let if inside_block => self.parse_let_expr(),

            Token::LBrace => self.parse_block_expr(),

            _ => Err(ParsingError::UnexpectedToken(
                self.current_token.clone(),
                "Expected an expression".to_string(),
            )),
        }?;

        while min_prec < token_to_pred(&self.current_token) {
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

        let right = self.parse_expr(precedence, false)?;

        Ok(Expr::Infix(
            operator.to_string(),
            Box::new(left),
            Box::new(right),
        ))
    }

    fn parse_prefix(&mut self, operator: &str) -> Result<Expr, ParsingError> {
        self.advance_token();

        let expr = self.parse_expr(15, false)?;

        Ok(Expr::Prefix(operator.to_string(), Box::new(expr)))
    }

    fn parse_call_expr(&mut self, left: Expr) -> Result<Expr, ParsingError> {
        self.advance_token();

        let mut args = vec![];

        loop {
            match self.current_token {
                Token::RParen => {
                    self.advance_token();
                    break;
                }
                Token::Comma => self.advance_token(),
                _ => {
                    let expr = self.parse_expr(LOWEST_PREC, false)?;
                    args.push(expr);
                }
            }
        }

        Ok(Expr::Call {
            f: Box::new(left),
            args,
        })
    }

    fn parse_parens_expr(&mut self) -> Result<Expr, ParsingError> {
        self.expect_token(Token::LParen)?;
        let expr = self.parse_expr(LOWEST_PREC, false)?;
        self.expect_token(Token::RParen)?;
        Ok(expr)
    }

    /// Pre: let token has been encountered
    fn parse_let_decl(&mut self) -> Result<Declaration, ParsingError> {
        self.advance_token();
        let Token::Ident(ref name) = self.current_token.clone() else {
          return Err(ParsingError::UnexpectedToken(self.current_token.clone(), "Expected a Ident token".to_string()))
        };

        self.advance_token();
        let () = self.expect_token(Token::Assign)?;

        let value = self.parse_expr(LOWEST_PREC, false)?;

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
                _ => {
                    return Err(ParsingError::UnexpectedToken(
                        self.current_token.clone(),
                        "Expected either a Comma, LBrace or Ident tokens".to_string(),
                    ))
                }
            }
        }

        let body = self.parse_expr(LOWEST_PREC, true)?;

        let () = self.expect_token(Token::RBrace)?;

        Ok(Expr::Fn {
            params,
            body: Box::new(body),
        })
    }

    fn expect_ident(&mut self) -> Result<String, ParsingError> {
        match &self.current_token {
            Token::Ident(name) => {
                let name = name.clone();
                self.advance_token();
                Ok(name)
            }
            _ => Err(ParsingError::UnexpectedToken(
                self.current_token.clone(),
                "Expected an Ident token".to_string(),
            )),
        }
    }

    fn parse_let_expr(&mut self) -> Result<Expr, ParsingError> {
        self.expect_token(Token::Let)?;
        let name = self.expect_ident()?;
        self.expect_token(Token::Assign)?;
        let value = self.parse_expr(LOWEST_PREC, false)?;
        self.expect_token(Token::Semicolon)?;
        let body = self.parse_expr(LOWEST_PREC, true)?;

        Ok(Expr::Let {
            name: name.clone(),
            value: Box::new(value),
            body: Box::new(body),
        })
    }

    fn parse_if_expr(&mut self) -> Result<Expr, ParsingError> {
        self.expect_token(Token::If)?;

        let condition = self.parse_expr(LOWEST_PREC, false)?;

        self.expect_token(Token::LBrace)?;

        let if_branch = self.parse_expr(LOWEST_PREC, true)?;

        self.expect_token(Token::RBrace)?;
        self.expect_token(Token::Else)?;
        self.expect_token(Token::LBrace)?;

        let else_branch = self.parse_expr(LOWEST_PREC, true)?;

        self.expect_token(Token::RBrace)?;
        Ok(Expr::If {
            condition: Box::new(condition),
            if_branch: Box::new(if_branch),
            else_branch: Box::new(else_branch),
        })
    }

    fn parse_block_expr(&mut self) -> Result<Expr, ParsingError> {
        self.expect_token(Token::LBrace)?;
        let expr = self.parse_expr(LOWEST_PREC, true)?;
        self.expect_token(Token::RBrace)?;
        Ok(expr)
    }

    fn expect_token(&mut self, expected_token: Token) -> Result<(), ParsingError> {
        if &self.current_token == &expected_token {
            self.advance_token();
            Ok(())
        } else {
            Err(ParsingError::UnexpectedToken(
                self.current_token.clone(),
                format!("Expected a {:?} token", expected_token),
            ))
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

    #[test]
    fn parse_if_expr() {
        assert_eq!(
            parse_expr("if true { 0 } else { 1 }").unwrap(),
            Expr::If {
                condition: Box::new(true.into()),
                if_branch: Box::new(0.0.into()),
                else_branch: Box::new(1.0.into())
            }
        );
    }

    #[test]
    fn parse_parenthesized_if_expr() {
        assert_eq!(
            parse_expr("(if true { 0 } else { 1 })").unwrap(),
            Expr::If {
                condition: Box::new(true.into()),
                if_branch: Box::new(0.0.into()),
                else_branch: Box::new(1.0.into())
            }
        );
    }

    #[test]
    fn parse_let_inside_if() {
        assert_eq!(
            parse_expr("if true { let x = 0; 100 } else { 1 }").unwrap(),
            Expr::If {
                condition: Box::new(true.into()),
                if_branch: Box::new(Expr::Let {
                    name: "x".to_string(),
                    value: Box::new(0.0.into()),
                    body: Box::new(100.0.into())
                }),
                else_branch: Box::new(1.0.into())
            }
        );
    }

    #[test]
    fn parse_nested_let_inside_if() {
        assert_eq!(
            parse_expr("if true { let x = 0; let y = 1; 100 } else { 1 }").unwrap(),
            Expr::If {
                condition: Box::new(true.into()),
                if_branch: Box::new(Expr::Let {
                    name: "x".to_string(),
                    value: Box::new(0.0.into()),
                    body: Box::new(Expr::Let {
                        name: "y".to_string(),
                        value: Box::new(1.0.into()),
                        body: Box::new(100.0.into())
                    })
                }),
                else_branch: Box::new(1.0.into())
            }
        );
    }

    #[test]
    fn parse_let_inside_fn_body() {
        assert_eq!(
            parse_expr("fn { let x = 0; 1 }").unwrap(),
            Expr::Fn {
                params: vec![],
                body: Box::new(Expr::Let {
                    name: "x".to_string(),
                    value: Box::new(0.0.into()),
                    body: Box::new(1.0.into())
                })
            }
        );
    }

    #[test]
    fn parse_block_expr() {
        assert_eq!(parse_expr("{ 42 }").unwrap(), 42.0.into());
    }

    #[test]
    fn parse_let_inside_block_expr() {
        assert_eq!(
            parse_expr("{ let x = 0; 1 }").unwrap(),
            Expr::Let {
                name: "x".to_string(),
                value: Box::new(0.0.into()),
                body: Box::new(1.0.into()),
            }
        );
    }
}
