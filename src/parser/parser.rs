use super::{lexer::Lexer, token::Token};
use crate::ast::{Expr, Lit, Program, Statement, NIL};

const LOWEST_PREC: u8 = 0;
const HIGHEST_PREC: u8 = 17;

#[derive(Debug)]
pub enum ParsingError {
    UnexpectedToken(Token, String),
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
}

fn token_to_pred(token: &Token, inside_block: bool) -> u8 {
    match token {
        Token::Semicolon if inside_block => 1,
        Token::DoublePipe => 3,
        Token::DoubleAnd => 4,
        Token::PipeRight => 8,
        Token::Eq | Token::NotEq => 8,
        Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => 9,
        Token::Plus | Token::Minus => 11,
        Token::Mult | Token::Slash | Token::Percentage => 12,
        Token::LParen => HIGHEST_PREC,
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

    #[allow(dead_code)]
    pub fn parse_toplevel_expr(&mut self) -> Result<Expr, ParsingError> {
        self.parse_expr(LOWEST_PREC, false)
    }

    pub fn parse_program(&mut self) -> Result<Program, ParsingError> {
        let mut statements = vec![];

        loop {
            match self.current_token {
                Token::Let => statements.push(self.parse_let_decl()?),
                Token::Semicolon => self.advance_token(),
                Token::Eof => return Ok(statements),

                // Assuming this is an expression otherwise
                _ => {
                    let expr = self.parse_expr(LOWEST_PREC, false)?;
                    statements.push(Statement::Expr(expr));
                }
            }
        }
    }

    fn consume_expr(&mut self, expr: Expr) -> Result<Expr, ParsingError> {
        self.advance_token();
        Ok(expr)
    }

    fn parse_expr(&mut self, min_prec: u8, inside_block: bool) -> Result<Expr, ParsingError> {
        let mut left = match self.current_token {
            // Simple literals
            Token::Nil => self.consume_expr(NIL),
            Token::True => self.consume_expr(true.into()),
            Token::False => self.consume_expr(false.into()),
            Token::Num(n) => self.consume_expr(n.into()),
            Token::Ident(ref name) => self.consume_expr(Expr::Ident(name.clone())),
            Token::String(ref str) => self.consume_expr(Expr::Lit(Lit::String(str.clone()))),

            // Prefix
            Token::Bang => self.parse_prefix("!"),
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

        loop {
            let pred = token_to_pred(&self.current_token, inside_block);
            if min_prec >= pred {
                break;
            }

            left = match self.current_token {
                Token::PipeRight => self.parse_infix(left, pred, "|>")?,
                Token::Plus => self.parse_infix(left, pred, "+")?,
                Token::Minus => self.parse_infix(left, pred, "-")?,
                Token::Slash => self.parse_infix(left, pred, "/")?,
                Token::Mult => self.parse_infix(left, pred, "*")?,
                Token::Less => self.parse_infix(left, pred, "<")?,
                Token::LessEqual => self.parse_infix(left, pred, "<=")?,
                Token::Greater => self.parse_infix(left, pred, ">")?,
                Token::GreaterEqual => self.parse_infix(left, pred, ">=")?,
                Token::Eq => self.parse_infix(left, pred, "==")?,
                Token::NotEq => self.parse_infix(left, pred, "!=")?,
                Token::DoubleAnd => self.parse_infix(left, pred, "&&")?,
                Token::DoublePipe => self.parse_infix(left, pred, "||")?,
                Token::Percentage => self.parse_infix(left, pred, "%")?,

                Token::LParen => self.parse_call_expr(left)?,

                Token::Semicolon if inside_block => self.parse_do_expr(left)?,

                Token::Eof => break,
                _ => {
                    return Err(ParsingError::UnexpectedToken(
                        self.current_token.clone(),
                        "Expected an infix operator".to_string(),
                    ))
                }
            };
        }

        Ok(left)
    }

    fn parse_infix(
        &mut self,
        left: Expr,
        precedence: u8,
        operator: &str,
    ) -> Result<Expr, ParsingError> {
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
    fn parse_let_decl(&mut self) -> Result<Statement, ParsingError> {
        self.advance_token();
        let Token::Ident(ref name) = self.current_token.clone() else {
          return Err(ParsingError::UnexpectedToken(self.current_token.clone(), "Expected a Ident token".to_string()))
        };

        self.advance_token();
        let () = self.expect_token(Token::Assign)?;

        let value = self.parse_expr(LOWEST_PREC, false)?;

        Ok(Statement::Let {
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
        let value = self.parse_expr(HIGHEST_PREC, false)?;
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

        if self.maybe_token(Token::LBrace) {
            let else_branch = self.parse_expr(LOWEST_PREC, true)?;

            self.expect_token(Token::RBrace)?;
            Ok(Expr::If {
                condition: Box::new(condition),
                if_branch: Box::new(if_branch),
                else_branch: Box::new(else_branch),
            })
        } else {
            let nested_expr = self.parse_if_expr()?;

            Ok(Expr::If {
                condition: Box::new(condition),
                if_branch: Box::new(if_branch),
                else_branch: Box::new(nested_expr),
            })
        }
    }

    fn parse_block_expr(&mut self) -> Result<Expr, ParsingError> {
        self.expect_token(Token::LBrace)?;
        let expr = self.parse_expr(LOWEST_PREC, true)?;
        self.expect_token(Token::RBrace)?;
        Ok(expr)
    }

    fn parse_do_expr(&mut self, left: Expr) -> Result<Expr, ParsingError> {
        self.expect_token(Token::Semicolon)?;
        let right = self.parse_expr(LOWEST_PREC, true)?;
        Ok(Expr::Do(Box::new(left), Box::new(right)))
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

    fn maybe_token(&mut self, expected_token: Token) -> bool {
        if &self.current_token == &expected_token {
            self.advance_token();
            true
        } else {
            false
        }
    }
}
