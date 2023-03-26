use crate::ast::{Declaration, Expr, Program, NIL};
use crate::lexer::Lexer;
use crate::token::Token;

#[derive(Debug)]
pub enum ParsingError {
    UnexpectedToken(Token),
}

pub fn parse(input: &str) -> Result<Program, ParsingError> {
    Parser::new(input).parse_program()
}

pub fn parse_expr(input: &str) -> Result<Expr, ParsingError> {
    Parser::new(input).parse_expr()
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        let mut parser = Self {
            lexer: Lexer::new(src),
            current_token: Token::Eof,
        };

        parser.advance_token();

        parser
    }

    fn advance_token(&mut self) {
        self.current_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Result<Program, ParsingError> {
        match self.current_token {
            Token::Let => Ok(vec![self.parse_let_decl()?]),
            _ => todo!("TODO: {:?}", self.current_token),
        }
    }

    pub fn parse_expr(&mut self) -> Result<Expr, ParsingError> {
        match self.current_token {
            Token::Nil => Ok(NIL),
            Token::True => Ok(true.into()),
            Token::False => Ok(false.into()),
            Token::Num(n) => Ok(n.into()),
            _ => Err(ParsingError::UnexpectedToken(self.current_token.clone())),
        }
    }

    /// Pre: let token has been encountered
    fn parse_let_decl(&mut self) -> Result<Declaration, ParsingError> {
        self.advance_token();
        let Token::Ident(ref name) = self.current_token.clone() else {
          return Err(ParsingError::UnexpectedToken(self.current_token.clone()))
        };

        self.advance_token();
        let Token::Assign = self.current_token.clone() else {
          return Err(ParsingError::UnexpectedToken(self.current_token.clone()))
        };

        self.advance_token();
        let value = self.parse_expr()?;

        Ok(Declaration::Let {
            name: name.clone(),
            value,
        })
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
        assert_eq!(parse_expr("42.0").unwrap(), 42.0.into());
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
}
