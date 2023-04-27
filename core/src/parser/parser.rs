use super::{lexer::Lexer, token::Token};
use crate::ast::{Ident, Lit, Namespace, Pattern};
use crate::cst::{Expr, Import, Program, Statement, NIL};
use crate::parser::lexer::LexerError;
use std::fmt::{Display, Formatter};
use std::vec;

const LOWEST_PREC: u8 = 0;
const HIGHEST_PREC: u8 = 17;

#[derive(Debug)]
pub enum ParsingError {
    UnexpectedToken(Token, String),
    LexerError(LexerError),
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsingError::LexerError(e) => {
                write!(f, "{e}")
            }

            ParsingError::UnexpectedToken(got, expected) => {
                write!(f, "{expected} (got {got:?} instead)")
            }
        }
    }
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

        // TODO remove unwrap
        parser.advance_token().unwrap();
        parser.advance_token().unwrap();

        parser
    }

    fn advance_token(&mut self) -> Result<(), ParsingError> {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token().map_err(ParsingError::LexerError)?;
        Ok(())
    }

    #[cfg(test)]
    pub fn parse_toplevel_expr(&mut self) -> Result<Expr, ParsingError> {
        self.parse_expr(LOWEST_PREC, false)
    }

    pub fn parse_program(&mut self) -> Result<Program, ParsingError> {
        let mut statements = vec![];

        loop {
            match self.current_token {
                Token::Let => statements.push(self.parse_let_decl(false)?),
                Token::Pub => {
                    self.expect_token(Token::Pub)?;
                    statements.push(self.parse_let_decl(true)?)
                }
                Token::Import => statements.push(self.parse_import_statement()?),
                Token::Semicolon => self.advance_token()?,
                Token::Eof => return Ok(Program { statements }),

                // Assuming this is an expression otherwise
                _ => {
                    let expr = self.parse_expr(LOWEST_PREC, false)?;
                    statements.push(Statement::Expr(expr));
                }
            }
        }
    }

    fn consume_expr(&mut self, expr: Expr) -> Result<Expr, ParsingError> {
        self.advance_token()?;
        Ok(expr)
    }

    fn parse_expr(&mut self, min_prec: u8, inside_block: bool) -> Result<Expr, ParsingError> {
        let mut left = match self.current_token {
            // Simple literals
            Token::Nil => self.consume_expr(NIL),
            Token::True => self.consume_expr(true.into()),
            Token::False => self.consume_expr(false.into()),
            Token::Num(n) => self.consume_expr(n.into()),

            Token::Ident(ref name) => self.consume_expr(Expr::Ident(Ident(None, name.clone()))),
            Token::NsIndent(_) => self.parse_qualified_ident(),

            Token::String(ref str) => self.consume_expr(Expr::Lit(Lit::String(str.clone()))),

            // Prefix
            Token::Bang => self.parse_prefix("!"),
            Token::Minus => self.parse_prefix("-"),

            // Complex expressions
            Token::LParen => self.parse_parens_expr(),
            Token::Fn => self.parse_fn_expr(),
            Token::If => self.parse_if_expr(),
            Token::Match => self.parse_match_expr(),
            Token::Let if inside_block => self.parse_let_expr(),
            Token::Use if inside_block => self.parse_use_expr(),

            Token::LBrace => self.parse_block_expr(),
            Token::LBracket => self.parse_list_expr(),
            Token::HashLParen => self.parse_tuple_expr(),

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
                Token::PipeRight => self.parse_pipe_right(left, pred)?,
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

    fn parse_pipe_right(&mut self, left: Expr, precedence: u8) -> Result<Expr, ParsingError> {
        self.advance_token()?;
        let right = self.parse_expr(precedence, false)?;
        Ok(Expr::Pipe(Box::new(left), Box::new(right)))
    }

    fn parse_infix(
        &mut self,
        left: Expr,
        precedence: u8,
        operator: &str,
    ) -> Result<Expr, ParsingError> {
        self.advance_token()?;

        let right = self.parse_expr(precedence, false)?;

        Ok(Expr::Infix(
            operator.to_string(),
            Box::new(left),
            Box::new(right),
        ))
    }

    fn parse_prefix(&mut self, operator: &str) -> Result<Expr, ParsingError> {
        self.advance_token()?;

        let expr = self.parse_expr(15, false)?;

        Ok(Expr::Prefix(operator.to_string(), Box::new(expr)))
    }

    fn parse_call_expr(&mut self, left: Expr) -> Result<Expr, ParsingError> {
        self.advance_token()?;

        let args = self.sep_by_zero_or_more(Token::Comma, Token::RParen, |p| {
            p.parse_expr(LOWEST_PREC, false)
        })?;

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
    fn parse_let_decl(&mut self, public: bool) -> Result<Statement, ParsingError> {
        self.expect_token(Token::Let)?;
        let Token::Ident(ref name) = self.current_token.clone() else {
          return Err(ParsingError::UnexpectedToken(self.current_token.clone(), "Expected a Ident token".to_string()))
        };

        self.advance_token()?;
        self.expect_token(Token::Assign)?;

        let value = self.parse_expr(LOWEST_PREC, false)?;

        Ok(Statement::Let {
            public,
            name: name.clone(),
            value,
        })
    }

    fn parse_fn_expr(&mut self) -> Result<Expr, ParsingError> {
        self.expect_token(Token::Fn)?;

        let params = self.sep_by_zero_or_more(Token::Comma, Token::LBrace, Parser::expect_ident)?;

        let body = self.parse_expr(LOWEST_PREC, true)?;

        self.expect_token(Token::RBrace)?;

        Ok(Expr::Fn {
            params,
            body: Box::new(body),
        })
    }

    fn parse_qualified_ident(&mut self) -> Result<Expr, ParsingError> {
        let ns = self.parse_namespace()?;
        let ident = self.expect_ident()?;

        Ok(Expr::Ident(Ident(Some(ns), ident)))
    }

    fn expect_ident(&mut self) -> Result<String, ParsingError> {
        match &self.current_token {
            Token::Ident(name) => {
                let name = name.clone();
                self.advance_token()?;
                Ok(name)
            }
            _ => Err(ParsingError::UnexpectedToken(
                self.current_token.clone(),
                "Expected an Ident token".to_string(),
            )),
        }
    }

    fn expect_ns_ident(&mut self) -> Result<String, ParsingError> {
        match &self.current_token {
            Token::NsIndent(name) => {
                let name = name.clone();
                self.advance_token()?;
                Ok(name)
            }
            _ => Err(ParsingError::UnexpectedToken(
                self.current_token.clone(),
                "Expected an NsIndent token".to_string(),
            )),
        }
    }

    fn parse_use_expr(&mut self) -> Result<Expr, ParsingError> {
        self.expect_token(Token::Use)?;

        let bindings =
            self.sep_by_zero_or_more(Token::Comma, Token::ArrowLeft, Parser::expect_ident)?;

        let value = self.parse_expr(LOWEST_PREC, false)?;
        self.expect_token(Token::Semicolon)?;
        let body = self.parse_expr(LOWEST_PREC, true)?;

        Ok(Expr::Use {
            f_call: Box::new(value),
            params: bindings,
            body: Box::new(body),
        })
    }

    fn parse_let_expr(&mut self) -> Result<Expr, ParsingError> {
        self.expect_token(Token::Let)?;
        let name = self.expect_ident()?;
        self.expect_token(Token::Assign)?;
        let value = self.parse_expr(LOWEST_PREC, false)?;
        self.expect_token(Token::Semicolon)?;
        let body = self.parse_expr(LOWEST_PREC, true)?;

        Ok(Expr::Let {
            name,
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

        if self.maybe_token(Token::LBrace)? {
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

    fn parse_match_expr(&mut self) -> Result<Expr, ParsingError> {
        self.expect_token(Token::Match)?;
        let expr = self.parse_expr(LOWEST_PREC, false)?;
        self.expect_token(Token::LBrace)?;

        let clauses = self.sep_by_zero_or_more(Token::Comma, Token::RBrace, |p| {
            let pattern = p.parse_pattern()?;
            p.expect_token(Token::FatArrowRight)?;
            let expr = p.parse_expr(LOWEST_PREC, false)?;

            Ok((pattern, expr))
        })?;

        Ok(Expr::Match(Box::new(expr), clauses))
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParsingError> {
        match self.current_token {
            Token::Num(n) => {
                self.advance_token()?;
                Ok(Pattern::Lit(Lit::Num(n)))
            }

            Token::True => {
                self.advance_token()?;
                Ok(Pattern::Lit(Lit::Bool(true)))
            }

            Token::False => {
                self.advance_token()?;
                Ok(Pattern::Lit(Lit::Bool(false)))
            }

            Token::Nil => {
                self.advance_token()?;
                Ok(Pattern::Lit(Lit::Nil))
            }

            Token::String(ref s) => {
                let s = s.to_string();
                self.advance_token()?;
                Ok(Pattern::Lit(Lit::String(s)))
            }

            Token::Ident(ref s) => {
                let s = s.to_string();
                self.advance_token()?;
                Ok(Pattern::Identifier(s))
            }

            Token::HashLParen => {
                self.advance_token()?;
                let patterns =
                    self.sep_by_zero_or_more(Token::Comma, Token::RParen, |p| p.parse_pattern())?;

                Ok(Pattern::Tuple(patterns))
            }

            Token::LBracket => {
                self.advance_token()?;

                let (patterns, end_tk) = self.sep_by_zero_or_more_multiple_ends(
                    Token::Comma,
                    vec![Token::RBracket, Token::Dots],
                    |p| p.parse_pattern(),
                )?;

                let tl = match end_tk {
                    Token::Dots if !patterns.is_empty() => {
                        let pat = self.parse_pattern()?;
                        self.expect_token(Token::RBracket)?;
                        pat
                    }

                    Token::Dots => {
                        return Err(ParsingError::UnexpectedToken(
                            self.current_token.clone(),
                            format!("Expected a {:?} token", Token::RBracket),
                        ))
                    }

                    Token::RBracket => Pattern::EmptyList,

                    _ => panic!("Invalid parser state"),
                };

                let pattern = patterns
                    .into_iter()
                    .rfold(tl, |acc, expr| Pattern::Cons(Box::new(expr), Box::new(acc)));

                Ok(pattern)
            }

            ref tk => Err(ParsingError::UnexpectedToken(
                tk.clone(),
                "Expected a pattern".to_string(),
            )),
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

    fn parse_tuple_expr(&mut self) -> Result<Expr, ParsingError> {
        self.expect_token(Token::HashLParen)?;
        let exprs = self.sep_by_zero_or_more(Token::Comma, Token::RParen, |p| {
            p.parse_expr(LOWEST_PREC, false)
        })?;
        Ok(Expr::Tuple(exprs))
    }

    fn parse_list_expr(&mut self) -> Result<Expr, ParsingError> {
        self.expect_token(Token::LBracket)?;

        let (exprs, end_tk) = self.sep_by_zero_or_more_multiple_ends(
            Token::Comma,
            vec![Token::RBracket, Token::Dots],
            |p| p.parse_expr(LOWEST_PREC, false),
        )?;

        let tl = match end_tk {
            Token::Dots if !exprs.is_empty() => {
                let tl = self.parse_expr(LOWEST_PREC, false)?;
                self.expect_token(Token::RBracket)?;
                tl
            }

            Token::Dots => {
                return Err(ParsingError::UnexpectedToken(
                    self.current_token.clone(),
                    format!("Expected a {:?} token", Token::RBracket),
                ))
            }

            Token::RBracket => Expr::EmptyList,

            _ => panic!("Invalid parser state"),
        };

        let list_lit = exprs
            .into_iter()
            .rfold(tl, |acc, expr| Expr::Cons(Box::new(expr), Box::new(acc)));

        Ok(list_lit)
    }

    fn expect_token(&mut self, expected_token: Token) -> Result<(), ParsingError> {
        if self.current_token == expected_token {
            self.advance_token()?;
            Ok(())
        } else {
            Err(ParsingError::UnexpectedToken(
                self.current_token.clone(),
                format!("Expected a {:?} token", expected_token),
            ))
        }
    }

    fn maybe_token(&mut self, expected_token: Token) -> Result<bool, ParsingError> {
        if self.current_token == expected_token {
            self.advance_token()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn parse_import_statement(&mut self) -> Result<Statement, ParsingError> {
        self.expect_token(Token::Import)?;
        let ns = self.parse_namespace()?;

        let rename = if self.maybe_token(Token::As)? {
            // only a single-ident ns is allowed
            let ident = self.expect_ns_ident()?;
            Some(Namespace(vec![ident]))
        } else {
            None
        };

        Ok(Statement::Import(Import { ns, rename }))
    }

    fn parse_namespace(&mut self) -> Result<Namespace, ParsingError> {
        let first = self.expect_ns_ident()?;

        let mut ns = vec![first];

        loop {
            match &self.current_token {
                Token::NsIndent(id) => ns.push(id.clone()),
                Token::Dot => {}
                _ => break,
            }

            self.advance_token()?;
        }

        Ok(Namespace(ns))
    }

    fn sep_by_zero_or_more<T, F>(
        &mut self,
        separator: Token,
        end: Token,
        f: F,
    ) -> Result<Vec<T>, ParsingError>
    where
        F: Fn(&mut Self) -> Result<T, ParsingError>,
    {
        let (r, _) = self.sep_by_zero_or_more_multiple_ends(separator, vec![end], f)?;
        Ok(r)
    }

    fn sep_by_zero_or_more_multiple_ends<T, F>(
        &mut self,
        separator: Token,
        end: Vec<Token>,
        f: F,
    ) -> Result<(Vec<T>, Token), ParsingError>
    where
        F: Fn(&mut Self) -> Result<T, ParsingError>,
    {
        let mut exprs = vec![];

        loop {
            if self.current_token == separator {
                self.advance_token()?;
            }

            if end.contains(&self.current_token) {
                let end_tk = self.current_token.clone();
                self.advance_token()?;
                return Ok((exprs, end_tk));
            }

            let expr = f(self)?;
            exprs.push(expr);
        }
    }
}
