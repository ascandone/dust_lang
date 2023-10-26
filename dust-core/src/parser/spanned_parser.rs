use super::{
    spanned_lexer::{Lexer, LexerError, Spanned},
    token::Token,
};
use crate::spanned_cst::{Expr, ParsingError, Program, Statement};

pub struct Parser {
    errors: Vec<ParsingError>,
    iter: std::iter::Peekable<Lexer>,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        Self {
            errors: Vec::new(),
            iter: Lexer::new(input).peekable(),
        }
    }

    fn parse_expr(&mut self) -> Expr {
        let Some((st, tk_res, end)) = self.iter.next() else {
            return Expr::Error(ParsingError::UnexpectedEof);
        };

        todo!("expr")
    }

    pub fn parse_program(&mut self) -> (Program, Vec<ParsingError>) {
        let mut statements = vec![];

        if let Some((start, tk_res, end)) = self.iter.peek().cloned() {
            match tk_res {
                Err(_) => todo!(),
                Ok(tk) => match tk {
                    // TODO handle let
                    // TODO handle import

                    // else assuming it's an expr
                    _ => {
                        let e = self.parse_expr();
                        statements.push((start, Statement::Expr(e), end))
                    }
                },
            }
        }

        // TODO remove clone
        let errors = self.errors.clone();
        self.errors = vec![];
        (Program { statements }, errors)
    }
}
