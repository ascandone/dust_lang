use self::parser::ParsingError;
use crate::{
    ast::{Expr, Program},
    parser::parser::Parser,
};
mod lexer;
mod parser;
mod tests;
mod token;

pub fn parse(input: &str) -> Result<Program, ParsingError> {
    Parser::new(input).parse_program()
}

#[allow(dead_code)]
pub fn parse_expr(input: &str) -> Result<Expr, ParsingError> {
    Parser::new(input).parse_toplevel_expr()
}
