pub use crate::parser::parser::ParsingError;
use crate::{ast::Program, parser::parser::Parser};
mod lexer;
mod parser;
mod tests;
mod token;

pub fn parse(input: &str) -> Result<Program, ParsingError> {
    Parser::new(input).parse_program()
}

#[cfg(test)]
pub fn parse_expr(input: &str) -> Result<crate::ast::Expr, ParsingError> {
    Parser::new(input).parse_toplevel_expr()
}
