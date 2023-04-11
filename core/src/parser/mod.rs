use crate::ast;
use crate::cst::{try_from_program, Program};
use crate::parser::parser::Parser;
pub use crate::parser::parser::ParsingError;

mod lexer;
mod parser;
#[cfg(test)]
mod parser_tests;
mod token;

pub fn parse(input: &str) -> Result<Program, ParsingError> {
    Parser::new(input).parse_program()
}

pub fn parse_ast(input: &str) -> Result<ast::Program, ParsingError> {
    parse(input).map(|p| try_from_program(p).unwrap())
}

#[cfg(test)]
pub fn parse_expr(input: &str) -> Result<crate::cst::Expr, ParsingError> {
    Parser::new(input).parse_toplevel_expr()
}

#[cfg(test)]
pub fn parse_expr_ast(input: &str) -> Result<ast::Expr, ParsingError> {
    parse_expr(input).map(|p| p.try_into().unwrap())
}
