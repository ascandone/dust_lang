use crate::{
    parser::{spanned_parser::Parser, Position},
    spanned_cst::{Expr, Program, Statement},
};

#[test]
fn parse_nil() {
    let (program, errs) = Parser::new("nil").parse_program();

    assert_eq!(errs, vec![]);
    assert_eq!(
        program,
        Program {
            statements: vec![(
                Position::new(0, 0),
                Statement::Expr(Expr::Nil),
                Position::new(0, 1),
            )]
        }
    );
}
