use monkey_lang_rust_interpreter::compiler::compiler::Compiler;
use monkey_lang_rust_interpreter::parser::parse;
use monkey_lang_rust_interpreter::vm::value::Value;
use monkey_lang_rust_interpreter::vm::vm::Vm;
use std::rc::Rc;

#[test]
fn test_lit() {
    assert_result("42", 42.0);
    assert_result("nil", Value::Nil);
    assert_result("true", true);
    assert_result("false", false);
    assert_result("\"abc\"", "abc");
}

#[test]
fn test_expr() {
    assert_result("1 + 2", 1 + 2);
    assert_result("10 - 1", 10 - 1);
    assert_result("10 * 2", 10 * 2);
    assert_result("10 / 2", 10 / 2);
    assert_result("10 % 3", 10 % 3);

    assert_result("10 < 10", 10 < 10);
    assert_result("10 < 3", 10 < 3);
    assert_result("10 < 30", 10 < 30);

    assert_result("10 <= 10", 10 <= 10);
    assert_result("10 <= 3", 10 <= 3);
    assert_result("10 <= 30", 10 <= 30);

    assert_result("10 > 10", 10 > 10);
    assert_result("10 > 3", 10 > 3);
    assert_result("10 > 30", 10 > 30);

    assert_result("10 >= 10", 10 >= 10);
    assert_result("10 >= 3", 10 >= 3);
    assert_result("10 >= 30", 10 >= 30);

    assert_result("10 == 10", 10 == 10);
    assert_result("10 == 3", 10 == 3);
    assert_result("10 == 30", 10 == 30);

    assert_result("!true", !true);
    assert_result("!false", !false);

    assert_result("-42", -42);
}

#[test]
fn test_expr_prec() {
    assert_result("1 + 2 * 4", 1 + 2 * 4);
    assert_result("!(100 + 1 > 3)", !(100 + 1 > 3));
}

#[test]
fn test_and() {
    assert_result("true && true", true);
    assert_result("true && false", false);
    assert_result("false && true", false);
    assert_result("false && false", false);
}

#[test]
fn test_or() {
    assert_result("true || true", true);
    assert_result("true || false", true);
    assert_result("false || true", true);
    assert_result("false || false", false);
}

#[test]
fn test_let_statement() {
    assert_result("let x = 42; x", 42);
    assert_result("let z = 0; let x = 42; x", 42);
    assert_result("let z = 0; let x = 42; z", 0);
    assert_result("let x = 0; let x = 42; x", 42);
}

#[test]
fn test_let_expr() {
    assert_result("{let x = 42; x}", 42);
    assert_result("{let z = 0; let x = 42; x}", 42);
    assert_result("{let z = 0; let x = 42; z}", 0);
    assert_result("{let x = 0; let x = 42; x}", 42);
    assert_result("{let x = 1; let y = 2; x + y}", 3);
}

#[test]
fn test_call_no_args() {
    assert_result("(fn { 42 })()", 42);
}

#[test]
fn test_call_close() {
    assert_result("(fn x { x })(42)", 42);
    assert_result("(fn x, y { x })(0, 1)", 0);
    assert_result("(fn x, y { y })(0, 1)", 1);
    assert_result("(fn x, x { x })(0, 1)", 1);
}

#[test]
fn test_call_with_let() {
    assert_result("(fn arg { let x = 0; x })(1)", 0);
    assert_result("(fn arg { let x = arg; x })(1)", 1);
}

#[ignore]
#[test]
fn test_closure() {
    assert_result("(fn x { fn y { x } })(1)(2)", 1);
    assert_result("(fn x { fn y { y } })(1)(2)", 1);
    assert_result("(fn x { fn y { x + y } })(1)(2)", 3);
}

fn assert_result<A>(src: &str, expected_value: A)
where
    A: Into<Value>,
{
    let program = parse(src).unwrap();
    let mut compiler = Compiler::default();
    let mut vm = Vm::default();

    let compiled_fn = compiler.compile_program(program).unwrap();

    let value_result = vm
        .run_main(Rc::new(compiled_fn))
        .expect("Error during execution");

    assert_eq!(value_result, expected_value.into(), "{:?}", src);
}
