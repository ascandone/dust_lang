use monkey_lang_rust_interpreter::compiler::compiler::Compiler;
use monkey_lang_rust_interpreter::parser::parse;
use monkey_lang_rust_interpreter::vm::value::Value;
use monkey_lang_rust_interpreter::vm::vm::Vm;
use std::rc::Rc;

#[test]
fn test_lit() {
    assert_result("42", Value::Int(42));
    assert_result("nil", Value::Nil);
    assert_result("true", Value::Bool(true));
    assert_result("false", Value::Bool(false));
    assert_result("\"abc\"", Value::String(Rc::new("abc".to_string())));
}

#[test]
fn test_expr() {
    assert_result("1 + 2", Value::Int(1 + 2));
    assert_result("1 + 2 * 4", Value::Int(1 + 2 * 4));
    assert_result("!(100 + 1 > 3)", Value::Bool(!(100 + 1 > 3)));
}

fn assert_result(src: &str, value: Value) {
    let program = match parse(src) {
        Ok(parsed) => parsed,
        Err(err) => {
            println!("Parsing error: {:?}", err);
            return;
        }
    };

    let mut compiler = Compiler::default();
    let mut vm = Vm::default();

    let compiled_fn = compiler.compile_program(program).unwrap();

    let value_result = vm
        .run_main(Rc::new(compiled_fn))
        .expect("Error during execution");

    assert_eq!(value, value_result);
}
