use core::ast::Namespace;
use core::interpreter::eval;
use core::interpreter::Interpreter;
use core::vm::list::List;
use core::vm::value::Value;

#[test]
fn empty_expr() {
    assert_result("", Value::Nil);
}

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

#[test]
fn test_closure() {
    assert_result("(fn x { fn y { x } })(1)(2)", 1);
    assert_result("{ let x = 1; fn { x }}()", 1);
    assert_result("(fn x { fn y { y } })(1)(2)", 2);
    assert_result("{ let x = 1; fn y { y } }(2)", 2);
    assert_result("(fn x { fn y { x + y } })(1)(2)", 3);
}

#[test]
fn pipe_macro() {
    assert_result(
        "
let incr = fn x { x + 1 };
let double = fn x { x * 2 };
42 |> incr() |> double()
    ",
        (42 + 1) * 2,
    );
}

#[test]
fn toplevel_recursion() {
    assert_result(
        "
let to_zero = fn n {
    if n == 0 {
        0
    } else {
        to_zero(n - 1)
    }
};

to_zero(3)
    ",
        0,
    );
}

#[test]
fn complex_expr() {
    assert_result(
        "
let double = fn n { n * 2 };

double(1) + double(10) 
    ",
        2 + 20,
    );
}

#[test]
fn let_binding_err() {
    assert_err("let x = { x }; x");
}

#[test]
fn blank_identifiers_do_not_bind() {
    assert_err("let _ = nil; _");
    assert_err("{ let _ = nil; _ }");
    assert_err("(fn _ { _ })(nil)");

    assert_result("let second = fn _, x { x }; second(0, 1)", 1);
    assert_result("let second = fn x, _ { x }; second(0, 1)", 0);
}

#[test]
fn use_sugar() {
    let src = "
    let apply = fn x, f {
        f(x)
    };

    {
        use a <- apply(42);
        a * 2
    }
    ";

    assert_result(src, 42 * 2);
}

#[test]
fn modules_import() {
    let mod_a = "
    pub let x = 42;
    ";

    let mod_b_c = "
    import A;
    pub let x = A.x + 100;
    ";

    let src = "
    import B.C;
    import A as Z;
    Z.x + B.C.x + 1
    ";

    let mut interpreter = Interpreter::new();
    interpreter
        .add_module(Namespace(vec!["A".to_string()]), mod_a, false)
        .unwrap();

    interpreter
        .add_module(
            Namespace(vec!["B".to_string(), "C".to_string()]),
            mod_b_c,
            false,
        )
        .unwrap();

    let result = interpreter.run("test", src).unwrap();

    assert_eq!(result, (42.0 + 42.0 + 100.0 + 1.0).into())
}

#[test]
fn modules_import_self() {
    let mod_a = "
    pub let x = 42;
User.x
    ";

    let mut interpreter = Interpreter::new();

    let result = interpreter.run("test", mod_a).unwrap();

    assert_eq!(result, 42.0.into())
}

#[test]
fn native_fn() {
    fn sum(body: &[Value]) -> Result<Value, String> {
        match body {
            [Value::Num(a), Value::Num(b)] => Ok(Value::Num(a + b)),
            _ => Err("Invalid body".to_string()),
        }
    }

    let mut interpreter = Interpreter::new();

    let ns = Namespace(vec!["Basics".to_string()]);
    interpreter.define_native(&ns, "sum", 2, sum);
    interpreter.add_module(ns, "", false).unwrap();

    let value = interpreter
        .run("test", "import Basics; Basics.sum(10, 20)")
        .unwrap();

    assert_eq!(value, Value::Num(30.0))
}

#[test]
fn stdlib_test() {
    assert_result("import String; String.length(\"abc\")", 3.0);
}

#[test]
fn stdlib_test_handles_ds_file() {
    assert_result("import String; String.is_empty(\"\")", true);
}

#[test]
fn let_bug() {
    assert_result(
        "{let x = 0; x };  import String; String.length(\"abc\")",
        3.0,
    );
}

#[test]
fn list_empty() {
    assert_result("import List; List.empty()", Value::List(List::Empty));
}

#[test]
fn concat_native_calls() {
    assert_result(
        "import String; \"a\" |> String.concat(\"b\") |> String.concat(\"c\")",
        "abc",
    );
}

#[test]
fn concat_native_calls_list() {
    assert_result(
        "import List; List.cons(1, List.cons(2, List.cons(3, List.empty())))",
        Value::List(List::from_vec(vec![
            Value::Num(1.0),
            Value::Num(2.0),
            Value::Num(3.0),
        ])),
    );
}

#[test]
fn list_lit() {
    assert_result(
        "
import List;
[1, 2, 3]
",
        Value::List(List::from_vec(vec![
            Value::Num(1.0),
            Value::Num(2.0),
            Value::Num(3.0),
        ])),
    );
}

#[test]
fn list_range() {
    assert_result(
        "import List; List.range(1, 4)",
        Value::List(List::from_vec(vec![
            Value::Num(1.0),
            Value::Num(2.0),
            Value::Num(3.0),
        ])),
    );
}

#[test]
fn list_map() {
    assert_result(
        "import List;
let lst = List.cons(1, List.cons(2, List.cons(3, List.empty())));
List.map(lst, fn x { x * 10 })
",
        Value::List(List::from_vec(vec![
            Value::Num(10.0),
            Value::Num(20.0),
            Value::Num(30.0),
        ])),
    );
}

#[test]
fn list_filter() {
    assert_result(
        "import List;
let lst = List.cons(1, List.cons(0, List.cons(3, List.empty())));
List.filter(lst, fn x { x != 0 })
",
        Value::List(List::from_vec(vec![Value::Num(1.0), Value::Num(3.0)])),
    );
}

#[test]
fn list_foldr() {
    assert_result(
        "
import String;
import List;
let lst = List.cons(\"a\", List.cons(\"b\", List.cons(\"c\", List.empty())));
List.foldr(lst, \"Z\", String.concat)
",
        "abcZ",
    );
}

#[test]
fn list_foldl() {
    assert_result(
        "
import String;
import List;
let lst = List.cons(1, List.cons(2, List.cons(3, List.empty())));
List.foldl(lst, List.empty(), fn acc, x { List.cons(x, acc) })
",
        Value::List(List::from_vec(vec![
            Value::Num(3.0),
            Value::Num(2.0),
            Value::Num(1.0),
        ])),
    );
}

#[test]
fn empty_match() {
    assert_err(
        "
match 42 { }
",
    );
}

#[test]
fn const_match_good_path() {
    assert_result(
        "
match 42 { 42 => true }
",
        true,
    );
}

#[test]
fn const_match_bad_path() {
    assert_err(
        "
match 42 { 0 => true }
",
    );
}

#[test]
fn const_multiple_match_good_path() {
    assert_result(
        "
match 42 { 0 => true, 42 => false }
",
        false,
    );
}

#[test]
fn ident_match() {
    assert_result(
        "
match 42 { x => x + 1 }
",
        43,
    );
}

#[test]
fn empty_list_match_failure() {
    assert_result(
        "
match 42 { [] => 0, n => n }
",
        42.0,
    );
}

#[test]
fn cons_match() {
    assert_result(
        "
import List;
let lst = [1, 2, 3, 4];
match lst {
    [hd, 2, 3, .._] => hd
}
",
        1,
    );
}

#[test]
fn tuple_match() {
    assert_result(
        "
import Tuple;
match #(1, 2) {
    #(x, _) => x
}
",
        1,
    );
}

#[test]
fn map_lit() {
    assert_result(
        "
import Map;
#{ \"x\" => 42 }
",
        Value::Map(im_rc::hashmap![
            "x".to_string() => 42.0.into()
        ]),
    );
}

#[test]
fn map_lit_get() {
    assert_result(
        r#"
    import Map;
    Map.get(#{ "x" => 42 }, "x")
"#,
        Value::Num(42.0),
    );
}

#[test]
fn cons_map_match() {
    assert_result(
        "
let m = #{ \"x\" => 0, \"y\" => 1 };

match m {
    #{ \"x\" => 0, \"y\" => y } => y + 10
}
",
        Value::Num(11.0),
    );
}

#[test]
fn let_expr_pattern() {
    assert_result(
        "{ let #(x, _) = #(1, 2); x }
",
        Value::Num(1.0),
    );
}

#[test]
fn fizz_buzz() {
    assert_result(
        "
let fizz_buzz = fn n {
  match #(n % 3, n % 5) {
    #(0, 0) => \"FizzBuzz\",
    #(0, _) => \"Fizz\",
    #(_, 0) => \"Buzz\",
    _ => n,
  }
};

fizz_buzz(30)

    ",
        "FizzBuzz",
    );
}

pub fn assert_result<A>(src: &str, expected_value: A)
where
    A: Into<Value>,
{
    let value_result = eval("test", src);
    match value_result {
        Err(e) => panic!("{e:?}\nsrc:\n{src}"),
        Ok(value_result) => assert_eq!(value_result, expected_value.into(), "{src}"),
    };
}

pub fn assert_err(src: &str) {
    let res = eval("test", src);
    assert!(res.is_err(), "{:?}", src);
}
