use crate::formatter::format;
use crate::parser::parse;

#[test]
fn expr_fmt() {
    assert_fmt("nil\n");
    assert_fmt("42\n"); // TODO fmt float
    assert_fmt("true\n");
    assert_fmt("false\n");
    assert_fmt("\"abc\"\n");
}

#[test]
fn expr_ident() {
    assert_fmt("x\n");
}

#[test]
fn if_ident() {
    assert_fmt(
        "if x {
  0
} else {
  1
}
",
    );

    let expr = "if cond {
  expr_a
} else {
  expr_b
}
";

    assert_fmt(expr);

    let expr = "if a {
  1
} else {
  2
}
";
    assert_fmt(expr);
}

#[test]
fn fn_expr() {
    assert_fmt("fn { nil }\n");
    assert_fmt("fn { 123456789 }\n");
    assert_fmt("fn a { nil }\n");
    assert_fmt("fn a, b { nil }\n");
    assert_fmt("fn a, b, c { nil }\n");
}

#[test]
fn fn_expr_wrap() {
    let expr = "fn {
  if cond {
    1
  } else {
    2
  }
}
";

    assert_fmt(expr);
}

#[test]
fn let_statement() {
    assert_fmt("let x = 42\n");
    assert_fmt("pub let x = 42\n");
    assert_fmt(
        "let x =
  if c {
    1
  } else {
    2
  }
",
    );
}

#[test]
fn call_expr() {
    assert_fmt("f()\n");
    assert_fmt("f(1)\n");
    assert_fmt("f(1, 2)\n");
    assert_fmt("f(1, 2, 3)\n");
    assert_fmt("f(x, fn { nil })\n");
}

#[test]
fn import_statement() {
    assert_fmt("import A\n");
    assert_fmt("import A.B\n");
    assert_fmt("import A.B as C\n");
}

#[test]
fn infix_expr() {
    assert_fmt("1 + 2\n");
    assert_fmt("1 + 2 + 3\n");
    assert_fmt("1 * 2 + 3\n");
    assert_fmt("1 + 2 * 3\n");
}

#[test]
fn prefix_expr() {
    assert_fmt("!true\n");
    assert_fmt("!!true\n");
    assert_fmt("!x\n");
    assert_fmt("!(x + y)\n");
    assert_fmt("!x + !y\n");
    assert_fmt("!x + !y\n");
    assert_fmt("!f()\n");
    assert_fmt("-2\n");
    assert_fmt("-(1 + 2)\n");
}

#[test]
fn nested_infix_expr_prec() {
    assert_fmt("1 * (2 + 3)\n");
}

#[test]
fn call_nested_prec() {
    assert_fmt("(1 + f)()\n");
    assert_fmt("(!f)()\n");
}

#[test]
fn do_block() {
    assert_fmt(
        "{
  a;
  b
}
",
    );

    assert_fmt(
        "{
  a;
  b
}
",
    );

    assert_fmt(
        "{
  a;
  b;
  c
}
",
    );

    assert_fmt(
        "fn {
  a;
  b;
  c
}
",
    );
}

#[test]
fn let_expr() {
    assert_fmt(
        "{
  let x = nil;
  x
}
",
    );

    assert_fmt(
        "{
  let x = 0;
  let y = 1;
  f(x + y)
}
",
    );

    assert_fmt(
        "fn {
  let x = 0;
  let y = 1;
  f(x + y)
}
",
    );
}

#[test]
fn use_expr() {
    assert_fmt(
        "{
  use <- expr();
  x
}
",
    );

    assert_fmt(
        "{
  use x <- expr(1, 2);
  x
}
",
    );

    assert_fmt(
        "{
  use x, y, z <- expr();
  x
}
",
    );

    assert_fmt(
        "fn {
  use x <- f();
  use y <- g();
  use z <- h();
  x + y + z
}
",
    );
}

#[test]
fn pipe_expr() {
    assert_fmt(
        "x
|> f(1, 2)
",
    );

    assert_fmt(
        "x
|> f(1, 2)
|> g()
|> h()
",
    );
}

#[test]
fn pipe_expr_prec() {
    assert_fmt(
        "1 + {
  2
  |> f(1, 2)
}
",
    );

    assert_fmt(
        "!{
  2
  |> f(1, 2)
}
",
    );
}

#[test]
fn pipe_inside_let() {
    assert_fmt(
        "let x =
  x
  |> f()
  |> g()
",
    );
}

#[test]
fn multiple_statements() {
    assert_fmt(
        "let x = 1;

import A;

let y = 2
",
    );
}

#[test]
fn list_lit() {
    assert_fmt(
        "[]
",
    );

    assert_fmt(
        "[1]
",
    );

    assert_fmt(
        "[1, 2, 3]
",
    );
}

#[test]
fn list_lit_cons() {
    assert_fmt(
        "[1, ..tl]
",
    );

    assert_fmt(
        "[1, 2, ..tl]
",
    );
}

#[test]
fn match_empty() {
    assert_fmt(
        "match x {}
",
    );
}

#[test]
fn match_const_clause() {
    assert_fmt(
        "match x {
  expr => value,
}
",
    );

    assert_fmt(
        "match x {
  42 => value,
}
",
    );

    assert_fmt(
        "match x {
  [] => value,
}
",
    );

    assert_fmt(
        "match x {
  [1, ..tl] => value,
}
",
    );
}

#[test]
fn match_many_clauses() {
    assert_fmt(
        "match x {
  0 => a,
  1 => b,
}
",
    );

    assert_fmt(
        "match x {
  0 => a,
  1 => b,
  2 => c,
}
",
    );
}

#[test]
fn match_list_sugar() {
    assert_fmt(
        "match x {
  [1, 2, 3, ..tl] => a,
}
",
    );
}

fn assert_fmt(expr: &str) {
    assert_eq!(&format(parse(expr).unwrap()), expr);
}
