use crate::vm::list::List;
use crate::vm::value::NativeFunction;
use crate::vm::{
    bytecode::OpCode,
    value::{Closure, Function, Value},
    vm::Vm,
};
use std::rc::Rc;

#[test]
fn test_const() {
    let main = Function {
        constant_pool: vec![Value::Num(42.0)],
        bytecode: vec![OpCode::Const as u8, 0, OpCode::Return as u8],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Num(42.0)
    )
}

#[test]
fn test_const_nil() {
    let main = Function {
        bytecode: vec![OpCode::ConstNil as u8, OpCode::Return as u8],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), Value::Nil)
}

#[test]
fn test_const_true() {
    let main = Function {
        bytecode: vec![OpCode::ConstTrue as u8, OpCode::Return as u8],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Bool(true)
    )
}

#[test]
fn test_const_false() {
    let main = Function {
        bytecode: vec![OpCode::ConstFalse as u8, OpCode::Return as u8],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Bool(false)
    )
}

#[test]
fn test_pop() {
    let main = Function {
        constant_pool: vec![Value::Num(10.0), Value::Num(20.0)],
        bytecode: vec![
            OpCode::Const as u8,
            0,
            OpCode::Const as u8,
            1,
            OpCode::Pop as u8,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Num(10.0)
    )
}

#[test]
fn test_add_nums() {
    let main = Function {
        constant_pool: vec![Value::Num(10.0), Value::Num(20.0)],
        bytecode: vec![
            OpCode::Const as u8,
            0,
            OpCode::Const as u8,
            1,
            OpCode::Add as u8,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Num(30.0)
    )
}

#[test]
fn test_add_strings() {
    let s1 = Rc::new("abc".to_string());
    let s2 = Rc::new("def".to_string());

    let main = Function {
        constant_pool: vec![Value::String(s1), Value::String(s2)],
        bytecode: vec![
            OpCode::Const as u8,
            0,
            OpCode::Const as u8,
            1,
            OpCode::Add as u8,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::String(Rc::new("abcdef".to_string()))
    )
}

#[test]
fn gt_10_20() {
    let main = Function {
        constant_pool: vec![Value::Num(10.0), Value::Num(20.0)],
        bytecode: vec![
            OpCode::Const as u8,
            0,
            OpCode::Const as u8,
            1,
            OpCode::Gt as u8,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Bool(false)
    );
}

#[test]
fn gt_20_10() {
    let main = Function {
        constant_pool: vec![Value::Num(10.0), Value::Num(20.0)],
        bytecode: vec![
            OpCode::Const as u8,
            1,
            OpCode::Const as u8,
            0,
            OpCode::Gt as u8,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Bool(true)
    );
}

#[test]
fn test_jump() {
    let main = Function {
        bytecode: vec![
            /* 0 */ OpCode::Jump as u8,
            /* 1 */ 0,
            /* 2 */ 5,
            /* 3 */ OpCode::ConstNil as u8,
            /* 4 */ OpCode::Return as u8,
            /* 5 */ OpCode::ConstTrue as u8, // <-
            /* 6 */ OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Bool(true)
    )
}

#[test]
fn test_jump_if_not_when_true() {
    let main = Function {
        bytecode: vec![
            /* 0 */ OpCode::ConstTrue as u8,
            /* 1 */ OpCode::JumpIfFalse as u8,
            /* 2 */ 0,
            /* 3 */ 6,
            /* 4 */ OpCode::ConstNil as u8,
            /* 5 */ OpCode::Return as u8,
            /* 6 */ OpCode::ConstTrue as u8, // <-
            /* 7 */ OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), Value::Nil)
}

#[test]
fn test_jump_if_not_when_false() {
    let main = Function {
        bytecode: vec![
            /* 0 */ OpCode::ConstFalse as u8,
            /* 1 */ OpCode::JumpIfFalse as u8,
            /* 2 */ 0,
            /* 3 */ 6,
            /* 4 */ OpCode::ConstNil as u8,
            /* 5 */ OpCode::Return as u8,
            /* 6 */ OpCode::ConstTrue as u8, // <-
            /* 7 */ OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Bool(true)
    )
}

#[test]
fn test_jump_if_false_else_pop_when_false() {
    let main = Function {
        bytecode: vec![
            /* 0 */ OpCode::ConstFalse as u8,
            /* 1 */ OpCode::JumpIfFalseElsePop as u8,
            /* 2 */ 0,
            /* 3 */ 6,
            /* 4 */ OpCode::ConstNil as u8,
            /* 5 */ OpCode::Return as u8,
            /* 6 */ OpCode::Return as u8, // <-
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Bool(false)
    )
}

#[test]
fn test_jump_if_false_else_pop_when_true() {
    let main = Function {
        bytecode: vec![
            /* 0 */ OpCode::ConstTrue as u8,
            /* 1 */ OpCode::JumpIfFalseElsePop as u8,
            /* 2 */ 0,
            /* 3 */ 6,
            /* 4 */ OpCode::ConstNil as u8,
            /* 5 */ OpCode::Return as u8,
            /* 6 */ OpCode::Return as u8, // <-
        ],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), Value::Nil)
}

#[test]
fn test_jump_if_true_else_pop_when_false() {
    let main = Function {
        bytecode: vec![
            /* 0 */ OpCode::ConstFalse as u8,
            /* 1 */ OpCode::JumpIfTrueElsePop as u8,
            /* 2 */ 0,
            /* 3 */ 6,
            /* 4 */ OpCode::ConstNil as u8,
            /* 5 */ OpCode::Return as u8,
            /* 6 */ OpCode::Return as u8, // <-
        ],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), Value::Nil)
}

#[test]
fn test_jump_if_true_else_pop_when_true() {
    let main = Function {
        bytecode: vec![
            /* 0 */ OpCode::ConstTrue as u8,
            /* 1 */ OpCode::JumpIfTrueElsePop as u8,
            /* 2 */ 0,
            /* 3 */ 6,
            /* 4 */ OpCode::ConstNil as u8,
            /* 5 */ OpCode::Return as u8,
            /* 6 */ OpCode::Return as u8, // <-
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Bool(true)
    )
}

#[test]
fn test_write_read_globals() {
    let main = Function {
        bytecode: vec![
            OpCode::ConstTrue as u8,
            OpCode::SetGlobal as u8,
            0,
            0,
            OpCode::ConstNil as u8,
            OpCode::GetGlobal as u8,
            0,
            0,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Bool(true)
    )
}

#[test]
fn test_write_read_locals() {
    let main = Function {
        locals: 1,
        bytecode: vec![
            OpCode::ConstTrue as u8,
            OpCode::SetLocal as u8,
            0,
            OpCode::ConstNil as u8,
            OpCode::GetLocal as u8,
            0,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Bool(true)
    )
}

#[test]
fn test_call_0_args() {
    let always_42_fn = Function {
        constant_pool: vec![Value::Num(42.0)],
        bytecode: vec![OpCode::Const as u8, 0, OpCode::Return as u8],
        ..Default::default()
    };

    let main = Function {
        constant_pool: vec![Value::Function(Rc::new(always_42_fn))],
        bytecode: vec![
            OpCode::Const as u8,
            0,
            OpCode::Call as u8,
            0,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Num(42.0)
    )
}

#[test]
fn test_call_1_arg() {
    let f = Function {
        arity: 1,
        constant_pool: vec![Value::Num(1.0)],
        bytecode: vec![
            OpCode::GetLocal as u8,
            0,
            OpCode::Const as u8,
            0,
            OpCode::Add as u8,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    let main = Function {
        constant_pool: vec![Value::Num(42.0), Value::Function(Rc::new(f))],
        bytecode: vec![
            OpCode::Const as u8,
            0,
            OpCode::Const as u8,
            1,
            OpCode::Call as u8,
            1,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Num(43.0)
    )
}

#[test]
fn test_call_2_args() {
    let f = Function {
        arity: 2,
        bytecode: vec![
            OpCode::GetLocal as u8,
            0,
            OpCode::GetLocal as u8,
            1,
            OpCode::Add as u8,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    let main = Function {
        constant_pool: vec![
            Value::Num(10.0),
            Value::Num(20.0),
            Value::Function(Rc::new(f)),
        ],
        bytecode: vec![
            OpCode::Const as u8,
            0,
            OpCode::Const as u8,
            1,
            OpCode::Const as u8,
            2,
            OpCode::Call as u8,
            2,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Num(30.0)
    )
}

#[test]
fn test_call_nested() {
    // ((\x y.y) true) false
    // ?=> false

    let nested = Value::Function(Rc::new(Function {
        arity: 1,
        bytecode: vec![OpCode::GetLocal as u8, 0, OpCode::Return as u8],
        ..Default::default()
    }));

    let f = Function {
        arity: 1,
        constant_pool: vec![nested],
        bytecode: vec![OpCode::Const as u8, 0, OpCode::Return as u8],
        ..Default::default()
    };

    let main = Function {
        constant_pool: vec![Value::Function(Rc::new(f))],
        bytecode: vec![
            OpCode::ConstFalse as u8,
            OpCode::ConstTrue as u8,
            OpCode::Const as u8,
            0,
            OpCode::Call as u8,
            1,
            OpCode::Call as u8,
            1,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Bool(false)
    )
}

#[test]
fn call_1_arg_and_return_local_test() {
    // fn(x) { let y = true; y }
    let f = Function {
        arity: 1,
        locals: 1,
        bytecode: vec![
            OpCode::ConstTrue as u8,
            OpCode::SetLocal as u8,
            1,
            OpCode::GetLocal as u8,
            1,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    // f(false)
    let main = Function {
        constant_pool: vec![Value::Function(Rc::new(f))],
        bytecode: vec![
            OpCode::ConstFalse as u8,
            OpCode::Const as u8,
            0,
            OpCode::Call as u8,
            1,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Bool(true)
    )
}

#[test]
fn call_1_arg_and_return_arg_test() {
    // fn(x) { let y = true; x }
    let f = Function {
        arity: 1,
        locals: 1,
        bytecode: vec![
            OpCode::ConstTrue as u8,
            OpCode::SetLocal as u8,
            1,
            OpCode::GetLocal as u8,
            0,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    // f(false)
    let main = Function {
        constant_pool: vec![Value::Function(Rc::new(f))],
        bytecode: vec![
            OpCode::ConstFalse as u8,
            OpCode::Const as u8,
            0,
            OpCode::Call as u8,
            1,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Bool(false)
    )
}

#[test]
fn test_call_not_enough_required_args() {
    let f = Function {
        arity: 2,
        bytecode: vec![OpCode::ConstNil as u8, OpCode::Return as u8],
        ..Default::default()
    };

    let main = Function {
        constant_pool: vec![Value::Function(Rc::new(f))],
        bytecode: vec![
            OpCode::ConstTrue as u8,
            OpCode::Const as u8,
            0,
            OpCode::Call as u8,
            1,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert!(Vm::default().run_main(Rc::new(main)).is_err(),);
}

#[test]
fn call_too_many_args_test() {
    let f = Function {
        bytecode: vec![OpCode::ConstNil as u8, OpCode::Return as u8],
        ..Default::default()
    };

    let main = Function {
        constant_pool: vec![Value::Function(Rc::new(f))],
        bytecode: vec![
            OpCode::ConstTrue as u8,
            OpCode::Const as u8,
            0,
            OpCode::Call as u8,
            1,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert!(Vm::default().run_main(Rc::new(main)).is_err(),);
}

#[test]
fn make_closure_test() {
    let f = Rc::new(Function {
        bytecode: vec![OpCode::ConstNil as u8, OpCode::Return as u8],
        ..Default::default()
    });

    let main = Function {
        constant_pool: vec![Value::Function(f.clone())],
        bytecode: vec![
            OpCode::ConstTrue as u8,
            OpCode::ConstFalse as u8,
            OpCode::MakeClosure as u8,
            2,
            0,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Closure(Rc::new(Closure {
            free: vec![Value::Bool(false), Value::Bool(true),],
            function: f,
        }))
    );
}

#[test]
fn get_current_closure_test() {
    let main = Function {
        constant_pool: vec![],
        bytecode: vec![OpCode::GetCurrentClosure as u8, OpCode::Return as u8],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main.clone())).unwrap(),
        Value::Closure(Rc::new(Closure {
            free: vec![],
            function: Rc::new(main),
        }))
    );
}

#[test]
fn call_closure_test() {
    let f = Rc::new(Function {
        bytecode: vec![OpCode::ConstTrue as u8, OpCode::Return as u8],
        ..Default::default()
    });

    let clo = Rc::new(Closure {
        free: vec![],
        function: f,
    });

    let main = Function {
        constant_pool: vec![Value::Closure(clo)],
        bytecode: vec![
            OpCode::Const as u8,
            0,
            OpCode::Call as u8,
            0,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Bool(true)
    );
}

#[test]
fn get_free_test() {
    let f = Rc::new(Function {
        bytecode: vec![OpCode::GetFree as u8, 0, OpCode::Return as u8],
        ..Default::default()
    });

    let clo = Rc::new(Closure {
        free: vec![Value::Num(42.0), Value::Bool(false)],
        function: f,
    });

    let main = Function {
        constant_pool: vec![Value::Closure(clo)],
        bytecode: vec![
            OpCode::Const as u8,
            0,
            OpCode::Call as u8,
            0,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Num(42.0)
    );
}

#[test]
fn call_native_test() {
    fn sum(body: &[Value]) -> Result<Value, String> {
        match body {
            [Value::Num(a), Value::Num(b)] => Ok(Value::Num(a + b)),
            _ => panic!("Invalid body"),
        }
    }

    let native_f = Rc::new(NativeFunction {
        name: "example".to_string(),
        args_number: 2,
        body: Box::new(sum),
    });

    let main = Function {
        constant_pool: vec![
            Value::Num(10.0),
            Value::Num(20.0),
            Value::NativeFunction(native_f),
        ],
        bytecode: vec![
            OpCode::Const as u8,
            0,
            OpCode::Const as u8,
            1,
            OpCode::Const as u8,
            2,
            OpCode::Call as u8,
            2,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Num(30.0)
    );
}

#[test]
fn panic_no_match_test() {
    let main = Function {
        bytecode: vec![
            OpCode::ConstNil as u8,
            OpCode::PanicNoMatch as u8,
            OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert!(Vm::default().run_main(Rc::new(main)).is_err());
}

#[test]
fn match_const_when_match_test() {
    let main = Function {
        constant_pool: vec![22.0.into(), 42.0.into()],
        locals: 1,
        bytecode: vec![
            /* 00 */ OpCode::Const as u8,
            /* 00 */ 0,
            /* 01 */ OpCode::Const as u8,
            /* 02 */ 1,
            /* 03 */ OpCode::SetLocal as u8,
            /* 04 */ 0,
            /* 05 */ OpCode::MatchConstElseJump as u8,
            /* 06 */ 0,
            /* 07 */ 10, // addr
            /* 08 */ 1, // const
            /* 09 */ 0, // local
            /* 10 */ OpCode::Return as u8,
            /* 11 */ OpCode::PanicNoMatch as u8,
        ],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), 22.0.into());
}

#[test]
fn match_const_when_not_match_test() {
    let main = Function {
        constant_pool: vec![22.0.into(), 42.0.into()],
        locals: 1,
        bytecode: vec![
            /* 00 */ OpCode::Const as u8,
            /* 01 */ 0,
            /* 02 */ OpCode::SetLocal as u8,
            /* 03 */ 0,
            /* 04 */ OpCode::MatchConstElseJump as u8,
            /* 05 */ 0,
            /* 06 */ 10, // addr
            /* 07 */ 1, // const
            /* 08 */ 0, // local
            /* 09 */ OpCode::Return as u8,
            /* 10 */ OpCode::PanicNoMatch as u8,
        ],
        ..Default::default()
    };

    assert!(Vm::default().run_main(Rc::new(main)).is_err());
}

#[ignore]
#[test]
fn match_empty_list_when_not_match_test() {
    let main = Function {
        constant_pool: vec![42.0.into()],
        bytecode: vec![
            /* 00 */ OpCode::ConstTrue as u8,
            //  /* 01 */ OpCode::MatchEmptyListElseJump as u8,
            /* 02 */ 0,
            /* 03 */ 5,
            /* 04 */ OpCode::Return as u8,
            /* 05 */ OpCode::ConstNil as u8,
            /* 06 */ OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), Value::Nil);
}
#[ignore]
#[test]
fn match_empty_list_when_match_test() {
    let main = Function {
        constant_pool: vec![Value::List(List::Empty)],
        bytecode: vec![
            /* 00 */ OpCode::ConstTrue as u8,
            /* 01 */ OpCode::Const as u8,
            /* 02 */ 0,
            // /* 03 */ OpCode::MatchEmptyListElseJump as u8,
            /* 04 */ 0,
            /* 05 */ 7,
            /* 06 */ OpCode::Return as u8,
            /* 07 */ OpCode::ConstFalse as u8,
            /* 08 */ OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), true.into());
}
#[ignore]
#[test]
fn match_empty_map_when_not_match_test() {
    let main = Function {
        constant_pool: vec![42.0.into()],
        bytecode: vec![
            /* 00 */ OpCode::ConstTrue as u8,
            // /* 01 */ OpCode::MatchEmptyMapElseJump as u8,
            /* 02 */ 0,
            /* 03 */ 5,
            /* 04 */ OpCode::Return as u8,
            /* 05 */ OpCode::ConstNil as u8,
            /* 06 */ OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), Value::Nil);
}
#[ignore]
#[test]
fn match_empty_map_when_match_test() {
    let main = Function {
        constant_pool: vec![Value::Map(im_rc::HashMap::new())],
        bytecode: vec![
            /* 00 */ OpCode::ConstTrue as u8,
            /* 01 */ OpCode::Const as u8,
            /* 02 */ 0,
            // /* 03 */ OpCode::MatchEmptyMapElseJump as u8,
            /* 04 */ 0,
            /* 05 */ 7,
            /* 06 */ OpCode::Return as u8,
            /* 07 */ OpCode::ConstFalse as u8,
            /* 08 */ OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), true.into());
}

#[ignore]
#[test]
fn match_cons_map_when_not_match_test() {
    let main = Function {
        constant_pool: vec!["key".into()],
        bytecode: vec![
            /* 00 */ OpCode::ConstTrue as u8,
            /* 01 */ OpCode::ConstNil as u8,
            // /* 02 */ OpCode::MatchConsMapElseJump as u8,
            /* 03 */ 0,
            /* 04 */ 8,
            /* 05 */ 0,
            /* 06 */ OpCode::Return as u8,
            /* 07 */ OpCode::ConstNil as u8,
            /* 08 */ OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), Value::Nil);
}

#[ignore]
#[test]
fn match_cons_map_tl_when_match_test() {
    let m = im_rc::hashmap![
        "x".to_string() => 0.0.into(),
        "y".to_string() => 1.0.into(),
        "z".to_string() => 2.0.into(),
    ];

    let main = Function {
        constant_pool: vec![Value::Map(m), "x".into()],
        bytecode: vec![
            /* 00 */ OpCode::Const as u8,
            /* 01 */ 0,
            // /* 02 */ OpCode::MatchConsMapElseJump as u8,
            /* 03 */ 0,
            /* 04 */ 7,
            /* 05 */ 1,
            /* 06 */ OpCode::Pop as u8,
            /* 07 */ OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::Map(im_rc::hashmap![
            "y".to_string() => 1.0.into(),
            "z".to_string() => 2.0.into(),
        ])
    );
}

#[ignore]
#[test]
fn match_cons_map_hd_when_match_test() {
    let m = im_rc::hashmap![
        "x".to_string() => 0.0.into(),
        "y".to_string() => 1.0.into(),
        "z".to_string() => 2.0.into(),
    ];

    let main = Function {
        constant_pool: vec![Value::Map(m), "x".into()],
        bytecode: vec![
            /* 00 */ OpCode::Const as u8,
            /* 01 */ 0,
            // /* 02 */ OpCode::MatchConsMapElseJump as u8,
            /* 03 */ 0,
            /* 04 */ 6,
            /* 05 */ 1,
            /* 06 */ OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), 0.0.into());
}

#[ignore]
#[test]
fn match_cons_list_when_not_match_test() {
    let main = Function {
        constant_pool: vec![42.0.into()],
        bytecode: vec![
            /* 00 */ OpCode::ConstTrue as u8,
            /* 01 */ OpCode::ConstNil as u8,
            // /* 02 */ OpCode::MatchConsElseJump as u8,
            /* 03 */ 0,
            /* 04 */ 7,
            /* 05 */ OpCode::Return as u8,
            /* 06 */ OpCode::ConstNil as u8,
            /* 07 */ OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), Value::Nil);
}
#[ignore]
#[test]
fn match_cons_list_tl_when_match_test() {
    let lst = Value::List(List::Cons(Rc::new(42.0.into()), Rc::new(List::Empty)));

    let main = Function {
        constant_pool: vec![lst],
        bytecode: vec![
            /* 00 */ OpCode::Const as u8,
            /* 01 */ 0,
            // /* 02 */ OpCode::MatchConsElseJump as u8,
            /* 03 */ 0,
            /* 04 */ 5,
            /* 05 */ OpCode::Pop as u8,
            /* 06 */ OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(
        Vm::default().run_main(Rc::new(main)).unwrap(),
        Value::List(List::Empty)
    );
}
#[ignore]
#[test]
fn match_cons_list_hd_when_match_test() {
    let lst = Value::List(List::Cons(Rc::new(42.0.into()), Rc::new(List::Empty)));

    let main = Function {
        constant_pool: vec![lst],
        bytecode: vec![
            /* 00 */ OpCode::Const as u8,
            /* 01 */ 0,
            // /* 02 */ OpCode::MatchConsElseJump as u8,
            /* 03 */ 0,
            /* 04 */ 6,
            /* 05 */ OpCode::Return as u8,
            /* 06 */ OpCode::ConstTrue as u8,
            /* 07 */ OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), 42.0.into());
}

#[test]
fn match_tuple2_when_match_test() {
    let t = Value::Tuple2(Rc::new(1.0.into()), Rc::new(2.0.into()));

    let main = Function {
        constant_pool: vec![t],
        locals: 1,
        bytecode: vec![
            /* 00 */ OpCode::Const as u8,
            /* 01 */ 0,
            /* 01 */ OpCode::SetLocal as u8,
            /* 02 */ 0,
            /* 03 */ OpCode::MatchTuple2ElseJump as u8,
            /* 04 */ 0,
            /* 05 */ 9,
            /* 06 */ 0,
            /* 07 */ OpCode::Return as u8,
            /* 08 */ OpCode::ConstTrue as u8,
            /* 09 */ OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), 1.0.into());
}

#[test]
fn match_tuple2_when_not_match_test() {
    let main = Function {
        locals: 1,
        bytecode: vec![
            /* 00 */ OpCode::ConstNil as u8,
            /* 01 */ OpCode::SetLocal as u8,
            /* 02 */ 0,
            /* 03 */ OpCode::MatchTuple2ElseJump as u8,
            /* 04 */ 0,
            /* 05 */ 8,
            /* 06 */ 0,
            /* 07 */ OpCode::Return as u8,
            /* 08 */ OpCode::ConstTrue as u8,
            /* 09 */ OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), true.into());
}
#[ignore]
#[test]
fn match_tuple3_when_match_test() {
    let t = Value::Tuple3(
        Rc::new(1.0.into()),
        Rc::new(2.0.into()),
        Rc::new(3.0.into()),
    );

    let main = Function {
        constant_pool: vec![t],
        bytecode: vec![
            /* 00 */ OpCode::Const as u8,
            /* 01 */ 0,
            // /* 02 */ OpCode::MatchTuple3ElseJump as u8,
            /* 03 */ 0,
            /* 04 */ 6,
            /* 05 */ OpCode::Return as u8,
            /* 06 */ OpCode::ConstTrue as u8,
            /* 07 */ OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), 1.0.into());
}
#[ignore]
#[test]
fn match_tuple3_when_not_match_test() {
    let main = Function {
        bytecode: vec![
            /* 00 */ OpCode::ConstNil as u8,
            // /* 01 */ OpCode::MatchTuple3ElseJump as u8,
            /* 02 */ 0,
            /* 03 */ 5,
            /* 04 */ OpCode::Return as u8,
            /* 05 */ OpCode::ConstTrue as u8,
            /* 06 */ OpCode::Return as u8,
        ],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), true.into());
}
#[ignore]
#[test]
fn match_cons_list_global_when_match_test() {
    let main = Function {
        constant_pool: vec![
            Value::Map(im_rc::hashmap! [
                "key".to_string() => 42.0.into()
            ]),
            Value::String(Rc::new("key".to_string())),
        ],
        bytecode: vec![
            /* 00 */ OpCode::Const as u8,
            /* 01 */ 0,
            // /* 02 */ OpCode::MatchConsMapElseJump as u8,
            /* 03 */ 0,
            /* 04 */ 16, // goto PanicNoMatch
            /* 05 */ 1,
            /* 06 */ OpCode::SetGlobal as u8,
            /* 07 */ 0,
            /* 08 */ 0,
            /* 09 */ OpCode::SetGlobal as u8,
            /* 10 */ 0,
            /* 11 */ 1,
            /* 12 */ OpCode::ConstNil as u8,
            /* 13 */ OpCode::Jump as u8,
            /* 14 */ 0,
            /* 15 */ 17, // goto return
            /* 16 */ OpCode::PanicNoMatch as u8, // <-
            /* 17 */ OpCode::Return as u8, // <-
        ],
        ..Default::default()
    };

    assert_eq!(Vm::default().run_main(Rc::new(main)).unwrap(), Value::Nil);
}
