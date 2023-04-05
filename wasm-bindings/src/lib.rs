use core::interpreter;
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

#[derive(Serialize, Deserialize)]
pub struct ReplOutput {
    result: Result<Vec<String>, String>,
}

#[derive(Serialize, Deserialize)]
pub struct EvalOutput {
    result: Result<String, String>,
}

#[wasm_bindgen(js_name = evalSrc)]
pub fn eval_src(src: &str) -> JsValue {
    let result = match interpreter::eval(src) {
        Ok(value) => Ok(value.to_string()),
        Err(e) => Err(format!("{:?}", e)),
    };

    let ev = EvalOutput { result };

    serde_wasm_bindgen::to_value(&ev).unwrap()
}
