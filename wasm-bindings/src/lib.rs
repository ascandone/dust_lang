use dust_core::formatter;
use dust_core::interpreter;
use dust_core::parser;
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
    let result = match interpreter::eval("Main.ds", src) {
        Ok(value) => Ok(value.to_string()),
        Err(e) => Err(format!("{:?}", e)),
    };

    let ev = EvalOutput { result };

    serde_wasm_bindgen::to_value(&ev).unwrap()
}

#[derive(Serialize, Deserialize)]
pub struct DebugValue {
    pub value: String,
    pub disassembled: String,
}

#[derive(Serialize, Deserialize)]
pub struct EvalDisassembledOutput {
    result: Result<DebugValue, String>,
}

#[wasm_bindgen(js_name = evalSrcDis)]
pub fn eval_src_dis(src: &str) -> JsValue {
    let result = match interpreter::eval_dis("Main.ds", src) {
        Ok((value, f)) => Ok(DebugValue {
            value: value.to_string(),
            disassembled: f.to_string(),
        }),
        Err(error) => Err({
            let err_fmt = interpreter::ErrorFmt { error };
            err_fmt.to_string()
        }),
    };

    let ev = EvalDisassembledOutput { result };

    serde_wasm_bindgen::to_value(&ev).unwrap()
}

#[derive(Serialize, Deserialize)]
pub struct FormatOutput {
    result: Result<String, String>,
}

#[wasm_bindgen()]
pub fn format(src: &str) -> JsValue {
    let result = match parser::parse(src) {
        Ok(program) => Ok(formatter::format(program)),
        Err(e) => Err(format!("{:?}", e)),
    };

    let ev = FormatOutput { result };

    serde_wasm_bindgen::to_value(&ev).unwrap()
}
