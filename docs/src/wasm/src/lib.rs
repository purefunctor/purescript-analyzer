mod utils;
use wasm_bindgen::prelude::*;
use web_sys::{js_sys, WorkerGlobalScope};

#[wasm_bindgen]
pub struct ParseResult {
    output: String,
    pub lex: f64,
    pub layout: f64,
    pub parse: f64,
}

#[wasm_bindgen]
impl ParseResult {
    #[wasm_bindgen(getter)]
    pub fn output(&self) -> String {
        self.output.clone()
    }
}

#[wasm_bindgen]
pub fn parse(source: &str) -> ParseResult {
    let performance = {
        js_sys::global()
            .dyn_into::<WorkerGlobalScope>()
            .ok()
            .and_then(|w| w.performance())
            .or_else(|| web_sys::window().and_then(|w| w.performance()))
            .unwrap()
    };

    let start = performance.now();
    let lexed = lexing::lex(source);
    let lex = performance.now() - start;

    let start = performance.now();
    let tokens = lexing::layout(&lexed);
    let layout = performance.now() - start;

    let start = performance.now();
    let (parsed, _) = parsing::parse(&lexed, &tokens);
    let node = parsed.syntax_node();
    let parse = performance.now() - start;

    let output = format!("{node:#?}");
    ParseResult { output, lex, layout, parse }
}
