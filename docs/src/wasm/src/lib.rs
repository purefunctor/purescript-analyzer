mod utils;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn parse(source: &str) -> String {
    let lexed = lexing::lex(source);
    let tokens = lexing::layout(&lexed);
    let (node, _) = parsing::parse(&lexed, &tokens);
    format!("{:#?}", node)
}
