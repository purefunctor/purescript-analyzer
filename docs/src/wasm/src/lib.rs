mod engine;
mod utils;

use std::cell::RefCell;

use building_types::QueryProxy;
use checking::core::pretty;
use engine::WasmQueryEngine;
use indexing::{TermItem, TypeItem};
use serde::Serialize;
use wasm_bindgen::prelude::*;
use web_sys::{js_sys, WorkerGlobalScope};

thread_local! {
    static ENGINE: RefCell<WasmQueryEngine> = RefCell::new(WasmQueryEngine::new());
}

fn get_performance() -> web_sys::Performance {
    js_sys::global()
        .dyn_into::<WorkerGlobalScope>()
        .ok()
        .and_then(|w| w.performance())
        .or_else(|| web_sys::window().and_then(|w| w.performance()))
        .unwrap()
}

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
    let performance = get_performance();

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

#[derive(Serialize)]
pub struct SynonymExpansion {
    name: String,
    expansion: String,
    quantified_variables: u32,
    kind_variables: u32,
    type_variables: u32,
}

#[derive(Serialize)]
pub struct CheckErrorInfo {
    kind: String,
    message: String,
    location: Option<String>,
}

#[derive(Serialize)]
pub struct CheckTiming {
    lex: f64,
    layout: f64,
    parse: f64,
    stabilize: f64,
    index: f64,
    resolve: f64,
    lower: f64,
    check: f64,
    total: f64,
}

#[derive(Serialize)]
pub struct CheckResult {
    terms: Vec<String>,
    types: Vec<String>,
    synonyms: Vec<SynonymExpansion>,
    errors: Vec<CheckErrorInfo>,
    timing: CheckTiming,
}

#[wasm_bindgen]
pub fn check(source: &str) -> JsValue {
    let performance = get_performance();

    let result = ENGINE.with_borrow_mut(|engine| {
        let total_start = performance.now();

        // Parse
        let start = performance.now();
        let lexed = lexing::lex(source);
        let lex_time = performance.now() - start;

        let start = performance.now();
        let tokens = lexing::layout(&lexed);
        let layout_time = performance.now() - start;

        let start = performance.now();
        let _ = parsing::parse(&lexed, &tokens);
        let parse_time = performance.now() - start;

        // Set user source and run queries
        let id = engine.set_user_source(source);

        let start = performance.now();
        let _ = engine.stabilized(id);
        let stabilize_time = performance.now() - start;

        let start = performance.now();
        let indexed = match engine.indexed(id) {
            Ok(i) => i,
            Err(e) => {
                return CheckResult {
                    terms: vec![],
                    types: vec![],
                    synonyms: vec![],
                    errors: vec![CheckErrorInfo {
                        kind: "IndexError".to_string(),
                        message: format!("{e:?}"),
                        location: None,
                    }],
                    timing: CheckTiming {
                        lex: lex_time,
                        layout: layout_time,
                        parse: parse_time,
                        stabilize: stabilize_time,
                        index: 0.0,
                        resolve: 0.0,
                        lower: 0.0,
                        check: 0.0,
                        total: performance.now() - total_start,
                    },
                };
            }
        };
        let index_time = performance.now() - start;

        let start = performance.now();
        if let Err(e) = engine.resolved(id) {
            return CheckResult {
                terms: vec![],
                types: vec![],
                synonyms: vec![],
                errors: vec![CheckErrorInfo {
                    kind: "ResolveError".to_string(),
                    message: format!("{e:?}"),
                    location: None,
                }],
                timing: CheckTiming {
                    lex: lex_time,
                    layout: layout_time,
                    parse: parse_time,
                    stabilize: stabilize_time,
                    index: index_time,
                    resolve: 0.0,
                    lower: 0.0,
                    check: 0.0,
                    total: performance.now() - total_start,
                },
            };
        }
        let resolve_time = performance.now() - start;

        let start = performance.now();
        if let Err(e) = engine.lowered(id) {
            return CheckResult {
                terms: vec![],
                types: vec![],
                synonyms: vec![],
                errors: vec![CheckErrorInfo {
                    kind: "LowerError".to_string(),
                    message: format!("{e:?}"),
                    location: None,
                }],
                timing: CheckTiming {
                    lex: lex_time,
                    layout: layout_time,
                    parse: parse_time,
                    stabilize: stabilize_time,
                    index: index_time,
                    resolve: resolve_time,
                    lower: 0.0,
                    check: 0.0,
                    total: performance.now() - total_start,
                },
            };
        }
        let lower_time = performance.now() - start;

        let start = performance.now();
        let checked: std::sync::Arc<checking::CheckedModule> = match engine.checked(id) {
            Ok(c) => c,
            Err(e) => {
                return CheckResult {
                    terms: vec![],
                    types: vec![],
                    synonyms: vec![],
                    errors: vec![CheckErrorInfo {
                        kind: "CheckError".to_string(),
                        message: format!("{e:?}"),
                        location: None,
                    }],
                    timing: CheckTiming {
                        lex: lex_time,
                        layout: layout_time,
                        parse: parse_time,
                        stabilize: stabilize_time,
                        index: index_time,
                        resolve: resolve_time,
                        lower: lower_time,
                        check: 0.0,
                        total: performance.now() - total_start,
                    },
                };
            }
        };
        let check_time = performance.now() - start;

        // Extract results
        let mut terms = Vec::new();
        for (term_id, TermItem { name, .. }) in indexed.items.iter_terms() {
            let Some(n) = name else { continue };
            let Some(t) = checked.lookup_term(term_id) else { continue };
            terms.push(pretty::print_signature_global(engine, n.as_str(), t));
        }

        let mut types = Vec::new();
        for (type_id, TypeItem { name, .. }) in indexed.items.iter_types() {
            let Some(n) = name else { continue };
            let Some(t) = checked.lookup_type(type_id) else { continue };
            types.push(pretty::print_signature_global(engine, n.as_str(), t));
        }

        let mut synonyms = Vec::new();
        for (type_id, TypeItem { name, .. }) in indexed.items.iter_types() {
            let Some(n) = name else { continue };
            let Some(group) = checked.lookup_synonym(type_id) else { continue };
            let expansion = pretty::print_global(engine, group.synonym_type);
            synonyms.push(SynonymExpansion {
                name: n.to_string(),
                expansion,
                quantified_variables: group.quantified_variables.0,
                kind_variables: group.kind_variables.0,
                type_variables: group.type_variables.0,
            });
        }

        let mut errors = Vec::new();
        for error in &checked.errors {
            let (kind, message) = match &error.kind {
                checking::error::ErrorKind::CannotUnify { t1, t2 } => {
                    let t1_pretty = pretty::print_global(engine, *t1);
                    let t2_pretty = pretty::print_global(engine, *t2);
                    ("CannotUnify".to_string(), format!("{t1_pretty} ~ {t2_pretty}"))
                }
                checking::error::ErrorKind::NoInstanceFound { constraint } => {
                    let c_pretty = pretty::print_global(engine, *constraint);
                    ("NoInstanceFound".to_string(), c_pretty)
                }
                checking::error::ErrorKind::AmbiguousConstraint { constraint } => {
                    let c_pretty = pretty::print_global(engine, *constraint);
                    ("AmbiguousConstraint".to_string(), c_pretty)
                }
                _ => (format!("{:?}", error.kind), String::new()),
            };
            errors.push(CheckErrorInfo {
                kind,
                message,
                location: Some(format!("{:?}", error.step)),
            });
        }

        let total_time = performance.now() - total_start;

        CheckResult {
            terms,
            types,
            synonyms,
            errors,
            timing: CheckTiming {
                lex: lex_time,
                layout: layout_time,
                parse: parse_time,
                stabilize: stabilize_time,
                index: index_time,
                resolve: resolve_time,
                lower: lower_time,
                check: check_time,
                total: total_time,
            },
        }
    });

    serde_wasm_bindgen::to_value(&result).unwrap()
}

/// Register an external module (from a package) with the engine.
#[wasm_bindgen]
pub fn register_module(module_name: &str, source: &str) {
    ENGINE.with_borrow_mut(|engine| {
        engine.register_external_module(module_name, source);
    });
}

/// Clear all external modules (packages), keeping Prim and user modules.
#[wasm_bindgen]
pub fn clear_packages() {
    ENGINE.with_borrow_mut(|engine| {
        engine.clear_external_modules();
    });
}
