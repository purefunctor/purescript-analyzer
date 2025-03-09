use std::sync::Arc;

use files::FileId;
use indexing::FullModuleIndex;
use la_arena::RawIdx;
use lowering::FullModuleLower;
use resolving::External;
use rowan::ast::AstNode;
use syntax::cst;

struct FauxExternal {}

const MAIN: &str = r#"module Main where

import Lib as Lib

eq = Lib.eqIntImpl"#;

const LIB: &str = r#"module Lib (eqIntImpl) where

eqInt _ _ = false

eqIntImpl _ _ = true"#;

impl External for FauxExternal {
    fn lookup_module_name(&self, name: &str) -> Option<files::FileId> {
        match name {
            "Main" => Some(FileId::from_raw(RawIdx::from_u32(0))),
            "Lib" => Some(FileId::from_raw(RawIdx::from_u32(1))),
            _ => None,
        }
    }

    fn index(&mut self, id: FileId) -> Arc<FullModuleIndex> {
        index_source(match id.into_raw().into_u32() {
            0 => MAIN,
            1 => LIB,
            _ => unreachable!(),
        })
    }
}

fn index_source(source: &str) -> Arc<FullModuleIndex> {
    let lexed = lexing::lex(source);
    let tokens = lexing::layout(&lexed);

    let (module, _) = parsing::parse(&lexed, &tokens);
    let module = cst::Module::cast(module).unwrap();
    let index = indexing::index_module(&module);

    Arc::new(index)
}

fn resolve_source(source: &str) -> (cst::Module, FullModuleIndex, FullModuleLower) {
    let lexed = lexing::lex(source);
    let tokens = lexing::layout(&lexed);

    let (module, _) = parsing::parse(&lexed, &tokens);
    let module = cst::Module::cast(module).unwrap();

    let mut context = FauxExternal {};
    let index = indexing::index_module(&module);
    let lower = lowering::lower_module(&module, &index.index, &index.relational, &index.source);

    let _ = resolving::resolve(&mut context, &index, &lower);

    (module, index, lower)
}

#[test]
fn test_basic() {
    let _ = resolve_source(MAIN);
}
