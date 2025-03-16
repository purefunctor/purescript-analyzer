use std::sync::Arc;

use files::FileId;
use indexing::FullModuleIndex;
use la_arena::RawIdx;
use lowering::{FullModuleLower, lower_module};
use resolving::External;
use rowan::ast::AstNode;
use syntax::cst;

struct FauxExternal {}

const MAIN: &str = r#"module Main where

import Lib

a = eqInt
b = eqIntImpl
c = a
d = internal
e = reallyInternal"#;

const LIB: &str = r#"module Lib (eqIntImpl, module Internal) where

import Internal as Internal

eqInt _ _ = false

eqIntImpl _ _ = true"#;

const INTERNAL: &str = r#"module Internal (module Lib) where

import Internal.Lib as Lib

internal = false"#;

const INTERNAL_LIB: &str = r#"module Internal.Lib where

internal = false
reallyInternal = false"#;

impl External for FauxExternal {
    fn lookup_module_name(&self, name: &str) -> Option<files::FileId> {
        match name {
            "Main" => Some(FileId::from_raw(RawIdx::from_u32(0))),
            "Lib" => Some(FileId::from_raw(RawIdx::from_u32(1))),
            "Internal" => Some(FileId::from_raw(RawIdx::from_u32(2))),
            "Internal.Lib" => Some(FileId::from_raw(RawIdx::from_u32(3))),
            _ => None,
        }
    }

    fn index(&mut self, id: FileId) -> Arc<FullModuleIndex> {
        index_source(match id.into_raw().into_u32() {
            0 => MAIN,
            1 => LIB,
            2 => INTERNAL,
            3 => INTERNAL_LIB,
            _ => unreachable!(),
        })
    }

    fn lower(&mut self, id: FileId) -> Arc<FullModuleLower> {
        lower_source(match id.into_raw().into_u32() {
            0 => MAIN,
            1 => LIB,
            2 => INTERNAL,
            3 => INTERNAL_LIB,
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

fn lower_source(source: &str) -> Arc<FullModuleLower> {
    let lexed = lexing::lex(source);
    let tokens = lexing::layout(&lexed);

    let (module, _) = parsing::parse(&lexed, &tokens);
    let module = cst::Module::cast(module).unwrap();

    let index = indexing::index_module(&module);
    let lower = lowering::lower_module(&module, &index.index, &index.relational, &index.source);

    Arc::new(lower)
}

#[test]
fn test_basic() {
    let mut external = FauxExternal {};
    resolving::resolve(&mut external, FileId::from_raw(RawIdx::from_u32(0)));
}
