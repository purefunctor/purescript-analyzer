use std::sync::Arc;

use files::FileId;
use indexing::FullModuleIndex;
use la_arena::RawIdx;
use lowering::FullModuleLower;
use resolving::{External, FullResolvedModule};
use rowan::ast::AstNode;
use syntax::cst;

pub const MAIN: &str = include_str!("fixtures/Main.txt");
pub const LIB: &str = include_str!("fixtures/Lib.txt");
pub const INTERNAL: &str = include_str!("fixtures/Internal.txt");

macro_rules! file {
    ($id:expr) => {
        FileId::from_raw(RawIdx::from_u32($id))
    };
}

pub fn parse(id: FileId) -> cst::Module {
    let id = id.into_raw().into_u32();
    let source = match id {
        0 => MAIN,
        1 => LIB,
        2 => INTERNAL,
        _ => unreachable!(),
    };

    let lexed = lexing::lex(source);
    let tokens = lexing::layout(&lexed);

    let (module, _) = parsing::parse(&lexed, &tokens);

    cst::Module::cast(module).unwrap()
}

struct TestExternal;

impl External for TestExternal {
    fn indexed(&mut self, id: FileId) -> Arc<FullModuleIndex> {
        let module = parse(id);
        let index = indexing::index_module(&module);
        Arc::new(index)
    }

    fn lowered(&mut self, id: FileId) -> Arc<FullModuleLower> {
        let module = parse(id);
        let index = self.indexed(id);
        let lowered =
            lowering::lower_module(&module, &index.index, &index.relational, &index.source);
        Arc::new(lowered)
    }

    fn resolved(&mut self, id: FileId) -> Arc<FullResolvedModule> {
        let exports = resolving::resolve_module(self, id);
        Arc::new(exports)
    }

    fn file_id(&mut self, name: &str) -> FileId {
        match name {
            "Main" => file!(0),
            "Lib" => file!(1),
            "Internal" => file!(2),
            _ => unreachable!(),
        }
    }
}

#[test]
fn test_basic() {
    let mut external = TestExternal;

    let lowered = external.lowered(file!(0));
    let resolved = external.resolved(file!(0));

    for (id, deferred) in lowered.graph.deferred() {
        match deferred.domain {
            lowering::ResolutionDomain::Term => {
                if let Some(name) = &deferred.name {
                    let r = resolved.lookup_term(name);
                    println!("{:?} = {:?}", id, r);
                }
            }
            lowering::ResolutionDomain::Type => {
                if let Some(name) = &deferred.name {
                    let r = resolved.lookup_type(name);
                    println!("{:?} = {:?}", id, r);
                }
            }
        }
    }
}
