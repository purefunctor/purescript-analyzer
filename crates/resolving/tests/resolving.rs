use std::sync::Arc;

use files::FileId;
use indexing::FullModuleIndex;
use la_arena::RawIdx;
use resolving::{External, FullModuleExports};
use rowan::ast::AstNode;
use syntax::cst;

pub const MAIN: &'static str = include_str!("fixtures/Main.txt");

pub fn parse(id: FileId) -> cst::Module {
    let id = id.into_raw().into_u32();
    let source = match id {
        0 => MAIN,
        _ => unreachable!(),
    };

    let lexed = lexing::lex(source);
    let tokens = lexing::layout(&lexed);

    let (module, _) = parsing::parse(&lexed, &tokens);
    let module = cst::Module::cast(module).unwrap();

    module
}

struct TestExternal;

impl External for TestExternal {
    fn index(&mut self, id: FileId) -> Arc<FullModuleIndex> {
        let module = parse(id);
        let index = indexing::index_module(&module);
        Arc::new(index)
    }

    fn exports(&mut self, id: FileId) -> Arc<FullModuleExports> {
        let exports = resolving::module_exports(self, id);
        Arc::new(exports)
    }
}

macro_rules! file {
    ($id:expr) => {
        FileId::from_raw(RawIdx::from_u32($id))
    };
}

#[test]
fn test_basic() {
    let mut external = TestExternal;
    dbg!(external.exports(file!(0)));
}
