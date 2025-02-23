use checking::{check, core};
use indexing::FullModuleIndex;
use interner::Interner;
use lowering::FullModuleLower;
use rowan::ast::AstNode;
use syntax::cst;

#[derive(Debug)]
struct InlineStorage {
    inner: Interner<core::Type>,
    unknown: core::TypeId,
}

impl Default for InlineStorage {
    fn default() -> InlineStorage {
        let mut inner = Interner::default();
        let unknown = inner.intern(core::Type::Unknown);
        InlineStorage { inner, unknown }
    }
}

impl core::CoreStorage for InlineStorage {
    fn unknown(&self) -> core::TypeId {
        self.unknown
    }

    fn allocate(&mut self, ty: core::Type) -> core::TypeId {
        self.inner.intern(ty)
    }

    fn index(&self, id: core::TypeId) -> &core::Type {
        self.inner.index(id)
    }
}

fn check_source(source: &str) -> (cst::Module, FullModuleIndex, FullModuleLower) {
    let lexed = lexing::lex(source);
    let tokens = lexing::layout(&lexed);

    let (module, _) = parsing::parse(&lexed, &tokens);
    let module = cst::Module::cast(module).unwrap();

    let full_module_index = indexing::index_module(&module);
    let full_module_lower = lowering::lower_module(
        &module,
        &full_module_index.index,
        &full_module_index.relational,
        &full_module_index.source,
    );

    let mut storage = InlineStorage::default();
    let _ = check::check_module(&mut storage, &full_module_index, &full_module_lower);

    (module, full_module_index, full_module_lower)
}

#[test]
fn test_basic() {
    let (_, _, _) = check_source(
        r#"module Main where
foreign import unit :: forall a. a -> a"#,
    );
}
