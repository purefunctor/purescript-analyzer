use checking::{check, core};
use indexing::FullIndexedModule;
use interner::Interner;
use lowering::FullLoweredModule;
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
        &self.inner[id]
    }
}

fn check_source(source: &str) -> (cst::Module, FullIndexedModule, FullLoweredModule) {
    let lexed = lexing::lex(source);
    let tokens = lexing::layout(&lexed);

    let (module, _) = parsing::parse(&lexed, &tokens);
    let module = cst::Module::cast(module).unwrap();

    let indexed = indexing::index_module(&module);
    let lowered = lowering::lower_module(&module, &indexed);

    let mut storage = InlineStorage::default();
    check::check_module(&mut storage, &indexed, &lowered);

    (module, indexed, lowered)
}

#[test]
fn test_basic() {
    let (_, _, _) = check_source(
        r#"module Main where
-- foreign import const :: forall a b. a -> b -> a

-- instance Eq a => Ord a

-- instance (Eq a, Eq b) => Ord (Either a b)

instance TypeEquals x y where
  proof :: forall p. p x -> p y
  proof = proofImpl
"#,
    );
}
