use analyzer::{QueryEngine, prim};
use checking::core::TypeStorage;
use files::Files;
use interner::Interner;

#[test]
fn test_checking() {
    let mut engine = QueryEngine::default();
    let mut files = Files::default();
    prim::configure(&mut engine, &mut files);

    let id = files.insert(
        "Main.purs",
        r#"
module Main where

foreign import data Proxy :: forall a b. a -> b -> a
"#,
    );

    let content = files.content(id);
    engine.set_content(id, content);

    #[derive(Debug)]
    struct InlineStorage {
        inner: Interner<checking::core::Type>,
    }

    impl Default for InlineStorage {
        fn default() -> Self {
            let inner = Interner::default();
            InlineStorage { inner }
        }
    }

    impl TypeStorage for InlineStorage {
        fn intern(&mut self, t: checking::core::Type) -> checking::core::TypeId {
            self.inner.intern(t)
        }

        fn index(&self, id: checking::core::TypeId) -> &checking::core::Type {
            &self.inner[id]
        }
    }

    let mut storage = InlineStorage::default();
    checking::check_module(&mut engine, &mut storage, id).unwrap();
}
