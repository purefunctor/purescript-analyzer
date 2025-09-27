pub mod core;
pub mod lsp;

use std::{
    fs,
    path::{Path, PathBuf},
};

use analyzer::{QueryEngine, prim};
use files::Files;
use glob::glob;
use url::Url;

fn load_file(engine: &mut QueryEngine, files: &mut Files, path: &Path) {
    let url = Url::from_file_path(path).unwrap();
    let file = fs::read_to_string(path).unwrap();

    let uri = url.to_string();
    let id = files.insert(uri, file);
    let content = files.content(id);

    engine.set_content(id, content);
    let Ok((parsed, _)) = engine.parsed(id) else {
        return;
    };

    if let Some(name) = parsed.module_name() {
        engine.set_module_file(&name, id);
    }
}

fn load_folder(folder: &Path) -> impl Iterator<Item = PathBuf> {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let packages = manifest.join(folder);
    let pattern = format!("{}/**/*.purs", packages.to_str().unwrap());
    glob(&pattern).unwrap().filter_map(Result::ok)
}

pub fn load_compiler(folder: &Path) -> (QueryEngine, Files) {
    let mut engine = QueryEngine::default();
    let mut files = Files::default();
    prim::configure(&mut engine, &mut files);
    load_folder(folder).for_each(|path| {
        load_file(&mut engine, &mut files, &path);
    });
    (engine, files)
}

#[cfg(test)]
mod manual_tests {
    use checking::core::TypeStorage;
    use files::Files;
    use interner::Interner;

    use crate::{QueryEngine, prim};

    #[test]
    fn test_rebracketing() {
        let mut engine = QueryEngine::default();
        let mut files = Files::default();
        prim::configure(&mut engine, &mut files);

        let id = files.insert(
            "Main.purs",
            r#"
module Main where

eq a a = false
add a a = 0
mult a a = 0
apply f a = f a
map f a = f a

infix 4 eq as ==
infixl 5 add as +
infixl 6 mult as *
infixr 0 apply as $
infixl 4 map as <$>

test = 1 + 2 * 3 + 4
"#,
        );
        let content = files.content(id);

        engine.set_content(id, content);

        let lowered = engine.lowered(id).unwrap();
        let result = sugar::bracketing::bracketed(&engine, &lowered);

        println!("{result:#?}");
    }

    #[test]
    fn test_checking() {
        let mut engine = QueryEngine::default();
        let mut files = Files::default();
        prim::configure(&mut engine, &mut files);

        let id = files.insert(
            "Main.purs",
            r#"
module Main where

foreign import data Effect :: Type -> Type

foreign import test :: (forall a. b) -> (forall a. b)
"#,
        );

        let content = files.content(id);
        engine.set_content(id, content);

        #[derive(Debug)]
        struct InlineStorage {
            inner: Interner<checking::core::Type>,
            unknown: checking::core::TypeId,
        }

        impl Default for InlineStorage {
            fn default() -> Self {
                let mut inner = Interner::default();
                let unknown = inner.intern(checking::core::Type::Unknown);
                Self { inner, unknown }
            }
        }

        impl TypeStorage for InlineStorage {
            fn unknown(&self) -> checking::core::TypeId {
                self.unknown
            }

            fn intern(&mut self, t: checking::core::Type) -> checking::core::TypeId {
                self.inner.intern(t)
            }

            fn index(&self, id: checking::core::TypeId) -> &checking::core::Type {
                &self.inner[id]
            }
        }

        let mut storage = InlineStorage::default();
        let indexed = engine.indexed(id).unwrap();
        let lowered = engine.lowered(id).unwrap();

        checking::check_module(&mut storage, &indexed, &lowered);
    }
}
