use std::{fs, path::Path, sync::Arc};

use files::FileId;
use indexing::FullIndexedModule;
use resolving::{External, FullResolvedModule};
use rowan::ast::AstNode;
use rustc_hash::FxHashMap;
use smol_str::{SmolStr, SmolStrBuilder};
use syntax::cst;

#[derive(Debug, Default)]
struct IntegrationTestExternal {
    files: files::Files,
    names: FxHashMap<SmolStr, FileId>,
}

impl IntegrationTestExternal {
    fn parsed(&mut self, id: FileId) -> Arc<cst::Module> {
        let source = self.files.content(id);
        let lexed = lexing::lex(&source);
        let tokens = lexing::layout(&lexed);
        let (module, _) = parsing::parse(&lexed, &tokens);
        Arc::new(cst::Module::cast(module).unwrap())
    }
}

impl External for IntegrationTestExternal {
    fn indexed(&mut self, id: FileId) -> Arc<FullIndexedModule> {
        let cst = self.parsed(id);
        Arc::new(indexing::index_module(&cst))
    }

    fn resolved(&mut self, id: FileId) -> Arc<FullResolvedModule> {
        Arc::new(resolving::resolve_module(self, id))
    }

    fn file_id(&mut self, name: &str) -> FileId {
        *self.names.get(name).unwrap()
    }
}

fn create_external(folder: &str) -> IntegrationTestExternal {
    let mut external = IntegrationTestExternal::default();

    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let folder = manifest.join("tests/fixtures").join(folder);

    for entry in folder.read_dir().unwrap() {
        let Ok(entry) = entry else { continue };
        let path = entry.path();

        let k = format!("file://{}", path.to_str().unwrap());
        let v = fs::read_to_string(path).unwrap();

        let id = external.files.insert(k, v);
        let cst = external.parsed(id);

        if let Some(cst) = cst.header().and_then(|cst| cst.name()) {
            let mut builder = SmolStrBuilder::default();
            if let Some(token) = cst.qualifier().and_then(|cst| cst.text()) {
                builder.push_str(token.text());
            }
            if let Some(token) = cst.name_token() {
                builder.push_str(token.text());
            }
            let name = builder.finish();
            external.names.insert(name, id);
        }
    }

    external
}

#[test]
fn test_001_local_resolution() {
    let mut external = create_external("001_local_resolution");
    let id = external.file_id("Main");
    insta::assert_debug_snapshot!(external.resolved(id));
}
