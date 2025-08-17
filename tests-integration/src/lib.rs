pub mod core;
pub mod lsp;

use std::{
    fs,
    path::{Path, PathBuf},
};

use analyzer::QueryEngine;
use files::Files;
use glob::glob;

fn load_file(engine: &mut QueryEngine, files: &mut Files, path: &Path) {
    let uri = format!("file://{}", path.to_str().unwrap());
    let file = fs::read_to_string(path).unwrap();

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
    use files::Files;

    use crate::QueryEngine;

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
apply f a = f a
map f a = f a

infix 4 eq as ==
infixl 5 add as +
infixr 0 apply as $
infixl 4 map as <$>

test = 1 $ 2 $ 3 $ 4
"#,
        );
        let content = files.content(id);

        engine.set_content(id, content);

        let _ = sugar::bracketed(&engine, id);
    }
}
