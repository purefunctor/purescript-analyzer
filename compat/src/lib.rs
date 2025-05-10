use std::{fs, path::Path};

use building::Runtime;
use files::Files;
use glob::glob;
use smol_str::SmolStrBuilder;

#[derive(Default)]
pub struct CompatibilityCompiler {
    files: Files,
    runtime: Runtime,
}

pub fn create_compiler() -> CompatibilityCompiler {
    let mut compiler = CompatibilityCompiler::default();

    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let packages = manifest.join("packages");
    let pattern = format!("{}/**/*.purs", packages.to_str().unwrap());

    for entry in glob(&pattern).unwrap().filter_map(Result::ok) {
        let k = format!("file://{}", entry.to_str().unwrap());
        let v = fs::read_to_string(entry).unwrap();

        let id = compiler.files.insert(k, v);
        let content = compiler.files.content(id);

        compiler.runtime.set_content(id, content);
        let cst = compiler.runtime.parse(id);

        if let Some(cst) = cst.header().and_then(|cst| cst.name()) {
            let mut builder = SmolStrBuilder::default();
            if let Some(token) = cst.qualifier().and_then(|cst| cst.text()) {
                builder.push_str(token.text());
            }
            if let Some(token) = cst.name_token() {
                builder.push_str(token.text());
            }
            let name = builder.finish();
            compiler.runtime.set_module(&name, id);
        }
    }

    compiler
}

#[test]
fn test_basic() {
    let mut compiler = create_compiler();
    let Some(id) = compiler.runtime.module("Data.Array") else { return };
    dbg!(compiler.runtime.resolve(id));
}
