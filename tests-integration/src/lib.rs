pub mod core;
pub mod lsp;

use std::{
    fs,
    path::{Path, PathBuf},
};

use analyzer::Compiler;
use glob::glob;
use smol_str::SmolStrBuilder;

fn load_file(compiler: &mut Compiler, path: &Path) {
    let uri = format!("file://{}", path.to_str().unwrap());
    let file = fs::read_to_string(path).unwrap();

    let id = compiler.files.insert(uri, file);
    let content = compiler.files.content(id);

    compiler.runtime.set_content(id, content);
    let Ok((parsed, _)) = compiler.runtime.parsed(id) else {
        return;
    };

    let cst = parsed.cst();
    if let Some(cst) = cst.header().and_then(|cst| cst.name()) {
        let mut builder = SmolStrBuilder::default();
        if let Some(token) = cst.qualifier().and_then(|cst| cst.text()) {
            builder.push_str(token.text());
        }
        if let Some(token) = cst.name_token() {
            builder.push_str(token.text());
        }
        let name = builder.finish();
        compiler.runtime.update_module_name(|m| {
            m.intern_with_file(&name, id);
        });
    }
}

fn load_folder(folder: &Path) -> impl Iterator<Item = PathBuf> {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let packages = manifest.join(folder);
    let pattern = format!("{}/**/*.purs", packages.to_str().unwrap());
    glob(&pattern).unwrap().filter_map(Result::ok)
}

pub fn load_compiler(folder: &Path) -> Compiler {
    let mut compiler = Compiler::default();
    load_folder(folder).for_each(|path| {
        load_file(&mut compiler, &path);
    });
    compiler
}
