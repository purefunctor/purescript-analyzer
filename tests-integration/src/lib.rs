pub mod generated;

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
