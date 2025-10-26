use std::{fs::File, io::Write, sync::LazyLock};

use files::Files;
use prim_constants::MODULE_MAP;
use tempfile::{Builder, TempDir};
use url::Url;

use crate::QueryEngine;

pub const SCHEME: &str = "prim";

pub fn configure(engine: &mut QueryEngine, files: &mut Files) {
    for (name, content) in MODULE_MAP {
        let path = format!("{SCHEME}://localhost/{name}.purs");
        let id = files.insert(path, *content);

        engine.set_content(id, *content);
        engine.set_module_file(name, id);
    }
}

pub static TEMPORARY_DIRECTORY: LazyLock<TempDir> = LazyLock::new(|| {
    Builder::new()
        .prefix("purescript-analyzer-")
        .tempdir()
        .expect("invariant violated: failed to create TEMPORARY_DIRECTORY")
});

pub fn handle_generated(uri: Url, content: &str) -> Option<Url> {
    if uri.scheme() != SCHEME {
        return Some(uri);
    }

    let file = uri.path_segments()?.next_back()?;
    let path = TEMPORARY_DIRECTORY.path().join(file);

    let mut file = File::create(&path).ok()?;
    write!(file, "{content}").ok()?;

    Url::from_file_path(path).ok()
}
