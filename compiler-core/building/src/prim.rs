use std::{fs::File, io::Write, sync::LazyLock};

use files::Files;
use tempfile::{Builder, TempDir};
use url::Url;

use crate::QueryEngine;

pub const SCHEME: &str = "prim";
pub const PRIM: &str = include_str!("prim/Prim.purs");
pub const PRIM_BOOLEAN: &str = include_str!("prim/Prim.Boolean.purs");
pub const PRIM_COERCE: &str = include_str!("prim/Prim.Coerce.purs");
pub const PRIM_INT: &str = include_str!("prim/Prim.Int.purs");
pub const PRIM_ORDERING: &str = include_str!("prim/Prim.Ordering.purs");
pub const PRIM_ROW: &str = include_str!("prim/Prim.Row.purs");
pub const PRIM_ROW_LIST: &str = include_str!("prim/Prim.RowList.purs");
pub const PRIM_SYMBOL: &str = include_str!("prim/Prim.Symbol.purs");
pub const PRIM_TYPE_ERROR: &str = include_str!("prim/Prim.TypeError.purs");

pub fn configure(engine: &mut QueryEngine, files: &mut Files) {
    for (name, content) in [
        ("Prim", PRIM),
        ("Prim.Boolean", PRIM_BOOLEAN),
        ("Prim.Coerce", PRIM_COERCE),
        ("Prim.Int", PRIM_INT),
        ("Prim.Ordering", PRIM_ORDERING),
        ("Prim.Row", PRIM_ROW),
        ("Prim.RowList", PRIM_ROW_LIST),
        ("Prim.Symbol", PRIM_SYMBOL),
        ("Prim.TypeError", PRIM_TYPE_ERROR),
    ] {
        let path = format!("{SCHEME}://localhost/{name}.purs");
        let id = files.insert(path, content);

        engine.set_content(id, content);
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
