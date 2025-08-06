use std::{fs::File, io::Write, sync::OnceLock};

use tempfile::{Builder, TempDir};
use url::Url;

use building::QueryEngine;
use files::Files;

pub const HOST: &str = "generated";
pub const PRIM: &str = include_str!("Prim.purs");
pub const PRIM_BOOLEAN: &str = include_str!("Prim.Boolean.purs");
pub const PRIM_COERCE: &str = include_str!("Prim.Coerce.purs");
pub const PRIM_INT: &str = include_str!("Prim.Int.purs");
pub const PRIM_ORDERING: &str = include_str!("Prim.Ordering.purs");
pub const PRIM_ROW: &str = include_str!("Prim.Row.purs");
pub const PRIM_ROW_LIST: &str = include_str!("Prim.RowList.purs");
pub const PRIM_SYMBOL: &str = include_str!("Prim.Symbol.purs");
pub const PRIM_TYPE_ERROR: &str = include_str!("Prim.TypeError.purs");

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
        let path = format!("file://{HOST}/{name}.purs");
        let id = files.insert(path, content);

        engine.set_content(id, content);
        engine.set_module_file(name, id);
    }
}

static TEMPORARY_DIRECTORY: OnceLock<TempDir> = OnceLock::new();

pub fn handle_generated(mut uri: Url, content: &str) -> Option<Url> {
    if uri.host_str() != Some(HOST) {
        return Some(uri);
    }

    let directory = TEMPORARY_DIRECTORY.get_or_init(|| {
        Builder::new()
            .prefix("purescript-analyzer-")
            .tempdir()
            .expect("invariant violated: failed to create TEMPORARY_DIRECTORY")
    });

    uri.set_host(Some("localhost")).ok()?;
    let original = uri.to_file_path().ok()?;

    let file = original.components().last()?;
    let path = directory.path().join(file);

    let mut file = File::create(&path).ok()?;
    write!(file, "{content}").ok()?;

    Url::from_file_path(path).ok()
}
