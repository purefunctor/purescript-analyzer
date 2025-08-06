use std::io::Write;

use tempfile::Builder;
use url::Url;

use building::QueryEngine;
use files::Files;

pub const SCHEME: &str = "generated";
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
        let path = format!("{SCHEME}://{name}.purs");
        let id = files.insert(path, content);

        engine.set_content(id, content);
        engine.set_module_file(name, id);
    }
}

pub fn handle_generated(uri: Url, content: &str) -> Option<Url> {
    if uri.scheme() != SCHEME {
        return Some(uri);
    }

    let mut temporary = Builder::new()
        .prefix("purescript-analyzer-")
        .disable_cleanup(true)
        .suffix(".purs")
        .tempfile()
        .ok()?;

    write!(temporary, "{content}").ok()?;

    let path = temporary.into_temp_path();
    let path = path.to_str()?;

    Url::from_file_path(path).ok()
}
