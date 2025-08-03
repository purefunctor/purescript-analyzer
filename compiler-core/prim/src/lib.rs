use std::io::Write;

use tempfile::Builder;
use url::Url;

use building::QueryEngine;
use files::Files;

pub const SCHEME: &'static str = "generated";
pub const PRIM: &'static str = include_str!("Prim.purs");

pub fn configure(engine: &mut QueryEngine, files: &mut Files) {
    for (name, content) in [("Prim", PRIM)] {
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

    let mut temporary = Builder::new().prefix("purescript-analyzer").tempfile().ok()?;
    write!(temporary, "{content}").ok()?;

    let path = temporary.into_temp_path();
    let path = path.to_str()?;

    Url::parse(path).ok()
}
