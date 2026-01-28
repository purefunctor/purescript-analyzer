//! Loads PureScript packages into QueryEngine + Files.

use std::fs;
use std::path::Path;

use analyzer::{QueryEngine, prim};
use files::{FileId, Files};
use glob::glob;
use url::Url;

/// Loads all .purs files from a directory into the engine.
fn load_file(engine: &mut QueryEngine, files: &mut Files, path: &Path) -> Option<FileId> {
    let path = path.canonicalize().ok()?;
    let url = Url::from_file_path(&path).ok()?;
    let content = fs::read_to_string(&path).ok()?;
    let content = content.replace("\r\n", "\n");

    let uri = url.to_string();
    let id = files.insert(uri, content);
    let content = files.content(id);

    engine.set_content(id, content);
    if let Ok((parsed, _)) = engine.parsed(id) {
        if let Some(name) = parsed.module_name() {
            engine.set_module_file(&name, id);
        }
    }

    Some(id)
}

/// Loads all packages from a packages directory.
///
/// Returns (engine, files, file_ids) where file_ids contains all loaded .purs files.
pub fn load_packages(packages_dir: &Path) -> (QueryEngine, Files, Vec<FileId>) {
    let mut engine = QueryEngine::default();
    let mut files = Files::default();
    prim::configure(&mut engine, &mut files);

    let pattern = format!("{}/**/*.purs", packages_dir.display());
    let mut paths: Vec<_> = glob(&pattern).into_iter().flatten().filter_map(Result::ok).collect();
    paths.sort();

    let mut file_ids = Vec::new();
    for path in paths {
        if let Some(id) = load_file(&mut engine, &mut files, &path) {
            file_ids.push(id);
        }
    }

    (engine, files, file_ids)
}

/// Filters file IDs to only those belonging to a specific package directory.
pub fn filter_package_files(files: &Files, file_ids: &[FileId], package_dir: &Path) -> Vec<FileId> {
    file_ids
        .iter()
        .copied()
        .filter(|&id| {
            let uri = files.path(id);
            Url::parse(&uri)
                .ok()
                .and_then(|u| u.to_file_path().ok())
                .is_some_and(|p| p.starts_with(package_dir))
        })
        .collect()
}
