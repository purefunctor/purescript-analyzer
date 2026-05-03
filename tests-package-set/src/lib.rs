use std::collections::HashSet;
use std::fs;
use std::hint::black_box;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, Instant};

use building::{QueryEngine, prim};
use files::{FileId, Files};
use globset::Glob;
use rayon::prelude::*;
use walkdir::WalkDir;

const CORE_JSON: &str = include_str!("../purescript-core.json");
const ACME_JSON: &str = include_str!("../purescript-acme.json");

pub fn all_source_files() -> Vec<PathBuf> {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let packages = manifest.join("packages");

    let mut source_files = Vec::new();
    let Ok(entries) = fs::read_dir(&packages) else {
        return source_files;
    };

    for entry in entries.flatten() {
        if !entry.file_type().map(|t| t.is_dir()).unwrap_or(false) {
            continue;
        }
        let entry = entry.path();
        source_files.extend(package_purs_files(&entry));
    }

    source_files
}

fn package_purs_files(pkg_dir: &Path) -> impl Iterator<Item = PathBuf> {
    [pkg_dir.join("src"), pkg_dir.join("test")].into_iter().flat_map(find_purs_files)
}

fn find_purs_files(root: impl AsRef<Path>) -> impl Iterator<Item = PathBuf> {
    let matcher = Glob::new("**/*.purs").unwrap().compile_matcher();
    WalkDir::new(root).into_iter().filter_map(move |entry| {
        let entry = entry.ok()?;
        let path = entry.path();
        if matcher.is_match(path) { Some(path.to_path_buf()) } else { None }
    })
}

pub fn core_source_files() -> Vec<PathBuf> {
    source_files_for(&parse_package_list(CORE_JSON))
}

pub fn acme_source_files() -> Vec<PathBuf> {
    source_files_for(&parse_package_list(ACME_JSON))
}

fn parse_package_list(text: &str) -> HashSet<String> {
    let names: Vec<String> =
        serde_json::from_str(text).expect("package list must be a JSON array of strings");
    names
        .into_iter()
        .filter_map(|name| name.strip_prefix("purescript-").map(str::to_owned))
        .collect()
}

fn source_files_for(names: &HashSet<String>) -> Vec<PathBuf> {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let packages = manifest.join("packages");

    let mut source_files = vec![];
    let entries = match fs::read_dir(&packages) {
        Ok(entries) => entries,
        Err(_) => return source_files,
    };

    for entry in entries.flatten() {
        if !entry.file_type().map(|t| t.is_dir()).unwrap_or(false) {
            continue;
        }

        let directory = entry.file_name().to_string_lossy().into_owned();
        let Some((package_name, _)) = directory.rsplit_once('-') else { continue };

        if !names.contains(package_name) {
            continue;
        }

        let entry = entry.path();
        source_files.extend(package_purs_files(&entry));
    }

    source_files
}

pub fn load_sources(paths: Vec<PathBuf>) -> Arc<[(String, String)]> {
    let sources = paths.iter().filter_map(|path| {
        let content = fs::read_to_string(path).ok()?;
        let uri = path.to_string_lossy().into_owned();
        Some((uri, content))
    });
    sources.collect()
}

pub struct WarmedEngine {
    pub engine: QueryEngine,
    pub candidates: Vec<FileId>,
}

pub fn build_warmed_engine(sources: &[(String, String)]) -> WarmedEngine {
    let mut engine = QueryEngine::default();
    let mut files = Files::default();
    prim::configure(&mut engine, &mut files);

    let mut candidates = vec![];
    for (uri, content) in sources {
        let file_id = files.insert(uri.as_str(), content.as_str());
        engine.set_content(file_id, content.as_str());
        candidates.push(file_id);
    }

    for &file_id in &candidates {
        if let Ok((parsed, _)) = engine.parsed(file_id) {
            if let Some(module_name) = parsed.module_name() {
                engine.set_module_file(&module_name.to_string(), file_id);
            }
        }
    }

    candidates.par_iter().for_each(|&file_id| {
        let snapshot = engine.snapshot();
        let _ = snapshot.parsed(file_id);
        let _ = snapshot.stabilized(file_id);
        let _ = snapshot.indexed(file_id);
        let _ = snapshot.resolved(file_id);
        let _ = snapshot.lowered(file_id);
        let _ = snapshot.grouped(file_id);
        let _ = snapshot.bracketed(file_id);
        let _ = snapshot.sectioned(file_id);
    });

    WarmedEngine { engine, candidates }
}

pub fn run_single_core(warmed: &WarmedEngine) {
    for &file_id in &warmed.candidates {
        let _ = black_box(warmed.engine.checked(file_id));
    }
}

pub fn run_multi_core(warmed: &WarmedEngine) {
    let engine = &warmed.engine;
    warmed.candidates.par_iter().for_each(|&file_id| {
        let snapshot = engine.snapshot();
        let _ = black_box(snapshot.checked(file_id));
    });
}

pub fn probe_iter_time(
    sources: &[(String, String)],
    routine: impl FnOnce(&WarmedEngine),
) -> Duration {
    let warmed = build_warmed_engine(sources);
    let start = Instant::now();
    routine(&warmed);
    start.elapsed()
}
