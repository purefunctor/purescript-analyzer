use std::path::{Path, PathBuf};

use glob::glob;

pub fn all_source_files() -> Vec<PathBuf> {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let pattern = format!("{}/packages/**/*.purs", manifest.to_str().unwrap());
    glob(&pattern).unwrap().filter_map(Result::ok).collect()
}
