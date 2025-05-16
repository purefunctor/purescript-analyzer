use std::path::PathBuf;

use anyhow::Ok;
use globset::{Glob, GlobSet, GlobSetBuilder};
use walkdir::WalkDir;

fn source_glob() -> anyhow::Result<GlobSet> {
    let mut builder = GlobSetBuilder::new();

    let sources = std::fs::read_to_string(".spago/sources.txt")?;
    for source in sources.lines() {
        let glob = format!("**/{}", source);
        builder.add(Glob::new(&glob)?);
    }

    Ok(builder.build()?)
}

pub fn source_files() -> anyhow::Result<Vec<PathBuf>> {
    let glob = source_glob()?;
    let cwd = std::env::current_dir()?;
    Ok(WalkDir::new(cwd)
        .into_iter()
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.is_file() && glob.is_match(path) { Some(path.to_path_buf()) } else { None }
        })
        .collect())
}
