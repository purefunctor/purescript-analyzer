use std::path::{Path, PathBuf};

use walkdir::WalkDir;

use crate::error::VerifierError;

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub package: String,
    pub version: String,
    pub path: PathBuf,
    pub relative_path: PathBuf,
}

pub fn discover_sources(
    package: &str,
    version: &str,
    root: &Path,
) -> Result<Vec<SourceFile>, VerifierError> {
    let mut files = Vec::new();
    let src = root.join("src");
    if !src.exists() {
        return Ok(files);
    }

    for entry in WalkDir::new(&src).into_iter().filter_map(Result::ok) {
        if !entry.file_type().is_file() || entry.path().extension().is_none_or(|ext| ext != "purs")
        {
            continue;
        }
        let relative_path =
            entry.path().strip_prefix(root).expect("walked under root").to_path_buf();
        files.push(SourceFile {
            package: package.to_string(),
            version: version.to_string(),
            path: entry.path().to_path_buf(),
            relative_path,
        });
    }

    files.sort_by(|a, b| a.relative_path.cmp(&b.relative_path));
    Ok(files)
}

#[cfg(test)]
mod tests {
    use std::fs;

    use tempfile::tempdir;

    use super::discover_sources;

    #[test]
    fn finds_only_src_purs_files() {
        let dir = tempdir().unwrap();
        fs::create_dir_all(dir.path().join("src/Data")).unwrap();
        fs::create_dir_all(dir.path().join("test")).unwrap();
        fs::write(dir.path().join("src/Main.purs"), "").unwrap();
        fs::write(dir.path().join("src/Main.js"), "").unwrap();
        fs::write(dir.path().join("src/Data/Foo.purs"), "").unwrap();
        fs::write(dir.path().join("test/Main.purs"), "").unwrap();

        let files = discover_sources("pkg", "1.0.0", dir.path()).unwrap();
        let paths: Vec<_> = files
            .into_iter()
            .map(|file| file.relative_path.to_string_lossy().into_owned())
            .collect();

        assert_eq!(paths, ["src/Data/Foo.purs", "src/Main.purs"]);
    }
}
