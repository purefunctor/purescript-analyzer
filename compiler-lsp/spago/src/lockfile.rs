use std::path::{Path, PathBuf};

use globset::Glob;
use rustc_hash::FxHashMap;
use serde::Deserialize;
use smol_str::SmolStr;
use walkdir::WalkDir;

#[derive(Debug, Deserialize)]
pub struct Lockfile {
    pub workspace: Workspace,
    pub packages: Packages,
}

#[derive(Debug, Deserialize)]
pub struct Workspace {
    pub packages: FxHashMap<SmolStr, WorkspacePackage>,
}

#[derive(Debug, Deserialize)]
pub struct WorkspacePackage {
    pub path: PathBuf,
}

pub type Packages = FxHashMap<SmolStr, PackageEntry>;

#[derive(Debug, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum PackageEntry {
    Git { rev: SmolStr },
    Registry { version: SmolStr },
}

impl Lockfile {
    pub fn sources(&self) -> impl Iterator<Item = PathBuf> {
        let workspace = self.workspace.packages.values().flat_map(move |package| {
            let src = package.path.join("src");
            let test = package.path.join("test");
            [src, test]
        });

        let packages = self.packages.iter().flat_map(move |(name, package)| match package {
            PackageEntry::Git { rev } => {
                let src = Path::new(".spago/p").join(name).join(rev).join("src");
                let test = Path::new(".spago/p").join(name).join(rev).join("test");
                [src, test]
            }
            PackageEntry::Registry { version } => {
                let name_version = format!("{}-{}", name, version);
                let src = Path::new(".spago/p").join(&name_version).join("src");
                let test = Path::new(".spago/p").join(&name_version).join("test");
                [src, test]
            }
        });

        workspace.chain(packages)
    }

    pub fn walk(&self, root: impl AsRef<Path>) -> impl Iterator<Item = PathBuf> {
        self.sources().filter_map(with_root(root)).flat_map(find_purs_files)
    }
}

fn with_root(root: impl AsRef<Path>) -> impl Fn(PathBuf) -> Option<PathBuf> {
    move |source| root.as_ref().join(source).canonicalize().ok()
}

fn find_purs_files(root: PathBuf) -> impl Iterator<Item = PathBuf> {
    let matcher = Glob::new("**/*.purs").unwrap().compile_matcher();
    WalkDir::new(root).into_iter().filter_map(move |entry| {
        let entry = entry.ok()?;
        let path = entry.path();
        if matcher.is_match(path) { Some(path.to_path_buf()) } else { None }
    })
}
