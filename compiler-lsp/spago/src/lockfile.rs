use std::path::{Component, Path, PathBuf};

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

    #[serde(default)]
    pub extra_packages: FxHashMap<SmolStr, ExtraPackage>,
}

#[derive(Debug, Deserialize)]
pub struct WorkspacePackage {
    pub path: PathBuf,
}

#[derive(Debug, Deserialize)]
pub struct ExtraPackage {
    pub subdir: Option<PathBuf>,

    /// Present in Spago lockfile schema for some extra-packages (e.g. local/path-based).
    /// Not currently used by `Lockfile::sources()`.
    pub path: Option<PathBuf>,
}

pub type Packages = FxHashMap<SmolStr, PackageEntry>;

#[derive(Debug, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum PackageEntry {
    Git {
        rev: SmolStr,
        #[serde(default)]
        subdir: Option<PathBuf>,
    },
    Local { path: SmolStr },
    Registry { version: SmolStr },
}

impl Lockfile {
    pub fn sources(&self) -> impl Iterator<Item = PathBuf> {
        let workspace = self.workspace.packages.values().flat_map(move |package| {
            let src = package.path.join("src");
            let test = package.path.join("test");
            [src, test]
        });

        let base = Path::new(".spago").join("p");
        let extra_packages = &self.workspace.extra_packages;

        let packages = self.packages.iter().flat_map(move |(name, package)| {
            let mut sources = Vec::new();
            match package {
                PackageEntry::Git { rev, subdir } => {
                    sources.push(base.join(name).join(rev).join("src"));
                    sources.push(base.join(name).join(rev).join("test"));

                    let subdir = subdir.as_ref().or_else(|| {
                        extra_packages
                            .get(name)
                            .and_then(|extra_package| extra_package.subdir.as_ref())
                    });

                    let subdir = subdir.filter(|subdir| is_safe_subdir(subdir));

                    if let Some(subdir) = subdir {
                        sources.push(base.join(name).join(rev).join(subdir).join("src"));
                        sources.push(base.join(name).join(rev).join(subdir).join("test"));
                    }
                }
                PackageEntry::Local { path } => {
                    let base = Path::new(path);
                    sources.push(base.join("src"));
                    sources.push(base.join("test"));
                }
                PackageEntry::Registry { version } => {
                    let name_version = format!("{name}-{version}");
                    sources.push(base.join(&name_version).join("src"));
                    sources.push(base.join(&name_version).join("test"));
                }
            }
            sources
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

fn is_safe_subdir(subdir: &Path) -> bool {
    if subdir.as_os_str().is_empty() {
        return false;
    }

    // Treat lockfile paths as untrusted input: only allow "normal" relative paths.
    !subdir.components().any(|component| match component {
        Component::Prefix(_) | Component::RootDir | Component::ParentDir | Component::CurDir => {
            true
        }
        Component::Normal(_) => false,
    })
}

fn find_purs_files(root: PathBuf) -> impl Iterator<Item = PathBuf> {
    let matcher = Glob::new("**/*.purs").unwrap().compile_matcher();
    WalkDir::new(root).into_iter().filter_map(move |entry| {
        let entry = entry.ok()?;
        let path = entry.path();
        if matcher.is_match(path) { Some(path.to_path_buf()) } else { None }
    })
}
