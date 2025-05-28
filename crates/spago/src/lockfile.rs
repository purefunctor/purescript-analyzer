use std::path::{Path, PathBuf};

use rustc_hash::FxHashMap;
use serde::Deserialize;
use smol_str::SmolStr;

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
        let workspace = self.workspace.packages.values().flat_map(|package| {
            let path = package.path.strip_prefix("./").unwrap_or(&package.path);
            let src = Path::new(&path).join("src/**/*.purs");
            let test = Path::new(&path).join("test/**/*.purs");
            vec![src, test]
        });

        let packages = self.packages.iter().flat_map(|(name, package)| match package {
            PackageEntry::Git { rev } => {
                let src = Path::new(".spago/p").join(name).join(rev).join("src/**/*.purs");
                let test = Path::new(".spago/p").join(name).join(rev).join("test/**/*.purs");
                [src, test]
            }
            PackageEntry::Registry { version } => {
                let src = Path::new(".spago/p")
                    .join(format!("{}-{}", name, version))
                    .join("src/**/*.purs");
                let test = Path::new(".spago/p")
                    .join(format!("{}-{}", name, version))
                    .join("test/**/*.purs");
                [src, test]
            }
        });

        workspace.chain(packages)
    }
}
