use std::fs;

use git2::{build::RepoBuilder, FetchOptions, Repository};
use purescript_registry::FsRegistry;

use crate::{error::Result, layout::Layout};

const REGISTRY_URL: &str = "https://github.com/purescript/registry";
const REGISTRY_INDEX_URL: &str = "https://github.com/purescript/registry-index";

pub struct Registry {
    layout: Layout,
    reader: FsRegistry,
}

impl Registry {
    pub fn new(layout: Layout) -> Registry {
        let reader = FsRegistry::new(layout.registry_layout());
        Registry { layout, reader }
    }

    pub fn ensure_repos(&self, update: bool) -> Result<()> {
        fs::create_dir_all(&self.layout.repos_dir)?;

        self.ensure_repo(REGISTRY_URL, &self.layout.registry_dir, update)?;
        self.ensure_repo(REGISTRY_INDEX_URL, &self.layout.index_dir, update)?;

        Ok(())
    }

    fn ensure_repo(&self, url: &str, path: &std::path::Path, update: bool) -> Result<()> {
        if path.exists() {
            if update {
                let repo = Repository::open(path)?;
                let mut remote = repo.find_remote("origin")?;
                remote.fetch(&["master"], None, None)?;

                let fetch_head = repo.find_reference("FETCH_HEAD")?;
                let commit = repo.reference_to_annotated_commit(&fetch_head)?;
                let (analysis, _) = repo.merge_analysis(&[&commit])?;

                if analysis.is_fast_forward() || analysis.is_normal() {
                    let refname = "refs/heads/master";
                    let mut reference = repo.find_reference(refname)?;
                    reference.set_target(commit.id(), "pull: fast-forward")?;
                    repo.set_head(refname)?;
                    repo.checkout_head(Some(git2::build::CheckoutBuilder::default().force()))?;
                }
            }
        } else {
            let mut fetch_opts = FetchOptions::new();
            fetch_opts.depth(1);

            RepoBuilder::new().fetch_options(fetch_opts).clone(url, path)?;
        }

        Ok(())
    }

    pub fn layout(&self) -> &Layout {
        &self.layout
    }

    pub fn reader(&self) -> &FsRegistry {
        &self.reader
    }
}
