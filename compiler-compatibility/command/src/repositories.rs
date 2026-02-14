use std::fs;

use git2::build::RepoBuilder;
use git2::{FetchOptions, Repository};
use registry::RegistryLayout;

use crate::error::Result;

const REGISTRY_URL: &str = "https://github.com/purescript/registry";
const REGISTRY_INDEX_URL: &str = "https://github.com/purescript/registry-index";

pub fn ensure_repositories(layout: &RegistryLayout, update: bool) -> Result<()> {
    if let Some(parent) = layout.registry_dir.parent() {
        fs::create_dir_all(parent)?;
    }

    ensure_repository(REGISTRY_URL, &layout.registry_dir, update)?;
    ensure_repository(REGISTRY_INDEX_URL, &layout.index_dir, update)?;

    Ok(())
}

fn ensure_repository(url: &str, path: &std::path::Path, update: bool) -> Result<()> {
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
