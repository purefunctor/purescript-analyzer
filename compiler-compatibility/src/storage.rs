use std::{fs, io::Read, path::PathBuf};

use sha2::{Digest, Sha256};

use crate::{
    error::{CompatError, Result},
    layout::Layout,
};

pub fn tarball_url(name: &str, version: &str) -> String {
    format!("https://packages.registry.purescript.org/{}/{}.tar.gz", name, version)
}

pub fn fetch_tarball(name: &str, version: &str, layout: &Layout, no_cache: bool) -> Result<PathBuf> {
    let tarball_path = layout.tarball_cache_path(name, version);

    if !no_cache && tarball_path.exists() {
        return Ok(tarball_path);
    }

    fs::create_dir_all(&layout.cache_tarballs_dir)?;

    let url = tarball_url(name, version);
    let response = reqwest::blocking::get(&url)?;
    let bytes = response.bytes()?;

    let part_path = tarball_path.with_extension("tar.gz.part");
    fs::write(&part_path, &bytes)?;
    fs::rename(&part_path, &tarball_path)?;

    Ok(tarball_path)
}

pub fn verify_tarball(path: &PathBuf, expected_sha256: &str, name: &str, version: &str) -> Result<()> {
    use base64::{engine::general_purpose::STANDARD, Engine};

    let mut file = fs::File::open(path)?;
    let mut hasher = Sha256::new();
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    hasher.update(&buffer);
    let hash = hasher.finalize();
    let hash_b64 = STANDARD.encode(hash);

    let expected_b64 = expected_sha256.strip_prefix("sha256-").unwrap_or(expected_sha256);

    if hash_b64 != expected_b64 {
        return Err(CompatError::HashMismatch { name: name.to_string(), version: version.to_string() });
    }

    Ok(())
}
