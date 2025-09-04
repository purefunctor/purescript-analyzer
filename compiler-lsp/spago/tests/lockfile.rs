use std::fmt::Write;
use std::path::Path;

use itertools::Itertools;
use spago::lockfile::Lockfile;

const SPAGO_LOCK: &str = include_str!("./fixture/spago.lock");

#[test]
fn test_parse_lockfile() {
    let lockfile = serde_json::from_str::<Lockfile>(SPAGO_LOCK);
    assert!(lockfile.is_ok(), "{lockfile:?}");
}

#[test]
fn test_lockfile_sources() {
    let lockfile = serde_json::from_str::<Lockfile>(SPAGO_LOCK);
    assert!(lockfile.is_ok(), "{lockfile:?}");

    let lockfile = lockfile.unwrap();
    let sources = lockfile
        .sources()
        .sorted()
        .filter_map(|source| {
            let source = source.to_str()?;
            let source = source.replace('\\', "/");
            Some(source.to_string())
        })
        .join("\n");

    insta::assert_snapshot!(sources);
}

#[test]
fn test_source_files() {
    let manifest_directory = env!("CARGO_MANIFEST_DIR");
    let manifest_directory_url = url::Url::from_file_path(manifest_directory).unwrap();
    let manifest_directory_uri = manifest_directory_url.to_string();

    let path = Path::new(manifest_directory);
    let fixture = path.join("tests/fixture");

    let mut snapshot = String::default();

    spago::source_files(fixture).unwrap().into_iter().for_each(|file| {
        let url = url::Url::from_file_path(file).unwrap();
        let uri = url.to_string();
        writeln!(&mut snapshot, "{}", uri.replace(&manifest_directory_uri, "./spago")).unwrap();
    });

    insta::assert_snapshot!(snapshot);
}
