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
fn test_lockfile_sources_subdir_precedence_packages_over_workspace_extra_packages() {
    let lockfile = serde_json::from_str::<Lockfile>(
        r#"{
  "workspace": {
    "packages": {},
    "extra_packages": {
      "foo": { "subdir": "from-workspace" }
    }
  },
  "packages": {
    "foo": { "type": "git", "rev": "abcd", "subdir": "from-packages" }
  }
}"#,
    )
    .unwrap();

    let sources = lockfile
        .sources()
        .filter_map(|source| source.to_str().map(|s| s.replace('\\', "/")))
        .collect::<Vec<_>>();

    assert!(
        sources.iter().any(|s| s == ".spago/p/foo/abcd/src"),
        "sources: {sources:?}"
    );
    assert!(
        sources.iter().any(|s| s == ".spago/p/foo/abcd/test"),
        "sources: {sources:?}"
    );

    assert!(
        sources
            .iter()
            .any(|s| s == ".spago/p/foo/abcd/from-packages/src"),
        "sources: {sources:?}"
    );
    assert!(
        sources
            .iter()
            .any(|s| s == ".spago/p/foo/abcd/from-packages/test"),
        "sources: {sources:?}"
    );
    assert!(
        sources
            .iter()
            .all(|s| s != ".spago/p/foo/abcd/from-workspace/src"),
        "sources: {sources:?}"
    );
    assert!(
        sources
            .iter()
            .all(|s| s != ".spago/p/foo/abcd/from-workspace/test"),
        "sources: {sources:?}"
    );
}

#[test]
fn test_lockfile_sources_subdir_fallback_to_workspace_extra_packages() {
    let lockfile = serde_json::from_str::<Lockfile>(
        r#"{
  "workspace": {
    "packages": {},
    "extra_packages": {
      "deku-core": { "subdir": "deku-core" }
    }
  },
  "packages": {
    "deku-core": { "type": "git", "rev": "65d6e9d" }
  }
}"#,
    )
    .unwrap();

    let sources = lockfile
        .sources()
        .filter_map(|source| source.to_str().map(|s| s.replace('\\', "/")))
        .collect::<Vec<_>>();

    assert!(
        sources
            .iter()
            .any(|s| s == ".spago/p/deku-core/65d6e9d/deku-core/src"),
        "sources: {sources:?}"
    );
    assert!(
        sources
            .iter()
            .any(|s| s == ".spago/p/deku-core/65d6e9d/deku-core/test"),
        "sources: {sources:?}"
    );
    assert!(
        sources
            .iter()
            .all(|s| s != ".spago/p/deku-core/65d6e9d/from-packages/src"),
        "sources: {sources:?}"
    );
    assert!(
        sources
            .iter()
            .all(|s| s != ".spago/p/deku-core/65d6e9d/from-packages/test"),
        "sources: {sources:?}"
    );
}

#[test]
fn test_parse_lockfile_without_extra_packages() {
    let lockfile = serde_json::from_str::<Lockfile>(
        r#"{
  "workspace": { "packages": {} },
  "packages": { "foo": { "type": "git", "rev": "abcd" } }
}"#,
    );
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
        let uri = url.to_string().replace(&manifest_directory_uri, "./spago");
        writeln!(&mut snapshot, "{}", uri).unwrap();
    });

    insta::assert_snapshot!(snapshot);
}
