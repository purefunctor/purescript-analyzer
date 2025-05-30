use itertools::Itertools;
use spago::lockfile::Lockfile;

const SPAGO_LOCK: &str = include_str!("./fixture/spago.lock");

#[test]
fn test_parse_lockfile() {
    let lockfile = serde_json::from_str::<Lockfile>(SPAGO_LOCK);
    assert!(lockfile.is_ok(), "{:?}", lockfile);
}

#[test]
fn test_lockfile_sources() {
    let lockfile = serde_json::from_str::<Lockfile>(SPAGO_LOCK);
    assert!(lockfile.is_ok(), "{:?}", lockfile);

    let lockfile = lockfile.unwrap();
    let sources = lockfile
        .sources()
        .sorted()
        .filter_map(|source| {
            let source = source.to_str()?;
            Some(source.to_string())
        })
        .join("\n");

    insta::assert_snapshot!(sources);
}
