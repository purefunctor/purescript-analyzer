use std::{fmt::Write, fs, sync::Arc};

use files::{ChangedFile, Files};
use insta::glob;
use salsa::Durability;

use crate::{
    id::InFile, infer, InferDatabase, ResolverDatabase, RootDatabase, SourceDatabase, SugarDatabase,
};

fn expect_infer_value(source: &str) {
    let mut db = RootDatabase::default();
    let mut files = Files::default();

    files.set_file_contents("./Main.purs".into(), Some(source.into()));

    for ChangedFile { file_id, .. } in files.take_changes() {
        let contents = files.file_contents(file_id);
        db.set_file_contents(file_id, Arc::from(std::str::from_utf8(contents).unwrap()));
    }

    db.set_file_paths_with_durability(files.iter().collect(), Durability::HIGH);

    let file_id = files.file_id("./Main.purs".into()).unwrap();
    let nominal_map = db.nominal_map(file_id);
    let pp = infer::PrettyPrinter::new(&db);

    let mut results = String::new();
    let binding_groups = db.binding_groups(file_id);
    for (binding_group_id, _) in binding_groups.iter() {
        for (value_group_id, infer_value_group) in db.infer_binding_group(binding_group_id).iter() {
            let name = nominal_map
                .value_group_data(InFile { file_id, value: value_group_id })
                .name
                .as_ref();
            writeln!(&mut results, "{} :: {}", name, pp.ty(infer_value_group.as_type()).pretty(80))
                .unwrap();
        }
    }

    insta::assert_snapshot!(results);
}

#[test]
fn test_infer_values() {
    glob!("tests/inputs/passing/values", "*.purs", |path| {
        insta::with_settings!({ snapshot_path => "tests/snapshots", omit_expression => true }, {
            let source = fs::read_to_string(path).unwrap();
            expect_infer_value(&source);
        });
    });
}
