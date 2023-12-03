use std::{fmt::Write, fs, sync::Arc};

use files::{ChangedFile, Files};
use insta::glob;
use salsa::Durability;

use crate::{
    infer::{self, InferResult},
    InferDatabase, ResolverDatabase, RootDatabase, SourceDatabase,
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
    for (value_group_id, value_group_data) in nominal_map.value_groups() {
        let name = value_group_data.name.as_ref();
        let infer_result = db.infer_value(value_group_id);
        match *infer_result {
            InferResult::Complete(t) => {
                writeln!(&mut results, "complete({}) :: {}", name, pp.ty(t).pretty(80)).unwrap();
            }
            InferResult::Incomplete(t, _) => {
                writeln!(&mut results, "incomplete({}) :: {}", name, pp.ty(t).pretty(80)).unwrap();
            }
            InferResult::Recursive => {
                writeln!(&mut results, "recursive({})", name).unwrap();
            }
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
