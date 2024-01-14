use std::{fmt::Write, fs, sync::Arc};

use files::{ChangedFile, FileId, Files};
use insta::glob;
use salsa::Durability;

use crate::{
    id::InFile, infer, InferDatabase, ResolverDatabase, RootDatabase, SourceDatabase,
    SugarDatabase, SurfaceDatabase,
};

use super::PrettyPrinter;

fn default_db(source: &str) -> (RootDatabase, Files, FileId) {
    let mut db = RootDatabase::default();
    let mut files = Files::default();

    files.set_file_contents("./Main.purs".into(), Some(source.into()));

    for ChangedFile { file_id, .. } in files.take_changes() {
        let contents = files.file_contents(file_id);
        db.set_file_contents(file_id, Arc::from(std::str::from_utf8(contents).unwrap()));
    }

    db.set_file_paths_with_durability(files.iter().collect(), Durability::HIGH);

    let file_id = files.file_id("./Main.purs".into()).unwrap();

    (db, files, file_id)
}

fn expect_infer_value(source: &str) {
    let (db, _, file_id) = default_db(source);

    let nominal_map = db.nominal_map(file_id);
    let pp = infer::PrettyPrinter::new(&db);

    let mut results = String::new();
    let binding_groups = db.binding_groups(file_id);
    for (binding_group_id, _) in binding_groups.iter() {
        for (value_group_id, value_group_ty) in db.infer_binding_group(binding_group_id).iter() {
            let name = nominal_map
                .value_group_data(InFile { file_id, value: value_group_id })
                .name
                .as_ref();
            writeln!(&mut results, "{} :: {}", name, pp.ty(value_group_ty.as_type()).pretty(80))
                .unwrap();
        }
    }

    insta::assert_snapshot!(results);
}

fn expect_infer_data(source: &str) {
    let (db, _, file_id) = default_db(source);

    let pp = PrettyPrinter::new(&db);
    let nominal_map = db.nominal_map(file_id);

    let mut results = String::new();
    for (data_group_id, _) in nominal_map.data_groups() {
        let data_data = db.data_surface(data_group_id);
        let data_ty = db.infer_data_group(data_group_id);
        for (constructor_id, constructor_ty) in data_ty.iter() {
            let constructor_data = data_data.value.declaration.get_constructor(constructor_id);
            writeln!(
                &mut results,
                "{} :: {}",
                constructor_data.name,
                pp.ty(constructor_ty).pretty(80)
            )
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

#[test]
fn test_infer_data() {
    glob!("tests/inputs/passing/data", "*.purs", |path| {
        insta::with_settings!({ snapshot_path => "tests/snapshots", omit_expression => true }, {
            let source = fs::read_to_string(path).unwrap();
            expect_infer_data(&source);
        });
    })
}

#[test]
fn api_test() {
    let (mut db, mut files, file_id) = default_db(
        "module B (List(..)) where
data List a = Cons a (List a) | Nil
",
    );

    files.set_file_contents(
        "./A.purs".into(),
        Some(
            "module A where
import B (List(..)) as B

data X = X

a = X"
                .into(),
        ),
    );
    for ChangedFile { file_id, .. } in files.take_changes() {
        let contents = files.file_contents(file_id);
        db.set_file_contents(file_id, Arc::from(std::str::from_utf8(contents).unwrap()));
    }
    db.set_file_paths_with_durability(files.iter().collect(), Durability::HIGH);

    let pp = infer::PrettyPrinter::new(&db);

    let a_file_id = files.file_id("./A.purs".into()).unwrap();
    // dbg!(db.parse_file(a_file_id));
    // dbg!(db.module_imports(a_file_id));

    let nominal_map = db.nominal_map(a_file_id);
    let binding_groups = db.binding_groups(a_file_id);
    for (binding_group_id, _) in binding_groups.iter() {
        for (value_group_id, value_group_ty) in db.infer_binding_group(binding_group_id).iter() {
            let name = nominal_map
                .value_group_data(InFile { file_id, value: value_group_id })
                .name
                .as_ref();
            println!("{} :: {}", name, pp.ty(value_group_ty.as_type()).pretty(80));
        }
    }
}
