use std::{fmt::Write, fs, sync::Arc};

use files::{ChangedFile, Files};
use indexmap::IndexSet;
use insta::glob;
use rowan::ast::AstNode;
use salsa::Durability;
use smol_str::SmolStr;
use syntax::ast;

use crate::{infer, InferDatabase, ResolverDatabase, RootDatabase, SourceDatabase};

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
    let node = db.parse_file(file_id);
    let declarations = ast::Source::<ast::Module>::cast(node)
        .and_then(|source| Some(source.child()?.body()?.declarations()?.children()))
        .unwrap();

    let names: IndexSet<SmolStr> = declarations
        .into_iter()
        .filter_map(|declaration| {
            if let ast::Declaration::ValueEquationDeclaration(equation) = declaration {
                equation.name()?.as_str()
            } else {
                None
            }
        })
        .collect();

    let nominal_map = db.nominal_map(file_id);
    let pp = infer::PrettyPrinter::new(&db);

    let mut results = String::new();
    for name in names {
        let name = name.as_ref();
        let id = nominal_map.value_group_id(name).unwrap();
        let (ty, _) = db.infer_value(id);
        writeln!(&mut results, "{} :: {}", name, pp.ty(ty).pretty(80)).unwrap();
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
