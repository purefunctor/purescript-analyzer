use std::sync::Arc;

use demo::{source::SourceDatabase, surface::SurfaceDatabase, RootDatabase};
use files::{ChangedFile, Files};
use rowan::ast::AstNode;
use syntax::ast;

#[test]
fn server_loop() {
    let mut db = RootDatabase::default();
    let mut fs = Files::default();

    fs.set_file_contents(
        "./Main.purs".into(),
        Some(
            "module Hello.World where
a = [0, 1, 2]"
                .into(),
        ),
    );
    if fs.has_changes() {
        for ChangedFile { file_id, .. } in fs.take_changes() {
            let contents = std::str::from_utf8(fs.file_contents(file_id)).unwrap();
            db.set_file_source(file_id, Arc::from(contents));
        }
    }

    // Basic demo of how the `declaration_map` query can be used to obtain
    // stable IDs for each declaration. These stable IDs are then used as
    // keys for lowering and type checking.

    let file_id = fs.file_id("./Main.purs".into()).unwrap();

    let node = db.parse_file(file_id).syntax.clone();
    let module: ast::Module = ast::Source::cast(node).unwrap().child().unwrap();

    for declaration in module.body().unwrap().declarations().unwrap().children() {
        let declaration_id = db.declaration_map(file_id).lookup(&declaration).in_file(file_id);
        dbg!(declaration_id);
    }
}
