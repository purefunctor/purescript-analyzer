use std::sync::Arc;

use demo::{source::SourceDatabase, RootDatabase};
use files::{ChangedFile, Files};

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

    let _ = fs.file_id("./Main.purs".into()).unwrap();

    //     let node = db.parse_file(file_id).syntax.clone();
    //     let module: ast::Module = ast::Source::cast(node).unwrap().child().unwrap();

    //     let ast_id = {
    //         let ast_ptr = module.body().unwrap().declarations().unwrap().children().next().unwrap();
    //         db.declaration_map(file_id).lookup(&ast_ptr).in_file(file_id)
    //     };

    //     dbg!("Starting demo...");
    //     db.type_infer_declaration(ast_id);

    //     db.set_file_source(
    //         file_id,
    //         "module Hello.World where

    // a = [0, 1, 2]
    // "
    //         .into(),
    //     );

    //     dbg!("declaration_map, lower_declaration, parse_file is refreshed...");
    //     db.type_infer_declaration(ast_id);
}
