use std::sync::Arc;

use demo::{source::SourceDatabase, RootDatabase, surface::SurfaceDatabase};
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

    let file_id = fs.file_id("./Main.purs".into()).unwrap();

    // let node = db.parse_file(file_id).syntax.clone();
    // let _: ast::Module = ast::Source::cast(node).unwrap().child().unwrap();

    for value_declaration in db.module_surface(file_id).value_declarations() {
        let _ = db.lookup_intern_value_declaration(*value_declaration);
    }

    /*
    
    Question:

    AstPtr -> DeclarationId

    Given an AstPtr, how do we find its corresponding DeclarationId.
    One way is by searching the module surface and determining if
    one declaration encapsulates this specific pointer. For instance
    if we had a pointer to an expression, we can ascend to its ancestors
    to determine the declaration it belongs to.

    Then, we can scan the value declarations available in the surface
    to figure out which of them match with a specific pointer, since
    we store the InFileAstId information which can then be turned into
    the corresponding AstPtr, which can then be compared to.

     */

}
