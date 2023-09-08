//! The core of the analyzer.

pub mod id;
pub mod lower;
pub mod names;
pub mod resolver;
pub mod source;

pub use resolver::ResolverDatabase;
pub use source::SourceDatabase;

/// The analyzer's core database.
#[derive(Default)]
#[salsa::database(resolver::ResolverStorage, lower::LowerStorage, source::SourceStorage)]
pub struct RootDatabase {
    storage: salsa::Storage<RootDatabase>,
}

impl salsa::Database for RootDatabase {}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use files::{ChangedFile, Files};
    use rowan::ast::AstNode;
    use salsa::Durability;
    use syntax::ast;

    use crate::{lower::LowerDatabase, ResolverDatabase, RootDatabase, SourceDatabase};

    #[test]
    fn api() {
        // Initialization Code
        let mut db = RootDatabase::default();
        let mut files = Files::default();

        // Given the source file glob, we take all purs files and load them onto the file system.
        files.set_file_contents(
            "./Main.purs".into(),
            Some("module Main where\n\nimport Hello as H".into()),
        );
        files.set_file_contents(
            "./Hello.purs".into(),
            Some("module Hello where\n\nhello = M.fromMaybe".into()),
        );
        // Then, we feed it to the database through the `take_changes` method.
        for ChangedFile { file_id, .. } in files.take_changes() {
            let contents = files.file_contents(file_id);
            db.set_file_contents(file_id, Arc::from(std::str::from_utf8(contents).unwrap()));
        }
        // Finally, we provide the file paths to the database for use by queries like `module_map`.
        // Note that we're setting the durability to medium as we don't expect file paths to change
        // as often as something like editing a file would with the file contents.
        db.set_file_paths_with_durability(files.iter().collect(), Durability::MEDIUM);

        let file_id = files.file_id("./Hello.purs".into()).unwrap();
        let source = ast::Source::<ast::Module>::cast(db.parse_file(file_id)).unwrap();
        dbg!(&source);
        let declaration = source
            .child()
            .unwrap()
            .body()
            .unwrap()
            .declarations()
            .unwrap()
            .children()
            .next()
            .unwrap();
        if let ast::Declaration::ValueDeclaration(value_declaration) = declaration {
            let declaration_id =
                db.positional_map(file_id).ast_id(&value_declaration).in_file(file_id);
            dbg!(&db.lower_value_declaration(declaration_id));
        }
    }
}
