//! Database for source file information.

use std::{path::PathBuf, sync::Arc};

use files::FileId;
use parsing::error::ParseError;
use syntax::SyntaxNode;

#[salsa::query_group(SourceStorage)]
pub trait SourceDatabase {
    #[salsa::input]
    fn file_contents(&self, file_id: FileId) -> Arc<str>;

    #[salsa::input]
    fn file_paths(&self) -> Arc<[(FileId, PathBuf)]>;

    #[salsa::transparent]
    fn parse_file(&self, file_id: FileId) -> SyntaxNode;

    #[salsa::transparent]
    fn parse_file_with_errors(&self, file_id: FileId) -> (SyntaxNode, Arc<[ParseError]>);
}

fn parse_file(db: &dyn SourceDatabase, file_id: FileId) -> SyntaxNode {
    db.parse_file_with_errors(file_id).0
}

fn parse_file_with_errors(
    db: &dyn SourceDatabase,
    file_id: FileId,
) -> (SyntaxNode, Arc<[ParseError]>) {
    let source = db.file_contents(file_id);
    let (node, errors) = parsing::parse_module(&source);
    (node, errors.into())
}
