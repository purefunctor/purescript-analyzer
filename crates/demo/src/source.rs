//! Queries related to a file's contents.

use std::sync::Arc;

use files::FileId;
use parsing::{error::ParseError, parse_module};
use syntax::SyntaxNode;

#[derive(Debug, PartialEq, Eq)]
pub struct ParseResult {
    pub syntax: SyntaxNode,
    pub errors: Vec<ParseError>,
}

#[salsa::query_group(SourceStorage)]
pub trait SourceDatabase {
    #[salsa::input]
    fn file_source(&self, file_id: FileId) -> Arc<str>;

    #[salsa::transparent]
    fn parse_file(&self, file_id: FileId) -> Arc<ParseResult>;
}

fn parse_file(db: &dyn SourceDatabase, file_id: FileId) -> Arc<ParseResult> {
    dbg!("parse_file is called...");
    let source = db.file_source(file_id);
    let (syntax, errors) = parse_module(&source);
    Arc::new(ParseResult { syntax, errors })
}
