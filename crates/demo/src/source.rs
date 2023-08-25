//! Queries related to a file's contents.

use std::sync::Arc;

use files::FileId;
use itertools::Itertools;
use parsing::{error::ParseError, parse_module};
use rowan::ast::AstNode;
use rustc_hash::FxHashMap;
use syntax::{ast, SyntaxNode};

#[derive(Debug, PartialEq, Eq)]
pub struct ParseResult {
    pub syntax: SyntaxNode,
    pub errors: Vec<ParseError>,
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct ModuleMap {
    inner: FxHashMap<String, FileId>,
}

impl ModuleMap {
    pub fn get_file_id(&self, module_name: &str) -> Option<FileId> {
        self.inner.get(module_name).cloned()
    }
}

#[salsa::query_group(SourceStorage)]
pub trait SourceDatabase {
    #[salsa::input]
    fn file_source(&self, file_id: FileId) -> Arc<str>;

    #[salsa::input]
    fn file_ids(&self) -> Arc<[FileId]>;

    #[salsa::transparent]
    fn parse_file(&self, file_id: FileId) -> Arc<ParseResult>;

    fn module_map(&self) -> Arc<ModuleMap>;
}

fn parse_file(db: &dyn SourceDatabase, file_id: FileId) -> Arc<ParseResult> {
    let source = db.file_source(file_id);
    let (syntax, errors) = parse_module(&source);
    Arc::new(ParseResult { syntax, errors })
}

fn module_map(db: &dyn SourceDatabase) -> Arc<ModuleMap> {
    let mut module_map = ModuleMap::default();
    for file_id in db.file_ids().iter().cloned() {
        let node = db.parse_file(file_id).syntax.clone();
        let module: ast::Module = ast::Source::cast(node).unwrap().child().unwrap();
        let name = module
            .header()
            .unwrap()
            .name()
            .unwrap()
            .children()
            .map(|name| name.as_str().unwrap())
            .join(".");
        module_map.inner.insert(name, file_id);
    }
    Arc::new(module_map)
}
