//! An index from module names to file IDs.

use std::{borrow::Borrow, sync::Arc};

use files::FileId;
use rowan::ast::AstNode;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{interner::InDb, IndexDatabase};

/// Associates module names to [`FileId`]s and vice-versa.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct ModuleMap {
    name_to_file: FxHashMap<Arc<str>, FileId>,
    file_to_name: FxHashMap<FileId, Arc<str>>,
}

impl ModuleMap {
    pub fn file_id(&self, module_name: impl Borrow<str>) -> Option<FileId> {
        self.name_to_file.get(module_name.borrow()).copied()
    }

    pub fn module_name(&self, file_id: FileId) -> Option<Arc<str>> {
        self.file_to_name.get(&file_id).cloned()
    }
}

pub(crate) fn module_map_query(db: &dyn IndexDatabase) -> Arc<ModuleMap> {
    let mut module_map = ModuleMap::default();
    for (file_id, _) in db.file_paths().iter() {
        let node = db.parse_file(*file_id);
        let module_name = ast::Source::<ast::Module>::cast(node)
            .and_then(|source| source.child()?.header()?.name()?.in_db(db));
        if let Some(module_name) = module_name {
            module_map.name_to_file.insert(Arc::clone(&module_name), *file_id);
            module_map.file_to_name.insert(*file_id, module_name);
        }
    }
    Arc::new(module_map)
}
