//! See documentation for [`ModuleMap`].

use std::sync::Arc;

use files::FileId;
use la_arena::{Arena, ArenaMap, Idx};
use rowan::ast::AstNode;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{names::ModuleName, ResolverDatabase};

pub type ModuleId = Idx<ModuleName>;

/// Assigns [`ModuleId`]s to [`ModuleName`]s.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct ModuleMap {
    inner: Arena<ModuleName>,
    name_to_id: FxHashMap<ModuleName, ModuleId>,
    id_to_file: ArenaMap<ModuleId, FileId>,
}

// FIXME: Take into account repeated modules, and report them as errors.
impl ModuleMap {
    pub(crate) fn module_map_query(db: &dyn ResolverDatabase) -> Arc<ModuleMap> {
        let mut module_map = ModuleMap::default();
        for (file_id, _) in db.file_paths().iter() {
            let node = db.parse_file(*file_id);
            let module_name = ast::Source::<ast::Module>::cast(node)
                .and_then(|source| source.child()?.header()?.name())
                .and_then(|module_name| ModuleName::try_from(module_name).ok());
            if let Some(module_name) = module_name {
                let module_id = module_map.inner.alloc(module_name.clone());
                module_map.name_to_id.insert(module_name, module_id);
                module_map.id_to_file.insert(module_id, *file_id);
            }
        }
        Arc::new(module_map)
    }

    pub fn module_id(&self, module_name: ModuleName) -> Option<ModuleId> {
        self.name_to_id.get(&module_name).copied()
    }

    pub fn file_id(&self, module_id: ModuleId) -> Option<FileId> {
        self.id_to_file.get(module_id).cloned()
    }
}
