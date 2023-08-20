use std::sync::Arc;

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;

pub(crate) type ModuleId = Idx<Arc<str>>;

#[derive(Debug, Default)]
pub(crate) struct ModuleMap {
    inner: Arena<Arc<str>>,
    entries: FxHashMap<Arc<str>, ModuleId>,
}

impl ModuleMap {
    pub(crate) fn allocate(&mut self, module_name: Arc<str>) -> ModuleId {
        if let Some(module_id) = self.entries.get(&module_name) {
            *module_id
        } else {
            let module_id = self.inner.alloc(module_name.clone());
            self.entries.insert(module_name, module_id);
            module_id
        }
    }

    pub(crate) fn get(&self, module_id: ModuleId) -> Arc<str> {
        self.inner[module_id].clone()
    }
}
