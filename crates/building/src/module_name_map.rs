//! See documentation for [`ModuleNameId`] and [`ModuleNameMap`].

use files::FileId;
use rustc_hash::{FxBuildHasher, FxHashMap};
use string_interner::{backend::BucketBackend, symbol::SymbolU32, StringInterner, Symbol};

/// A stable ID for module names.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleNameId(SymbolU32);

impl Symbol for ModuleNameId {
    fn try_from_usize(index: usize) -> Option<ModuleNameId> {
        SymbolU32::try_from_usize(index).map(ModuleNameId)
    }

    fn to_usize(self) -> usize {
        SymbolU32::to_usize(self.0)
    }
}

type Interner = StringInterner<BucketBackend<ModuleNameId>, FxBuildHasher>;

/// Assigns stable IDs to module names and maps them to files.
pub struct ModuleNameMap {
    interner: Interner,
    modules: FxHashMap<ModuleNameId, FileId>,
}

impl Default for ModuleNameMap {
    fn default() -> ModuleNameMap {
        let interner = Interner::new();
        let modules = FxHashMap::default();
        ModuleNameMap { interner, modules }
    }
}

impl ModuleNameMap {
    /// Intern a module name.
    pub fn intern(&mut self, name: &str) -> ModuleNameId {
        self.interner.get_or_intern(name)
    }

    /// Intern a module name with a file.
    pub fn intern_with_file(&mut self, name: &str, file: FileId) -> ModuleNameId {
        let id = self.intern(name);
        self.modules.insert(id, file);
        id
    }

    /// Look up the name of a module.
    pub fn module_name(&self, id: ModuleNameId) -> Option<&str> {
        self.interner.resolve(id)
    }

    /// Look up the ID of a module.
    pub fn module_id(&self, name: &str) -> Option<ModuleNameId> {
        self.interner.get(name)
    }

    /// Look up the file of a module.
    pub fn file_id(&self, id: ModuleNameId) -> Option<FileId> {
        self.modules.get(&id).copied()
    }
}
