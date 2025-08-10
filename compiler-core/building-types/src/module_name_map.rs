use rustc_hash::FxBuildHasher;
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

pub struct ModuleNameInterner(StringInterner<BucketBackend<ModuleNameId>, FxBuildHasher>);

impl Default for ModuleNameInterner {
    fn default() -> ModuleNameInterner {
        ModuleNameInterner(StringInterner::new())
    }
}

impl ModuleNameInterner {
    pub fn intern(&mut self, name: &str) -> ModuleNameId {
        self.0.get_or_intern(name)
    }

    pub fn lookup(&self, name: &str) -> Option<ModuleNameId> {
        self.0.get(name)
    }
}
