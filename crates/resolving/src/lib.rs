use std::sync::Arc;

use files::FileId;
use indexing::FullModuleIndex;
use lowering::FullModuleLower;

pub trait External {
    fn lookup_module_name(&self, name: &str) -> Option<FileId>;

    fn index(&mut self, id: FileId) -> Arc<FullModuleIndex>;
}

pub fn resolve(external: &mut impl External, index: &FullModuleIndex, lower: &FullModuleLower) {
    for (_, deferred) in lower.graph.deferred() {
        let Some(q) = &deferred.qualifier else { continue };
        let Some(n) = &deferred.name else { continue };

        let alias = q.trim_end_matches('.');
        let Some(ids) = index.index.lookup_import_alias(alias) else { continue };

        for &id in ids {
            let Some(name) = index.index.import_module_name(id) else { continue };
            let Some(file) = external.lookup_module_name(name) else { continue };

            let index = external.index(file);
            dbg!(index.index.lookup_term_item(n));
        }

        // Search the index using the alias, which yields the ImportId
        // Given the ImportId, take the module name of the import
        // Given the module name of the import, get the FileId
        // Finally, we can start resolving the names!

        // Cool, but how do we implement resolution from ModuleName
        // to FileId? We could make it such that Files is an input
        // to the query system, iterate over it, then use the parse
        // query to obtain the module name for each file.
        //
        // The alternative solution is to implement resolution
        // outside of the query engine, keeping files as a non-input
        // in the system and only putting the module graph as an input
        //
        // A decision that we made early on as well was to make sure
        // that the query engine explicitly does not handle cycle
        // detection, and that it's up to the caller to maintain this
        // invariant.
        //
        // Basically, here's how it'd work
        //
        // Server -> Driver -> Engine
        //
        // Wait, is resolution susceptible to cycles in the first place?
        // Indexes only depend on the current module to be built, while
        // global resolution, even with exports, only ever depend on said
        // acyclic indexes. Global resolution for between module is completely
        // independent!
        //
        // Consider the following case
        //
        // module Lib (module Internal) where
        //
        // import Internal (internal) as Internal
        //
        // Suppose that we import `internal` through `Lib`, we don't immediately
        // see that `Lib` exports it but we'll see that we have some re-exports.
        // What's interesting here is that we're effectively treating the module
        // with the exports as an extension of the current module's import list
        //
        // When resolving `internal`, we can just use the functions exposed by
        // Lib's Index.
    }
}
