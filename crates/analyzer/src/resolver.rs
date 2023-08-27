//! Database for resolving information.
//!
//! In particular, the [`ResolverDatabase`] indexes information about source
//! files to facilitate processes such as name resolution, as in finding the
//! corresponding [`FileId`] for a [`ModuleName`], or assigning stable IDs 
//! [`AstId`] to positional information [`AstPtr`].
//!
//! [`AstPtr`]: rowan::ast::AstPtr
//! [`AstId`]: crate::id::AstId

use std::sync::Arc;

use files::FileId;
use rowan::ast::AstNode;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{names::ModuleName, SourceDatabase};

#[salsa::query_group(ResolverStorage)]
pub trait ResolverDatabase: SourceDatabase {
    fn module_map(&self) -> Arc<FxHashMap<ModuleName, FileId>>;
}

fn module_map(db: &dyn ResolverDatabase) -> Arc<FxHashMap<ModuleName, FileId>> {
    let mut module_map = FxHashMap::default();
    for (file_id, _) in db.file_paths().iter() {
        let node = db.parse_file(*file_id);
        let module_name = ast::Source::<ast::Module>::cast(node)
            .and_then(|source| source.child()?.header()?.name())
            .map(ModuleName::from);
        if let Some(module_name) = module_name {
            module_map.insert(module_name, *file_id);
        }
    }
    Arc::new(module_map)
}
