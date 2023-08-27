//! Database for resolving information.
//!
//! In particular, the [`ResolverDatabase`] indexes information about source
//! files to facilitate processes such as name resolution, as in finding the
//! corresponding [`FileId`] for a [`ModuleName`], or assigning stable IDs
//! ([`AstId`]) to positional information ([`AstPtr`]).
//!
//! [`AstPtr`]: rowan::ast::AstPtr
//! [`AstId`]: crate::id::AstId

pub mod nominal;
pub mod positional;

use std::sync::Arc;

use files::FileId;
use rowan::ast::AstNode;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{names::ModuleName, SourceDatabase};

pub use nominal::NominalMap;
pub use positional::PositionalMap;

#[salsa::query_group(ResolverStorage)]
pub trait ResolverDatabase: SourceDatabase {
    fn module_map(&self) -> Arc<FxHashMap<ModuleName, FileId>>;

    #[salsa::invoke(NominalMap::nominal_map_query)]
    fn nominal_map(&self, file_id: FileId) -> Arc<NominalMap>;

    #[salsa::invoke(PositionalMap::positional_map_query)]
    fn positional_map(&self, file_id: FileId) -> Arc<PositionalMap>;
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
