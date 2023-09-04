//! Database for resolving information.
//!
//! In particular, the [`ResolverDatabase`] indexes information about source
//! files to facilitate processes such as name resolution, as in finding the
//! corresponding [`FileId`] for a [`ModuleName`], or assigning stable IDs
//! ([`AstId`]) to positional information ([`AstPtr`]).
//!
//! [`AstPtr`]: rowan::ast::AstPtr
//! [`AstId`]: crate::id::AstId

pub mod import;
pub mod module;
pub mod nominal;
pub mod positional;

use std::sync::Arc;

use files::FileId;

use crate::SourceDatabase;

pub use import::QualifiedImports;
pub use module::ModuleMap;
pub use nominal::NominalMap;
pub use positional::PositionalMap;

#[salsa::query_group(ResolverStorage)]
pub trait ResolverDatabase: SourceDatabase {
    #[salsa::invoke(ModuleMap::module_map_query)]
    fn module_map(&self) -> Arc<ModuleMap>;

    #[salsa::invoke(NominalMap::nominal_map_query)]
    fn nominal_map(&self, file_id: FileId) -> Arc<NominalMap>;

    #[salsa::invoke(PositionalMap::positional_map_query)]
    fn positional_map(&self, file_id: FileId) -> Arc<PositionalMap>;

    #[salsa::invoke(QualifiedImports::qualified_imports_query)]
    fn qualified_imports(&self, file_id: FileId) -> Arc<QualifiedImports>;
}
