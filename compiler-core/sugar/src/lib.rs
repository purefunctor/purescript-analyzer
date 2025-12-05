pub mod bracketing;
pub use bracketing::{Bracketed, OperatorTree, bracketed};

use std::sync::Arc;

use building_types::QueryProxy;
use indexing::IndexedModule;
use lowering::LoweredModule;
use resolving::ResolvedModule;

pub trait ExternalQueries:
    QueryProxy<
        Indexed = Arc<IndexedModule>,
        Resolved = Arc<ResolvedModule>,
        Lowered = Arc<LoweredModule>,
    >
{
}
