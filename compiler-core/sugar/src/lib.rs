pub mod bracketing;
pub mod sections;
pub use bracketing::{Bracketed, OperatorTree, bracketed};
pub use sections::{SectionResult, Sectioned, sectioned};

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
