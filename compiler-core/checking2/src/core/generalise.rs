//! Implements generalisation algorithms for the core representation.
//!
//! Simply put, generalisation is an operation that takes some inferred
//! type full of unsolved [unification variables] and replaces them with
//! [universally quantified] [rigid type variables]. For example:
//!
//! ```purescript
//! id :: ?0 -> ?0
//! ```
//!
//! this will generalise into the following:
//!
//! ```purescript
//! id :: forall (t0 :: Type). t0 -> t0
//! ```
//!
//! [unification variables]: crate::core::Type::Unification
//! [universally quantified]: crate::core::Type::Forall
//! [rigid type variables]: crate::core::Type::Rigid

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::TypeId;
use crate::state::CheckState;

/// Generalises a given type. See also module-level documentation.
pub fn generalise<Q>(
    _state: &mut CheckState,
    _context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    Ok(id)
}
