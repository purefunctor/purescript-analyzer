//! Implements syntax-driven checking rules for types.

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::TypeId;
use crate::state::CheckState;

/// Checks the kind of a syntax type against a core type.
///
/// This function returns the core type and kind.
pub fn check_kind<Q>(
    _state: &mut CheckState,
    _context: &CheckContext<Q>,
    _source_type: lowering::TypeId,
    _expected_kind: TypeId,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    todo!()
}
