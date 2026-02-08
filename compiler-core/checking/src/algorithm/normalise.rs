use building_types::QueryResult;

use crate::ExternalQueries;
use crate::algorithm::kind::{operator, synonym};
use crate::algorithm::safety::safe_loop;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::core::TypeId;

pub fn normalise_expand_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut type_id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    safe_loop! {
        let expanded_id = state.normalize_type(type_id);
        let expanded_id = operator::expand_type_operator(state, context, expanded_id)?;
        let expanded_id = synonym::expand_type_synonym(state, context, expanded_id)?;

        if expanded_id == type_id {
            return Ok(type_id);
        }

        type_id = expanded_id;
    }
}
