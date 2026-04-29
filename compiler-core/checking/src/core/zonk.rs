use std::mem;

use building_types::QueryResult;

use crate::context::CheckContext;
use crate::core::fold::{FoldAction, TypeFold, fold_type};
use crate::core::{Type, TypeId};
use crate::state::CheckState;
use crate::{ExternalQueries, OperatorBranchTypes};

struct Zonk;

impl TypeFold for Zonk {
    fn transform<Q>(
        &mut self,
        _state: &mut CheckState,
        _context: &CheckContext<Q>,
        _id: TypeId,
        _t: &Type,
    ) -> QueryResult<FoldAction>
    where
        Q: ExternalQueries,
    {
        Ok(FoldAction::Continue)
    }
}

pub fn zonk<Q>(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    fold_type(state, context, id, &mut Zonk)
}

pub fn zonk_nodes<Q>(state: &mut CheckState, context: &CheckContext<Q>) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    macro_rules! zonk_type_map {
        ($field:ident) => {
            for (node_id, type_id) in mem::take(&mut state.checked.nodes.$field) {
                let type_id = zonk(state, context, type_id)?;
                state.checked.nodes.$field.insert(node_id, type_id);
            }
        };
    }

    macro_rules! zonk_operator_map {
        ($field:ident) => {
            for (node_id, branch_types) in mem::take(&mut state.checked.nodes.$field) {
                let branch_types = zonk_operator_branch(state, context, branch_types)?;
                state.checked.nodes.$field.insert(node_id, branch_types);
            }
        };
    }

    zonk_type_map!(types);
    zonk_type_map!(expressions);
    zonk_type_map!(binders);
    zonk_type_map!(lets);
    zonk_type_map!(puns);
    zonk_type_map!(sections);
    zonk_operator_map!(term_operator);
    zonk_operator_map!(type_operator);

    Ok(())
}

fn zonk_operator_branch<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    branch_types: OperatorBranchTypes,
) -> QueryResult<OperatorBranchTypes>
where
    Q: ExternalQueries,
{
    Ok(OperatorBranchTypes {
        left: zonk(state, context, branch_types.left)?,
        right: zonk(state, context, branch_types.right)?,
        result: zonk(state, context, branch_types.result)?,
    })
}
