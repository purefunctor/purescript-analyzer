use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::fold::{FoldAction, TypeFold, fold_type};
use crate::core::{Type, TypeId};
use crate::state::CheckState;

pub struct Zonk;

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

impl Zonk {
    pub fn on<Q>(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: TypeId,
    ) -> QueryResult<TypeId>
    where
        Q: ExternalQueries,
    {
        fold_type(state, context, id, &mut Zonk)
    }
}
