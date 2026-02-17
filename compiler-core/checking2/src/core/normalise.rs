//! Implements normalisation algorithms for the core representation.

use building_types::QueryResult;
use itertools::Itertools;

use crate::context::CheckContext;
use crate::core::{RowType, Type, TypeId};
use crate::state::{CheckState, UnificationState};
use crate::{ExternalQueries, safe_loop};

struct ReductionContext<'a, 'q, Q>
where
    Q: ExternalQueries,
{
    state: &'a mut CheckState,
    context: &'a CheckContext<'q, Q>,
    compression: Vec<u32>,
}

impl<'a, 'q, Q> ReductionContext<'a, 'q, Q>
where
    Q: ExternalQueries,
{
    fn new(state: &'a mut CheckState, context: &'a CheckContext<'q, Q>) -> Self {
        ReductionContext { state, context, compression: vec![] }
    }

    fn reduce_once(&mut self, id: TypeId) -> Option<TypeId> {
        let t = self.context.queries.lookup_type(id);

        if let Some(next) = self.rule_prune_unifications(&t) {
            return Some(next);
        }
        if let Some(next) = self.rule_simplify_rows(&t) {
            return Some(next);
        }

        None
    }

    fn rule_prune_unifications(&mut self, t: &Type) -> Option<TypeId> {
        let Type::Unification(unification_id) = *t else {
            return None;
        };

        let UnificationState::Solved(solution_id) =
            self.state.unifications.get(unification_id).state
        else {
            return None;
        };

        self.compression.push(unification_id);
        Some(solution_id)
    }

    fn rule_simplify_rows(&self, t: &Type) -> Option<TypeId> {
        let Type::Row(row_id) = *t else {
            return None;
        };

        let row = self.context.queries.lookup_row_type(row_id);

        if row.fields.is_empty() {
            return row.tail;
        }

        let tail_id = row.tail?;
        let tail_t = self.context.queries.lookup_type(tail_id);

        let Type::Row(inner_row_id) = tail_t else {
            return None;
        };

        if inner_row_id == row_id {
            return None;
        }

        let inner = self.context.queries.lookup_row_type(inner_row_id);

        let merged_fields = {
            let left = row.fields.iter().cloned();
            let right = inner.fields.iter().cloned();
            left.merge_by(right, |left, right| left.label <= right.label)
        };

        let merged_row = RowType::new(merged_fields, inner.tail);
        let merged_row = self.context.queries.intern_row_type(merged_row);
        let merged_row = self.context.queries.intern_type(Type::Row(merged_row));

        Some(merged_row)
    }
}

pub fn normalise<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let mut reduction = ReductionContext::new(state, context);

    let id = safe_loop! {
        if let Some(reduced_id) = reduction.reduce_once(id) {
            id = reduced_id;
        } else {
            break id;
        }
    };

    for unification_id in reduction.compression {
        state.unifications.solve(unification_id, id);
    }

    Ok(id)
}
