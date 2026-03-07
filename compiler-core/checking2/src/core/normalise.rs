//! Implements normalisation algorithms for the core representation.

use std::iter;

use building_types::QueryResult;
use itertools::Itertools;

use crate::context::CheckContext;
use crate::core::substitute::{NameToType, SubstituteName};
use crate::core::{KindOrType, RowType, Saturation, Type, TypeId, toolkit};
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

pub fn expand<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    safe_loop! {
        let expanded = expand_synonym(state, context, id)?;
        let normalised = normalise(state, context, expanded)?;
        if normalised == id {
            return Ok(id);
        }
        id = normalised;
    }
}

/// Expands [`Type::SynonymApplication`] with respect to oversaturation.
///
/// In certain cases, type synonyms can be oversaturated or applied with more
/// arguments than they're declared to accept. In the following example:
///
/// ```purescript
/// type Identity :: forall k. k -> k
/// type Identity a = a
///
/// data Tuple a b = Tuple a b
///
/// test1 :: Identity Array Int
/// test1 = [42]
///
/// test2 :: Identity Tuple Int String
/// test2 = Tuple 42 "hello"
///
/// forceSolve = { test1, test2 }
/// ```
///
/// The `Identity Array` and `Identity Tuple` will be expanded to reveal
/// `Array` and `Tuple` which are applied to their respective arguments.
fn expand_synonym<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let mut current = id;
    let mut applied_arguments = vec![];

    // Collect oversaturated arguments, in our example above, this
    // would collect `[Int]` and `[Int, String]` into `arguments`.
    safe_loop! {
        current = normalise(state, context, current)?;
        match context.lookup_type(current) {
            Type::Application(function, argument) => {
                applied_arguments.push(KindOrType::Type(argument));
                current = function;
            }
            Type::KindApplication(function, argument) => {
                applied_arguments.push(KindOrType::Kind(argument));
                current = function;
            }
            _ => break,
        }
    }

    current = normalise(state, context, current)?;
    let Type::SynonymApplication(synonym_id) = context.lookup_type(current) else {
        return Ok(id);
    };

    let synonym_application = context.lookup_synonym(synonym_id);
    if synonym_application.saturation != Saturation::Full {
        return Ok(id);
    }

    let (file_id, type_id) = synonym_application.reference;
    let checked_synonym = toolkit::lookup_file_synonym(state, context, file_id, type_id)?;
    let Some(checked_synonym) = checked_synonym else {
        return Ok(id);
    };

    let mut bindings = NameToType::default();
    let mut kind = checked_synonym.kind;
    let mut arguments = {
        let synonym_arguments = synonym_application.arguments.iter().copied();
        let applied_arguments = applied_arguments.iter().copied().rev();
        iter::chain(synonym_arguments, applied_arguments)
    };

    // Create substitutions for kind arguments. For example,
    //
    //   type T :: forall k. k -> Type
    //   type T (a :: k) = Proxy (a :: k)
    //
    // given an application such as,
    //
    //   T @Type Int
    //
    // this loop produces the replacement,
    //
    //   k := Type
    //
    // which is later substituted into the synonym body. Without this step,
    // expansion would leave `k` rigid inside the synonym body causing
    // unification errors downstream.
    safe_loop! {
        kind = normalise(state, context, kind)?;

        let Type::Forall(binder_id, inner) = context.lookup_type(kind) else {
            break;
        };

        let Some(KindOrType::Kind(argument)) = arguments.next() else {
            return Ok(id);
        };

        let binder = context.lookup_forall_binder(binder_id);
        bindings.insert(binder.name, argument);

        kind = inner;
    }

    // Create substitutions for type arguments.
    for parameter in &checked_synonym.parameters {
        let Some(KindOrType::Type(argument)) = arguments.next() else {
            return Ok(id);
        };
        bindings.insert(parameter.name, argument);
    }

    // Apply the substitutions if there are any.
    let mut substituted = if bindings.is_empty() {
        checked_synonym.synonym
    } else {
        SubstituteName::many(state, context, &bindings, checked_synonym.synonym)?
    };

    // Reconstruct applications from remaining oversaturated arguments.
    for argument in arguments {
        substituted = match argument {
            KindOrType::Type(argument) => context.intern_application(substituted, argument),
            KindOrType::Kind(argument) => context.intern_kind_application(substituted, argument),
        };
    }

    Ok(substituted)
}
