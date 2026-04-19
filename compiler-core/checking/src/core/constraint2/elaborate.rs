//! Implements elabortion for given constraints.

use std::collections::VecDeque;

use building_types::QueryResult;
use itertools::Itertools;
use rustc_hash::FxHashSet;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::constraint2::canonical::CanonicalConstraint;
use crate::core::constraint2::{CanonicalConstraintId, canonical};
use crate::core::substitute::{NameToType, SubstituteName};
use crate::core::{CheckedClass, KindOrType, toolkit};
use crate::state::CheckState;

/// Entrypoint for elaborating given [`CanonicalConstraint`].
pub fn elaborate_given<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    given: &[CanonicalConstraintId],
) -> QueryResult<Vec<CanonicalConstraintId>>
where
    Q: ExternalQueries,
{
    let given = elaborate_superclasses(state, context, given)?;
    let given = elaborate_coercible(state, context, given);
    Ok(given)
}

/// Elaborates superclasses from a given [`CanonicalConstraint`].
pub fn elaborate_superclasses<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    given: &[CanonicalConstraintId],
) -> QueryResult<Vec<CanonicalConstraintId>>
where
    Q: ExternalQueries,
{
    let mut elaborated = Vec::with_capacity(given.len());
    let mut pending = VecDeque::with_capacity(given.len());
    let mut seen = FxHashSet::default();

    for &given in given {
        if seen.insert(given) {
            elaborated.push(given);
            pending.push_back(given);
        }
    }

    while let Some(constraint) = pending.pop_front() {
        elaborate_via_superclass(
            state,
            context,
            constraint,
            &mut elaborated,
            &mut pending,
            &mut seen,
        )?;
    }

    Ok(elaborated)
}

fn elaborate_via_superclass<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    constraint: CanonicalConstraintId,
    constraints: &mut Vec<CanonicalConstraintId>,
    pending: &mut VecDeque<CanonicalConstraintId>,
    seen: &mut FxHashSet<CanonicalConstraintId>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let CanonicalConstraint { file_id, type_id, .. } = state.canonicals[constraint];
    let Some(class) = toolkit::lookup_file_class(state, context, file_id, type_id)? else {
        return Ok(());
    };

    if class.superclasses.is_empty() {
        return Ok(());
    }

    let CanonicalConstraint { arguments, .. } = &state.canonicals[constraint];
    let Some(substitutions) = superclass_substitutions(context, &class, arguments)? else {
        return Ok(());
    };

    for superclass in class.superclasses {
        let superclass = SubstituteName::many(state, context, &substitutions, superclass)?;
        if let Some(superclass) = canonical::canonicalise(state, context, superclass)? {
            if seen.insert(superclass) {
                constraints.push(superclass);
                pending.push_back(superclass);
            }
        }
    }

    Ok(())
}

fn superclass_substitutions<Q>(
    context: &CheckContext<Q>,
    class: &CheckedClass,
    arguments: &[KindOrType],
) -> QueryResult<Option<NameToType>>
where
    Q: ExternalQueries,
{
    let mut bindings = NameToType::default();
    let mut arguments = arguments.iter().copied();

    for &binder_id in &class.kind_binders {
        let Some(KindOrType::Kind(argument)) = arguments.next() else {
            return Ok(None);
        };
        let binder = context.lookup_forall_binder(binder_id);
        bindings.insert(binder.name, argument);
    }

    for &binder_id in &class.type_parameters {
        let Some(KindOrType::Type(argument)) = arguments.next() else {
            return Ok(None);
        };
        let binder = context.lookup_forall_binder(binder_id);
        bindings.insert(binder.name, argument);
    }

    if arguments.next().is_some() {
        return Ok(None);
    }

    Ok(Some(bindings))
}

/// Elaborates useful [`CanonicalConstraint`] for `Coercible`.
pub fn elaborate_coercible<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut given: Vec<CanonicalConstraintId>,
) -> Vec<CanonicalConstraintId>
where
    Q: ExternalQueries,
{
    let symmetric = given.iter().filter_map(|given| {
        let CanonicalConstraint { file_id, type_id, ref arguments } = state.canonicals[*given];

        if (file_id, type_id) != (context.prim_coerce.file_id, context.prim_coerce.coercible) {
            return None;
        }

        let (kind @ KindOrType::Kind(_), left @ KindOrType::Type(_), right @ KindOrType::Type(_)) =
            arguments.iter().copied().collect_tuple()?
        else {
            return None;
        };

        let arguments = [kind, right, left].into();
        Some(state.canonicals.intern(CanonicalConstraint { file_id, type_id, arguments }))
    });

    let symmetric = symmetric.collect_vec();
    given.extend(symmetric);

    given
}
