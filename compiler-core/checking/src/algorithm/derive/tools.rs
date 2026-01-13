//! Shared utilities for derive instance checking.

use building_types::QueryResult;
use files::FileId;
use indexing::{DeriveId, TermItemId, TypeItemId};
use itertools::Itertools;

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{quantify, transfer};
use crate::core::{Instance, InstanceKind, Type, TypeId, debruijn};
use crate::error::ErrorKind;

/// Elaborated derive instance after kind inference.
pub struct ElaboratedDerive {
    pub derive_id: DeriveId,
    pub constraints: Vec<(TypeId, TypeId)>,
    pub arguments: Vec<(TypeId, TypeId)>,
    pub class_file: FileId,
    pub class_id: TypeItemId,
}

/// Emits a type class constraint `Class type_id`.
pub fn emit_constraint(
    state: &mut CheckState,
    (class_file, class_id): (FileId, TypeItemId),
    type_id: TypeId,
) {
    let class_type = state.storage.intern(Type::Constructor(class_file, class_id));
    let constraint = state.storage.intern(Type::Application(class_type, type_id));
    state.constraints.push_wanted(constraint);
}

/// Pushes given constraints from the instance head onto the constraint stack.
pub fn push_given_constraints(state: &mut CheckState, constraints: &[(TypeId, TypeId)]) {
    for (constraint_type, _) in constraints {
        state.constraints.push_given(*constraint_type);
    }
}

/// Solves constraints and reports any unsatisfied constraints as errors.
pub fn solve_and_report_constraints<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let residual = state.solve_constraints(context)?;
    for constraint in residual {
        let constraint = transfer::globalize(state, context, constraint);
        state.insert_error(ErrorKind::NoInstanceFound { constraint });
    }
    Ok(())
}

/// Registers a derived instance in the checked state.
///
/// This performs generalisation and globalization of the instance
/// types, then stores the result in [`CheckState::checked`].
pub fn register_derived_instance<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: ElaboratedDerive,
) where
    Q: ExternalQueries,
{
    let ElaboratedDerive { derive_id, constraints, arguments, class_file, class_id } = input;

    let mut instance = Instance {
        arguments,
        constraints,
        resolution: (class_file, class_id),
        kind: InstanceKind::Derive,
        kind_variables: debruijn::Size(0),
    };

    quantify::quantify_instance(state, &mut instance);

    for (t, k) in instance.arguments.iter_mut() {
        *t = transfer::globalize(state, context, *t);
        *k = transfer::globalize(state, context, *k);
    }

    for (t, k) in instance.constraints.iter_mut() {
        *t = transfer::globalize(state, context, *t);
        *k = transfer::globalize(state, context, *k);
    }

    state.checked.derived.insert(derive_id, instance);
}

/// Looks up data constructors for a type, handling cross-file lookups.
pub fn lookup_data_constructors<Q>(
    context: &CheckContext<Q>,
    data_file: FileId,
    data_id: TypeItemId,
) -> QueryResult<Vec<TermItemId>>
where
    Q: ExternalQueries,
{
    let constructors = if data_file == context.id {
        context.indexed.pairs.data_constructors(data_id).collect_vec()
    } else {
        let indexed = context.queries.indexed(data_file)?;
        indexed.pairs.data_constructors(data_id).collect_vec()
    };
    Ok(constructors)
}
