//! Shared utilities for derive instance checking.

use building_types::QueryResult;
use files::FileId;
use indexing::{DeriveId, TermItemId, TypeItemId};
use itertools::Itertools;

use rustc_hash::FxHashMap;

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{constraint, quantify, transfer};
use crate::core::{Instance, InstanceKind, Type, TypeId, debruijn};
use crate::error::ErrorKind;

mod substitute {
    pub use crate::algorithm::substitute::SubstituteBindings;
}

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

/// Emits wanted constraints for the superclasses of the class being derived.
///
/// When deriving `Traversable (Compose f g)`, this emits:
/// - `Functor (Compose f g)`
/// - `Foldable (Compose f g)`
///
/// These constraints ensure that the derived instance has all required
/// instances available, which is a pre-requisite for code generation.
pub fn emit_superclass_constraints<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    class_file: FileId,
    class_id: TypeItemId,
    arguments: &[(TypeId, TypeId)],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(class_info) = constraint::lookup_file_class(state, context, class_file, class_id)?
    else {
        return Ok(());
    };

    if class_info.superclasses.is_empty() {
        return Ok(());
    }

    let initial_level = class_info.quantified_variables.0 + class_info.kind_variables.0;
    let mut bindings = FxHashMap::default();
    for (index, &(argument_type, _)) in arguments.iter().enumerate() {
        let level = debruijn::Level(initial_level + index as u32);
        bindings.insert(level, argument_type);
    }

    for &(superclass, _) in class_info.superclasses.iter() {
        let specialised = substitute::SubstituteBindings::on(state, &bindings, superclass);
        state.constraints.push_wanted(specialised);
    }

    Ok(())
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
        let constraint = state.render_local_type(context, constraint);
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
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let ElaboratedDerive { derive_id, constraints, arguments, class_file, class_id } = input;

    let mut instance = Instance {
        arguments,
        constraints,
        resolution: (class_file, class_id),
        kind: InstanceKind::Derive,
        kind_variables: vec![],
    };

    quantify::quantify_instance(state, &mut instance);

    constraint::validate_instance_rows(state, context, class_file, class_id, &instance.arguments)?;

    for (t, k) in instance.arguments.iter_mut() {
        *t = transfer::globalize(state, context, *t);
        *k = transfer::globalize(state, context, *k);
    }

    for (t, k) in instance.constraints.iter_mut() {
        *t = transfer::globalize(state, context, *t);
        *k = transfer::globalize(state, context, *k);
    }

    state.checked.derived.insert(derive_id, instance);

    Ok(())
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
