//! Implements the subtyping and unification algorithms.

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::substitute::SubstituteName;
use crate::core::{RowTypeId, Type, TypeId, normalise};
use crate::error::ErrorKind;
use crate::state::CheckState;

/// Determines if constraints are elaborated during [`subtype`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ElaborationMode {
    Yes,
    No,
}

pub fn subtype<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    t1: TypeId,
    t2: TypeId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    subtype_with_mode(state, context, ElaborationMode::Yes, t1, t2)
}

pub fn subtype_with_mode<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mode: ElaborationMode,
    t1: TypeId,
    t2: TypeId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let t1 = normalise::normalise(state, context, t1)?;
    let t2 = normalise::normalise(state, context, t2)?;

    if t1 == t2 {
        return Ok(true);
    }

    let t1_core = context.queries.lookup_type(t1);
    let t2_core = context.queries.lookup_type(t2);

    match (t1_core, t2_core) {
        (Type::Function(t1_argument, t1_result), Type::Function(t2_argument, t2_result)) => {
            Ok(subtype_with_mode(state, context, ElaborationMode::No, t2_argument, t1_argument)?
                && subtype_with_mode(state, context, ElaborationMode::No, t1_result, t2_result)?)
        }

        (Type::Application(_, _), Type::Function(t2_argument, t2_result)) => {
            let t2 = context.intern_function_application(t2_argument, t2_result);
            subtype_with_mode(state, context, mode, t1, t2)
        }

        (Type::Function(t1_argument, t1_result), Type::Application(_, _)) => {
            let t1 = context.intern_function_application(t1_argument, t1_result);
            subtype_with_mode(state, context, mode, t1, t2)
        }

        (_, Type::Forall(binder_id, inner)) => {
            let binder = context.lookup_forall_binder(binder_id);
            let skolem = state.fresh_rigid(context.queries, binder.kind);

            let inner = SubstituteName::one(state, context, binder.name, skolem, inner)?;
            state.with_depth(|state| subtype_with_mode(state, context, mode, t1, inner))
        }

        (Type::Forall(binder_id, inner), _) => {
            let binder = context.lookup_forall_binder(binder_id);
            let unification = state.fresh_unification(context.queries, binder.kind);

            let inner = SubstituteName::one(state, context, binder.name, unification, inner)?;
            subtype_with_mode(state, context, mode, inner, t2)
        }

        (Type::Constrained(constraint, inner), _) if mode == ElaborationMode::Yes => {
            // TODO: implication constraints
            //
            // state.push_wanted(constraint);
            // subtype_with_mode(state, context, inner, t2, mode)
            let _ = (constraint, inner);
            Ok(false)
        }

        (
            Type::Application(t1_function, t1_argument),
            Type::Application(t2_function, t2_argument),
        ) if t1_function == context.prim.record && t2_function == context.prim.record => {
            let t1_argument = normalise::normalise(state, context, t1_argument)?;
            let t2_argument = normalise::normalise(state, context, t2_argument)?;

            let t1_argument_core = context.queries.lookup_type(t1_argument);
            let t2_argument_core = context.queries.lookup_type(t2_argument);

            if let (Type::Row(t1_row_id), Type::Row(t2_row_id)) =
                (t1_argument_core, t2_argument_core)
            {
                subtype_rows(state, context, mode, t1_row_id, t2_row_id)
            } else {
                unify(state, context, t1, t2)
            }
        }

        (_, _) => unify(state, context, t1, t2),
    }
}

pub fn unify<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    t1: TypeId,
    t2: TypeId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let t1 = normalise::normalise(state, context, t1)?;
    let t2 = normalise::normalise(state, context, t2)?;

    if t1 == t2 {
        return Ok(true);
    }

    let t1_core = context.queries.lookup_type(t1);
    let t2_core = context.queries.lookup_type(t2);

    let unifies = match (t1_core, t2_core) {
        // TODO: document impredicativity
        (Type::Unification(id), _) => return solve(state, context, id, t2),
        (_, Type::Unification(id)) => return solve(state, context, id, t1),

        (
            Type::Application(t1_function, t1_argument),
            Type::Application(t2_function, t2_argument),
        ) => {
            unify(state, context, t1_function, t2_function)?
                && unify(state, context, t1_argument, t2_argument)?
        }

        (
            Type::KindApplication(t1_function, t1_argument),
            Type::KindApplication(t2_function, t2_argument),
        ) => {
            unify(state, context, t1_function, t2_function)?
                && unify(state, context, t1_argument, t2_argument)?
        }

        (
            Type::OperatorApplication(t1_file, t1_type, t1_left, t1_right),
            Type::OperatorApplication(t2_file, t2_type, t2_left, t2_right),
        ) if t1_file == t2_file && t1_type == t2_type => {
            unify(state, context, t1_left, t2_left)? && unify(state, context, t1_right, t2_right)?
        }

        (Type::Forall(t1_binder_id, t1_inner), Type::Forall(t2_binder_id, t2_inner)) => {
            let t1_binder = context.lookup_forall_binder(t1_binder_id);
            let t2_binder = context.lookup_forall_binder(t2_binder_id);

            unify(state, context, t1_binder.kind, t2_binder.kind)?;

            let skolem = state.fresh_rigid(context.queries, t1_binder.kind);

            let t1_inner = SubstituteName::one(state, context, t1_binder.name, skolem, t1_inner)?;
            let t2_inner = SubstituteName::one(state, context, t2_binder.name, skolem, t2_inner)?;

            state.with_depth(|state| unify(state, context, t1_inner, t2_inner))?
        }

        (Type::Forall(binder_id, inner), _) => {
            let binder = context.lookup_forall_binder(binder_id);
            let skolem = state.fresh_rigid(context.queries, binder.kind);
            let inner = SubstituteName::one(state, context, binder.name, skolem, inner)?;
            state.with_depth(|state| unify(state, context, inner, t2))?
        }

        (_, Type::Forall(binder_id, inner)) => {
            let binder = context.lookup_forall_binder(binder_id);
            let skolem = state.fresh_rigid(context.queries, binder.kind);
            let inner = SubstituteName::one(state, context, binder.name, skolem, inner)?;
            state.with_depth(|state| unify(state, context, t1, inner))?
        }

        (
            Type::Constrained(t1_constraint, t1_inner),
            Type::Constrained(t2_constraint, t2_inner),
        ) => {
            unify(state, context, t1_constraint, t2_constraint)?
                && unify(state, context, t1_inner, t2_inner)?
        }

        (Type::Function(t1_argument, t1_result), Type::Function(t2_argument, t2_result)) => {
            unify(state, context, t1_argument, t2_argument)?
                && unify(state, context, t1_result, t2_result)?
        }

        (Type::Kinded(t1_inner, t1_kind), Type::Kinded(t2_inner, t2_kind)) => {
            unify(state, context, t1_inner, t2_inner)? && unify(state, context, t1_kind, t2_kind)?
        }

        (Type::Constructor(t1_file, t1_item), Type::Constructor(t2_file, t2_item))
            if t1_file == t2_file && t1_item == t2_item =>
        {
            true
        }

        (
            Type::OperatorConstructor(t1_file, t1_item),
            Type::OperatorConstructor(t2_file, t2_item),
        ) if t1_file == t2_file && t1_item == t2_item => true,

        (Type::Integer(t1_value), Type::Integer(t2_value)) if t1_value == t2_value => true,

        (Type::String(t1_kind, t1_value), Type::String(t2_kind, t2_value))
            if t1_kind == t2_kind && t1_value == t2_value =>
        {
            true
        }

        (Type::Row(t1_row_id), Type::Row(t2_row_id)) => {
            unify_rows(state, context, t1_row_id, t2_row_id)?
        }

        (Type::Rigid(t1_name, _, t1_kind), Type::Rigid(t2_name, _, t2_kind))
            if t1_name == t2_name =>
        {
            unify(state, context, t1_kind, t2_kind)?
        }

        _ => false,
    };

    if !unifies {
        // TODO: pretty-print types for error messages
        let t1 = context.queries.intern_smol_str("?".into());
        let t2 = context.queries.intern_smol_str("?".into());
        state.insert_error(ErrorKind::CannotUnify { t1, t2 });
    }

    Ok(unifies)
}

/// Solves a unification variable to a given type.
fn solve<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    unification_id: u32,
    solution: TypeId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let _ = (state, context, unification_id, solution);
    Ok(false)
}

fn unify_rows<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    t1: RowTypeId,
    t2: RowTypeId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    // TODO: implement row unification
    let _ = (state, context, t1, t2);
    Ok(false)
}

fn subtype_rows<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mode: ElaborationMode,
    t1: RowTypeId,
    t2: RowTypeId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    // TODO: implement row subtyping
    let _ = (state, context, mode, t1, t2);
    Ok(false)
}
