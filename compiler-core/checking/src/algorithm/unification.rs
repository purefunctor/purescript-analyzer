//! Implements subsumption and unification.

use std::sync::Arc;

use building_types::QueryResult;
use itertools::{EitherOrBoth, Itertools};

use crate::ExternalQueries;
use crate::algorithm::kind::synonym;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{kind, substitute, transfer};
use crate::core::{RowField, RowType, Type, TypeId, Variable, debruijn};
use crate::error::ErrorKind;

pub fn subsumes<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    t1: TypeId,
    t2: TypeId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let t1 = synonym::normalize_expand_type(state, context, t1)?;
    let t2 = synonym::normalize_expand_type(state, context, t2)?;

    if t1 == t2 {
        return Ok(true);
    }

    let t1_core = state.storage[t1].clone();
    let t2_core = state.storage[t2].clone();

    match (t1_core, t2_core) {
        (Type::Function(t1_argument, t1_result), Type::Function(t2_argument, t2_result)) => {
            Ok(subsumes(state, context, t2_argument, t1_argument)?
                && subsumes(state, context, t1_result, t2_result)?)
        }

        (_, Type::Forall(ref binder, inner)) => {
            let v = Variable::Skolem(binder.level, binder.kind);
            let t = state.storage.intern(Type::Variable(v));

            let inner = substitute::substitute_bound(state, t, inner);
            subsumes(state, context, t1, inner)
        }

        (Type::Forall(ref binder, inner), _) => {
            let k = state.normalize_type(binder.kind);
            let t = state.fresh_unification_kinded(k);

            let inner = substitute::substitute_bound(state, t, inner);
            subsumes(state, context, inner, t2)
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
    let t1 = synonym::normalize_expand_type(state, context, t1)?;
    let t2 = synonym::normalize_expand_type(state, context, t2)?;

    if t1 == t2 {
        return Ok(true);
    }

    let t1_core = state.storage[t1].clone();
    let t2_core = state.storage[t2].clone();

    let unifies = match (t1_core, t2_core) {
        (
            Type::Application(t1_function, t1_argument),
            Type::Application(t2_function, t2_argument),
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

        (Type::Function(t1_argument, t1_result), Type::Function(t2_argument, t2_result)) => {
            unify(state, context, t1_argument, t2_argument)?
                && unify(state, context, t1_result, t2_result)?
        }

        (Type::Row(t1_row), Type::Row(t2_row)) => unify_rows(state, context, t1_row, t2_row)?,

        (Type::Unification(unification_id), _) => {
            solve(state, context, unification_id, t2)?.is_some()
        }

        (_, Type::Unification(unification_id)) => {
            solve(state, context, unification_id, t1)?.is_some()
        }

        _ => false,
    };

    if !unifies {
        // at this point, it should be impossible to have
        // unsolved unification variables within t1 and t2
        let t1 = transfer::globalize(state, context, t1);
        let t2 = transfer::globalize(state, context, t2);
        state.insert_error(ErrorKind::CannotUnify { t1, t2 });
    }

    Ok(unifies)
}

pub fn solve<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    unification_id: u32,
    solution: TypeId,
) -> QueryResult<Option<u32>>
where
    Q: ExternalQueries,
{
    let codomain = state.bound.size();
    let occurs = unification_id;

    if !promote_type(state, occurs, codomain, unification_id, solution) {
        return Ok(None);
    }

    let unification_kind = state.unification.get(unification_id).kind;
    let solution_kind = kind::elaborate_kind(state, context, solution)?;
    unify(state, context, unification_kind, solution_kind)?;

    state.unification.solve(unification_id, solution);

    Ok(Some(unification_id))
}

pub fn promote_type(
    state: &mut CheckState,
    occurs: u32,
    codomain: debruijn::Size,
    unification_id: u32,
    solution: TypeId,
) -> bool {
    let solution = state.normalize_type(solution);
    match state.storage[solution] {
        Type::Application(function, argument) => {
            promote_type(state, occurs, codomain, unification_id, function)
                && promote_type(state, occurs, codomain, unification_id, argument)
        }

        Type::Constrained(constraint, inner) => {
            promote_type(state, occurs, codomain, unification_id, constraint)
                && promote_type(state, occurs, codomain, unification_id, inner)
        }

        Type::Constructor(_, _) => true,

        Type::Forall(ref binder, inner) => {
            let inner_codomain = codomain.increment();
            promote_type(state, occurs, codomain, unification_id, binder.kind)
                && promote_type(state, occurs, inner_codomain, unification_id, inner)
        }

        Type::Function(argument, result) => {
            promote_type(state, occurs, codomain, unification_id, argument)
                && promote_type(state, occurs, codomain, unification_id, result)
        }

        Type::Integer(_) => true,

        Type::KindApplication(function, argument) => {
            promote_type(state, occurs, codomain, unification_id, function)
                && promote_type(state, occurs, codomain, unification_id, argument)
        }

        Type::Kinded(inner, kind) => {
            promote_type(state, occurs, codomain, unification_id, inner)
                && promote_type(state, occurs, codomain, unification_id, kind)
        }

        Type::Operator(_, _) => true,

        Type::OperatorApplication(_, _, left, right) => {
            promote_type(state, occurs, codomain, unification_id, left)
                && promote_type(state, occurs, codomain, unification_id, right)
        }

        Type::Row(RowType { ref fields, tail }) => {
            let fields = Arc::clone(fields);

            for field in fields.iter() {
                if !promote_type(state, occurs, codomain, unification_id, field.id) {
                    return false;
                }
            }

            if let Some(tail) = tail
                && !promote_type(state, occurs, codomain, unification_id, tail)
            {
                return false;
            }

            true
        }

        Type::String(_, _) => true,

        Type::SynonymApplication(_, _, _, ref arguments) => {
            let arguments = Arc::clone(arguments);
            for argument in arguments.iter() {
                if !promote_type(state, occurs, codomain, unification_id, *argument) {
                    return false;
                }
            }
            true
        }

        Type::Unification(solution_id) => {
            let unification = state.unification.get(unification_id);
            let solution = state.unification.get(solution_id);

            if occurs == solution_id {
                return false;
            }

            if unification.domain < solution.domain {
                let promoted_ty =
                    state.fresh_unification_kinded_at(unification.domain, unification.kind);

                // promoted_ty is simple enough to not warrant `solve` recursion
                state.unification.solve(solution_id, promoted_ty);
            }

            true
        }

        Type::Variable(ref variable) => {
            let unification = state.unification.get(unification_id);
            if let Variable::Bound(index) = variable
                && !index.in_scope(unification.domain)
            {
                return false;
            }

            true
        }

        Type::Unknown => true,
    }
}

fn unify_rows<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    t1_row: RowType,
    t2_row: RowType,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let (extras_left, extras_right, ok) = partition_row_fields(state, context, &t1_row, &t2_row)?;

    if !ok {
        return Ok(false);
    }

    match (t1_row.tail, t2_row.tail) {
        (None, None) => Ok(extras_left.is_empty() && extras_right.is_empty()),

        (Some(t1_tail), None) => {
            if !extras_left.is_empty() {
                return Ok(false);
            }
            let row = Type::Row(RowType { fields: Arc::from(extras_right), tail: None });
            let row_id = state.storage.intern(row);
            unify(state, context, t1_tail, row_id)
        }

        (None, Some(t2_tail)) => {
            if !extras_right.is_empty() {
                return Ok(false);
            }
            let row = Type::Row(RowType { fields: Arc::from(extras_left), tail: None });
            let row_id = state.storage.intern(row);
            unify(state, context, t2_tail, row_id)
        }

        (Some(t1_tail), Some(t2_tail)) => {
            if extras_left.is_empty() && extras_right.is_empty() {
                return unify(state, context, t1_tail, t2_tail);
            }

            let unification = state.fresh_unification_type(context);

            let left_tail_row =
                Type::Row(RowType { fields: Arc::from(extras_right), tail: Some(unification) });
            let left_tail_row_id = state.storage.intern(left_tail_row);

            let right_tail_row =
                Type::Row(RowType { fields: Arc::from(extras_left), tail: Some(unification) });
            let right_tail_row_id = state.storage.intern(right_tail_row);

            Ok(unify(state, context, t1_tail, left_tail_row_id)?
                && unify(state, context, t2_tail, right_tail_row_id)?)
        }
    }
}

pub fn partition_row_fields<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    t1_row: &RowType,
    t2_row: &RowType,
) -> QueryResult<(Vec<RowField>, Vec<RowField>, bool)>
where
    Q: ExternalQueries,
{
    let mut extras_left = vec![];
    let mut extras_right = vec![];
    let mut ok = true;

    let t1_fields = t1_row.fields.iter();
    let t2_fields = t2_row.fields.iter();

    for field in t1_fields.merge_join_by(t2_fields, |left, right| left.label.cmp(&right.label)) {
        match field {
            EitherOrBoth::Both(left, right) => {
                if !unify(state, context, left.id, right.id)? {
                    ok = false;
                }
            }
            EitherOrBoth::Left(left) => {
                let left = left.clone();
                extras_left.push(left)
            }
            EitherOrBoth::Right(right) => {
                let right = right.clone();
                extras_right.push(right)
            }
        }
    }

    Ok((extras_left, extras_right, ok))
}
