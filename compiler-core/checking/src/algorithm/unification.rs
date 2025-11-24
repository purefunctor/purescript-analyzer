pub mod context;
pub mod level;

pub use context::*;

use crate::ExternalQueries;
use crate::algorithm::{CheckContext, CheckState, kind, substitute};
use crate::core::{Type, TypeId, Variable, debruijn};

pub fn subsumes<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    t1: TypeId,
    t2: TypeId,
) -> bool
where
    Q: ExternalQueries,
{
    let t1 = state.normalize_type(t1);
    let t2 = state.normalize_type(t2);

    let t1_core = state.storage[t1].clone();
    let t2_core = state.storage[t2].clone();

    match (t1_core, t2_core) {
        (Type::Function(t1_argument, t1_result), Type::Function(t2_argument, t2_result)) => {
            subsumes(state, context, t2_argument, t1_argument)
                && subsumes(state, context, t1_result, t2_result)
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

        _ => unify(state, context, t1, t2),
    }
}

pub fn unify<Q>(state: &mut CheckState, context: &CheckContext<Q>, t1: TypeId, t2: TypeId) -> bool
where
    Q: ExternalQueries,
{
    let t1 = state.normalize_type(t1);
    let t2 = state.normalize_type(t2);
    match (&state.storage[t1], &state.storage[t2]) {
        (
            &Type::Application(t1_function, t1_argument),
            &Type::Application(t2_function, t2_argument),
        ) => {
            unify(state, context, t1_function, t2_function)
                && unify(state, context, t1_argument, t2_argument)
        }

        (Type::Constructor(t1_file_id, t1_item_id), Type::Constructor(t2_file_id, t2_item_id)) => {
            (t1_file_id, t1_item_id) == (t2_file_id, t2_item_id)
        }

        (&Type::Function(t1_argument, t1_result), &Type::Function(t2_argument, t2_result)) => {
            unify(state, context, t1_argument, t2_argument)
                && unify(state, context, t1_result, t2_result)
        }

        (&Type::Unification(unification_id), _) => {
            solve(state, context, unification_id, t2).is_some()
        }

        (_, &Type::Unification(unification_id)) => {
            solve(state, context, unification_id, t1).is_some()
        }

        (
            &Type::Variable(Variable::Bound(t1_index)),
            &Type::Variable(Variable::Bound(t2_index)),
        ) => t1_index == t2_index,

        (
            &Type::Variable(Variable::Skolem(t1_level, _)),
            &Type::Variable(Variable::Skolem(t2_level, _)),
        ) => t1_level == t2_level,

        _ => false,
    }
}

pub fn solve<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    unification_id: u32,
    solution: TypeId,
) -> Option<u32>
where
    Q: ExternalQueries,
{
    let codomain = state.bound.size();
    let occurs = Some(unification_id);

    if !promote_type(state, occurs, codomain, unification_id, solution) {
        return None;
    }

    let unification_kind = state.unification.get(unification_id).kind;
    let solution_kind = kind::elaborate_kind(state, context, solution);
    unify(state, context, unification_kind, solution_kind);

    state.unification.solve(unification_id, solution);

    Some(unification_id)
}

pub fn promote_type(
    state: &mut CheckState,
    occurs: Option<u32>,
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

        Type::KindApplication(function, argument) => {
            promote_type(state, occurs, codomain, unification_id, function)
                && promote_type(state, occurs, codomain, unification_id, argument)
        }

        Type::Unification(solution_id) => {
            let unification = state.unification.get(unification_id);
            let solution = state.unification.get(solution_id);

            if occurs == Some(solution_id) {
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
