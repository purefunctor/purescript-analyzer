use std::sync::Arc;

use building_types::QueryResult;
use itertools::{EitherOrBoth, Itertools};

use crate::ExternalQueries;
use crate::algorithm::kind::synonym;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{kind, substitute};
use crate::core::{RowField, RowType, Type, TypeId, Variable, debruijn};
use crate::error::ErrorKind;

/// Determines if constraints are elaborated during [`subtype`].
///
/// Elaboration means pushing constraints as "wanted" and inserting dictionary
/// placeholders. This is only valid in **covariant** positions where the type
/// checker controls what value is passed.
///
/// In **contravariant** positions (e.g., function arguments), the caller provides
/// values—we cannot insert dictionaries there. When both sides have matching
/// constraint structure, structural unification handles them correctly:
///
/// ```text
/// (IsSymbol ?sym => Proxy ?sym -> r) <= (IsSymbol ~sym => Proxy ~sym -> r)
///   IsSymbol ?sym ~ IsSymbol ~sym  → solves ?sym := ~sym
/// ```
///
/// [`Type::Function`] in the [`subtype`] rule disables this for the argument
/// and result positions. Syntax-driven rules like checking for binders and
/// expressions that appear in the argument position also disable elaboration.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ElaborationMode {
    Yes,
    No,
}

/// Check that `t1` is a subtype of `t2`
///
/// In the type system, we define that polymorphic types are subtypes of
/// monomorphic types because they can be instantiated by turning type
/// variables into unification variables, for example:
///
/// ```text
/// subtype (forall a. a -> a) (Int -> Int)
///   subtype (?a -> ?a) (Int -> Int)
///     subtype ?a Int
/// ```
///
/// Similarly, polymorphic types are subtypes of other polymorphic types
/// through skolemisation, for example:
///
/// ```text
/// subtype (forall a. a -> a) (forall b. b -> b)
///   subtype (?a -> ?a) (~b -> ~b)
///     subtype ?a ~b
/// ```
///
/// With this in mind, [`subtype`] can be used to check if an inferred
/// type properly fits a declared type, for example:
///
/// ```text
/// id :: forall a. a -> a
/// id = \a -> a
///
/// subtype (?a -> ?a) (forall a. a -> a)
///   subtype (?a -> ?a) (~a -> ~a)
///     subtype ?a ~a
/// ```
pub fn subtype<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    t1: TypeId,
    t2: TypeId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    subtype_with_mode(state, context, t1, t2, ElaborationMode::Yes)
}

#[tracing::instrument(skip_all, name = "subtype_with_mode")]
pub fn subtype_with_mode<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    t1: TypeId,
    t2: TypeId,
    mode: ElaborationMode,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let t1 = synonym::normalize_expand_type(state, context, t1)?;
    let t2 = synonym::normalize_expand_type(state, context, t2)?;

    crate::debug_fields!(state, context, { t1 = t1, t2 = t2, ?mode = mode });

    if t1 == t2 {
        crate::trace_fields!(state, context, { t1 = t1, t2 = t2 }, "identical");
        return Ok(true);
    }

    let t1_core = state.storage[t1].clone();
    let t2_core = state.storage[t2].clone();

    match (t1_core, t2_core) {
        (Type::Function(t1_argument, t1_result), Type::Function(t2_argument, t2_result)) => {
            Ok(subtype_with_mode(state, context, t2_argument, t1_argument, ElaborationMode::No)?
                && subtype_with_mode(state, context, t1_result, t2_result, ElaborationMode::No)?)
        }

        (Type::Application(t1_partial, t1_result), Type::Function(t2_argument, t2_result)) => {
            let t1_partial = state.normalize_type(t1_partial);
            if let Type::Application(t1_constructor, t1_argument) = state.storage[t1_partial] {
                Ok(unify(state, context, t1_constructor, context.prim.function)?
                    && subtype_with_mode(
                        state,
                        context,
                        t2_argument,
                        t1_argument,
                        ElaborationMode::No,
                    )?
                    && subtype_with_mode(
                        state,
                        context,
                        t1_result,
                        t2_result,
                        ElaborationMode::No,
                    )?)
            } else {
                unify(state, context, t1, t2)
            }
        }

        (Type::Function(t1_argument, t1_result), Type::Application(t2_partial, t2_result)) => {
            let t2_partial = state.normalize_type(t2_partial);
            if let Type::Application(t2_constructor, t2_argument) = state.storage[t2_partial] {
                Ok(unify(state, context, t2_constructor, context.prim.function)?
                    && subtype_with_mode(
                        state,
                        context,
                        t2_argument,
                        t1_argument,
                        ElaborationMode::No,
                    )?
                    && subtype_with_mode(
                        state,
                        context,
                        t1_result,
                        t2_result,
                        ElaborationMode::No,
                    )?)
            } else {
                unify(state, context, t1, t2)
            }
        }

        (_, Type::Forall(ref binder, inner)) => {
            let v = Variable::Skolem(binder.level, binder.kind);
            let t = state.storage.intern(Type::Variable(v));

            let inner = substitute::SubstituteBound::on(state, binder.level, t, inner);
            subtype_with_mode(state, context, t1, inner, mode)
        }

        (Type::Forall(ref binder, inner), _) => {
            let k = state.normalize_type(binder.kind);
            let t = state.fresh_unification_kinded(k);

            let inner = substitute::SubstituteBound::on(state, binder.level, t, inner);
            subtype_with_mode(state, context, inner, t2, mode)
        }

        (Type::Constrained(constraint, inner), _) if mode == ElaborationMode::Yes => {
            state.constraints.push_wanted(constraint);
            subtype_with_mode(state, context, inner, t2, mode)
        }

        (
            Type::Application(t1_function, t1_argument),
            Type::Application(t2_function, t2_argument),
        ) if t1_function == context.prim.record && t2_function == context.prim.record => {
            let t1_argument = synonym::normalize_expand_type(state, context, t1_argument)?;
            let t2_argument = synonym::normalize_expand_type(state, context, t2_argument)?;

            let t1_core = state.storage[t1_argument].clone();
            let t2_core = state.storage[t2_argument].clone();

            if let (Type::Row(t1_row), Type::Row(t2_row)) = (t1_core, t2_core) {
                subtype_record_rows(state, context, &t1_row, &t2_row, mode)
            } else {
                unify(state, context, t1, t2)
            }
        }

        (_, _) => unify(state, context, t1, t2),
    }
}

#[tracing::instrument(skip_all, name = "unify")]
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

    crate::debug_fields!(state, context, { t1 = t1, t2 = t2 });

    if t1 == t2 {
        crate::trace_fields!(state, context, { t1 = t1, t2 = t2 }, "identical");
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

        (Type::Function(t1_argument, t1_result), Type::Function(t2_argument, t2_result)) => {
            unify(state, context, t1_argument, t2_argument)?
                && unify(state, context, t1_result, t2_result)?
        }

        // Unify Application(Application(f, a), b) with Function(a', b').
        //
        // This handles the case where `f` is a unification variable that should
        // be solved to `Function`. For example, when checking:
        //
        //   identity :: forall t. Category a => a t t
        //
        //   monomorphic :: forall a. a -> a
        //   monomorphic = identity
        //
        // Unifying `?a ?t ?t` and `a -> a` solves `?a := Function`.
        (Type::Application(t1_partial, t1_result), Type::Function(t2_argument, t2_result)) => {
            let t1_partial = state.normalize_type(t1_partial);
            if let Type::Application(t1_constructor, t1_argument) = state.storage[t1_partial] {
                unify(state, context, t1_constructor, context.prim.function)?
                    && unify(state, context, t1_argument, t2_argument)?
                    && unify(state, context, t1_result, t2_result)?
            } else {
                false
            }
        }

        (Type::Function(t1_argument, t1_result), Type::Application(t2_partial, t2_result)) => {
            let t2_partial = state.normalize_type(t2_partial);
            if let Type::Application(t2_constructor, t2_argument) = state.storage[t2_partial] {
                unify(state, context, t2_constructor, context.prim.function)?
                    && unify(state, context, t1_argument, t2_argument)?
                    && unify(state, context, t1_result, t2_result)?
            } else {
                false
            }
        }

        (Type::Row(t1_row), Type::Row(t2_row)) => unify_rows(state, context, t1_row, t2_row)?,

        (
            Type::Variable(Variable::Bound(t1_level, t1_kind)),
            Type::Variable(Variable::Bound(t2_level, t2_kind)),
        ) => {
            if t1_level == t2_level {
                unify(state, context, t1_kind, t2_kind)?
            } else {
                false
            }
        }

        (
            Type::Variable(Variable::Skolem(t1_level, t1_kind)),
            Type::Variable(Variable::Skolem(t2_level, t2_kind)),
        ) => {
            if t1_level == t2_level {
                unify(state, context, t1_kind, t2_kind)?
            } else {
                false
            }
        }

        (Type::Unification(unification_id), _) => {
            solve(state, context, unification_id, t2)?.is_some()
        }

        (_, Type::Unification(unification_id)) => {
            solve(state, context, unification_id, t1)?.is_some()
        }

        (
            Type::Constrained(t1_constraint, t1_inner),
            Type::Constrained(t2_constraint, t2_inner),
        ) => {
            unify(state, context, t1_constraint, t2_constraint)?
                && unify(state, context, t1_inner, t2_inner)?
        }

        _ => false,
    };

    if !unifies {
        // at this point, it should be impossible to have
        // unsolved unification variables within t1 and t2
        let t1 = state.render_local_type(context, t1);
        let t2 = state.render_local_type(context, t2);
        state.insert_error(ErrorKind::CannotUnify { t1, t2 });
    }

    Ok(unifies)
}

#[tracing::instrument(skip_all, name = "solve")]
pub fn solve<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    unification_id: u32,
    solution: TypeId,
) -> QueryResult<Option<u32>>
where
    Q: ExternalQueries,
{
    crate::trace_fields!(state, context, {
        ?unification_id = unification_id,
        solution = solution,
    });

    let codomain = state.type_scope.size();

    if !promote_type(state, codomain, unification_id, solution) {
        crate::trace_fields!(state, context, {
            ?unification_id = unification_id,
            solution = solution,
        }, "occurs check failed");
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
    codomain: debruijn::Size,
    unification_id: u32,
    solution: TypeId,
) -> bool {
    let solution = state.normalize_type(solution);
    match state.storage[solution] {
        Type::Application(function, argument) => {
            promote_type(state, codomain, unification_id, function)
                && promote_type(state, codomain, unification_id, argument)
        }

        Type::Constrained(constraint, inner) => {
            promote_type(state, codomain, unification_id, constraint)
                && promote_type(state, codomain, unification_id, inner)
        }

        Type::Constructor(_, _) => true,

        Type::Forall(ref binder, inner) => {
            let inner_codomain = codomain.increment();
            promote_type(state, codomain, unification_id, binder.kind)
                && promote_type(state, inner_codomain, unification_id, inner)
        }

        Type::Function(argument, result) => {
            promote_type(state, codomain, unification_id, argument)
                && promote_type(state, codomain, unification_id, result)
        }

        Type::Integer(_) => true,

        Type::KindApplication(function, argument) => {
            promote_type(state, codomain, unification_id, function)
                && promote_type(state, codomain, unification_id, argument)
        }

        Type::Kinded(inner, kind) => {
            promote_type(state, codomain, unification_id, inner)
                && promote_type(state, codomain, unification_id, kind)
        }

        Type::Operator(_, _) => true,

        Type::OperatorApplication(_, _, left, right) => {
            promote_type(state, codomain, unification_id, left)
                && promote_type(state, codomain, unification_id, right)
        }

        Type::Row(RowType { ref fields, tail }) => {
            let fields = Arc::clone(fields);

            for field in fields.iter() {
                if !promote_type(state, codomain, unification_id, field.id) {
                    return false;
                }
            }

            if let Some(tail) = tail
                && !promote_type(state, codomain, unification_id, tail)
            {
                return false;
            }

            true
        }

        Type::String(_, _) => true,

        Type::SynonymApplication(_, _, _, ref arguments) => {
            let arguments = Arc::clone(arguments);
            for argument in arguments.iter() {
                if !promote_type(state, codomain, unification_id, *argument) {
                    return false;
                }
            }
            true
        }

        Type::Unification(solution_id) => {
            let unification = state.unification.get(unification_id);
            let solution = state.unification.get(solution_id);

            if unification_id == solution_id {
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
            // A bound variable escapes if its level >= the unification variable's domain.
            // This means the variable was bound at or after the unification was created.
            match variable {
                Variable::Bound(level, kind) => {
                    let unification = state.unification.get(unification_id);
                    if level.0 >= unification.domain.0 {
                        return false;
                    }
                    promote_type(state, codomain, unification_id, *kind)
                }
                Variable::Skolem(_, kind) => promote_type(state, codomain, unification_id, *kind),
                Variable::Free(_) => true,
            }
        }

        Type::Unknown => true,
    }
}

/// Checks that `t1_row` is a subtype of `t2_row`, generated errors for
/// additional or missing fields. This is used for record subtyping.
///
/// * This algorithm partitions row fields into common, t1-only, and t2-only fields.
/// * If t1_row is closed and t2_row is non-empty, [`ErrorKind::PropertyIsMissing`]
/// * If t2_row is closed and t1_row is non-empty, [`ErrorKind::AdditionalProperty`]
#[tracing::instrument(skip_all, name = "subtype_rows")]
fn subtype_record_rows<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    t1_row: &RowType,
    t2_row: &RowType,
    mode: ElaborationMode,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let (left_only, right_only, ok) = partition_row_fields_with(
        state,
        context,
        t1_row,
        t2_row,
        |state, context, left, right| {
            Ok(subtype_with_mode(state, context, left, right, mode)?
                && subtype_with_mode(state, context, right, left, mode)?)
        },
    )?;

    if !ok {
        return Ok(false);
    }

    let mut failed = false;

    if t1_row.tail.is_none() && !right_only.is_empty() {
        let labels = right_only.iter().map(|field| field.label.clone());
        let labels = Arc::from_iter(labels);
        state.insert_error(ErrorKind::PropertyIsMissing { labels });
        failed = true;
    }

    if t2_row.tail.is_none() && !left_only.is_empty() {
        let labels = left_only.iter().map(|field| field.label.clone());
        let labels = Arc::from_iter(labels);
        state.insert_error(ErrorKind::AdditionalProperty { labels });
        failed = true;
    }

    if failed {
        return Ok(false);
    }

    unify_row_tails(state, context, t1_row.tail, t2_row.tail, left_only, right_only)
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
    let (left_only, right_only, ok) = partition_row_fields(state, context, &t1_row, &t2_row)?;

    if !ok {
        return Ok(false);
    }

    if t1_row.tail.is_none() && t2_row.tail.is_none() {
        return Ok(left_only.is_empty() && right_only.is_empty());
    }

    if t2_row.tail.is_none() && !left_only.is_empty() {
        return Ok(false);
    }

    if t1_row.tail.is_none() && !right_only.is_empty() {
        return Ok(false);
    }

    unify_row_tails(state, context, t1_row.tail, t2_row.tail, left_only, right_only)
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
    partition_row_fields_with(state, context, t1_row, t2_row, unify)
}

fn partition_row_fields_with<Q, F>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    t1_row: &RowType,
    t2_row: &RowType,
    mut field_check: F,
) -> QueryResult<(Vec<RowField>, Vec<RowField>, bool)>
where
    Q: ExternalQueries,
    F: FnMut(&mut CheckState, &CheckContext<Q>, TypeId, TypeId) -> QueryResult<bool>,
{
    let mut extras_left = vec![];
    let mut extras_right = vec![];
    let mut ok = true;

    let t1_fields = t1_row.fields.iter();
    let t2_fields = t2_row.fields.iter();

    for field in t1_fields.merge_join_by(t2_fields, |left, right| left.label.cmp(&right.label)) {
        match field {
            EitherOrBoth::Both(left, right) => {
                if !field_check(state, context, left.id, right.id)? {
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

fn unify_row_tails<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    t1_tail: Option<TypeId>,
    t2_tail: Option<TypeId>,
    extras_left: Vec<RowField>,
    extras_right: Vec<RowField>,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    match (t1_tail, t2_tail) {
        (None, None) => Ok(true),

        (Some(t1_tail), None) => {
            let row = Type::Row(RowType { fields: Arc::from(extras_right), tail: None });
            let row_id = state.storage.intern(row);
            unify(state, context, t1_tail, row_id)
        }

        (None, Some(t2_tail)) => {
            let row = Type::Row(RowType { fields: Arc::from(extras_left), tail: None });
            let row_id = state.storage.intern(row);
            unify(state, context, t2_tail, row_id)
        }

        (Some(t1_tail), Some(t2_tail)) => {
            if extras_left.is_empty() && extras_right.is_empty() {
                return unify(state, context, t1_tail, t2_tail);
            }

            let unification = state.fresh_unification_kinded(context.prim.row_type);
            let tail = Some(unification);

            let left_tail_row = Type::Row(RowType { fields: Arc::from(extras_right), tail });
            let left_tail_row_id = state.storage.intern(left_tail_row);

            let right_tail_row = Type::Row(RowType { fields: Arc::from(extras_left), tail });
            let right_tail_row_id = state.storage.intern(right_tail_row);

            Ok(unify(state, context, t1_tail, left_tail_row_id)?
                && unify(state, context, t2_tail, right_tail_row_id)?)
        }
    }
}
