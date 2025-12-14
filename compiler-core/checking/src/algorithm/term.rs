//! Implements the type checker.

use std::iter;

use building_types::QueryResult;
use indexing::TermItemId;
use itertools::Itertools;

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{inspect, kind, substitute, transfer, unification};
use crate::core::{ForallBinder, Type, TypeId};

pub(crate) fn infer_value_group<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
    equations: &[lowering::Equation],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let item_type = state
        .binding_group
        .lookup_term(item_id)
        .expect("invariant violated: invalid binding_group in type inference");

    infer_equations(state, context, item_type, equations)
}

fn infer_equations<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_type: TypeId,
    equations: &[lowering::Equation],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let minimum_equation_arity =
        equations.iter().map(|equation| equation.binders.len()).min().unwrap_or(0);

    for equation in equations {
        let binder_count = equation.binders.len();

        let argument_types = iter::repeat_with(|| state.fresh_unification_type(context))
            .take(binder_count)
            .collect_vec();

        for (&binder_id, &argument_type) in equation.binders.iter().zip(&argument_types) {
            let _ = check_binder(state, context, binder_id, argument_type)?;
        }

        // ✨ Create unification variables for additional binders.
        // This is particularly useful for when the user is editing
        // an equation and they haven't updated the other equations
        // yet. TODO: per-binder errors, BinderId can be obtained
        // by dropping binder_count on equation.binders.
        if binder_count > minimum_equation_arity {
            let additional = binder_count - minimum_equation_arity;
            iter::repeat_with(|| state.fresh_unification_type(context))
                .take(additional)
                .for_each(drop);
        }

        let result_type = state.fresh_unification_type(context);

        // Only use the minimum number of binders across equations.
        let argument_types = &argument_types[..minimum_equation_arity];
        let expected_type = state.make_function(argument_types, result_type);
        let _ = unification::subsumes(state, context, expected_type, item_type)?;

        if let Some(guarded) = &equation.guarded {
            let inferred_type = infer_guarded_expression(state, context, &guarded)?;
            let _ = unification::subsumes(state, context, inferred_type, result_type)?;
        }
    }

    Ok(())
}

pub(crate) fn check_value_group<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
    (_, signature): (lowering::TypeId, inspect::InspectSignature),
    equations: &[lowering::Equation],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    debug_assert!(
        state.binding_group.lookup_term(item_id).is_none(),
        "invariant violated: check_value_group in binding_group"
    );

    let item_type = signature.restore(state);
    state.binding_group.terms.insert(item_id, item_type);

    check_equations(state, context, signature, equations)?;

    Ok(())
}

fn check_equations<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature: inspect::InspectSignature,
    equations: &[lowering::Equation],
) -> Result<(), building_types::QueryError>
where
    Q: ExternalQueries,
{
    let minimum_equation_arity = signature.arguments.len();

    for equation in equations {
        let binder_count = equation.binders.len();

        for (&binder_id, &argument_type) in equation.binders.iter().zip(&signature.arguments) {
            let _ = check_binder(state, context, binder_id, argument_type)?;
        }

        if binder_count > minimum_equation_arity {
            let additional = binder_count - minimum_equation_arity;
            iter::repeat_with(|| state.fresh_unification_type(context))
                .take(additional)
                .for_each(drop);
        }

        if let Some(guarded) = &equation.guarded {
            let inferred_type = infer_guarded_expression(state, context, &guarded)?;
            let _ = unification::subsumes(state, context, inferred_type, signature.result)?;
        }
    }

    if let Some(variable) = signature.variables.first() {
        state.unbind(variable.level);
    }

    Ok(())
}

fn check_binder<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    binder_id: lowering::BinderId,
    type_id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let unknown = context.prim.unknown;

    let Some(kind) = context.lowered.info.get_binder_kind(binder_id) else {
        return Ok(unknown);
    };

    match kind {
        lowering::BinderKind::Typed { .. } => Ok(unknown),

        lowering::BinderKind::OperatorChain { .. } => Ok(unknown),

        lowering::BinderKind::Integer => {
            let _ = unification::unify(state, context, context.prim.int, type_id)?;
            Ok(context.prim.int)
        }

        lowering::BinderKind::Number => {
            let _ = unification::unify(state, context, context.prim.number, type_id)?;
            Ok(context.prim.number)
        }

        lowering::BinderKind::Constructor { resolution, arguments } => {
            let Some((file_id, term_id)) = resolution else { return Ok(unknown) };

            let mut constructor_t = lookup_file_term(state, context, *file_id, *term_id)?;

            for &argument in arguments.iter() {
                constructor_t =
                    check_constructor_binder_application(state, context, constructor_t, argument)?;
            }

            let _ = unification::subsumes(state, context, constructor_t, type_id)?;

            Ok(type_id)
        }

        lowering::BinderKind::Variable { .. } => {
            state.bind_binder(binder_id, type_id);
            Ok(type_id)
        }

        lowering::BinderKind::Named { binder, .. } => {
            let Some(binder) = binder else { return Ok(unknown) };

            let type_id = check_binder(state, context, *binder, type_id)?;
            state.bind_binder(binder_id, type_id);

            Ok(type_id)
        }

        lowering::BinderKind::Wildcard => Ok(type_id),

        lowering::BinderKind::String => {
            let _ = unification::unify(state, context, context.prim.string, type_id)?;
            Ok(context.prim.string)
        }

        lowering::BinderKind::Char => {
            let _ = unification::unify(state, context, context.prim.char, type_id)?;
            Ok(context.prim.char)
        }

        lowering::BinderKind::Boolean { .. } => {
            let _ = unification::unify(state, context, context.prim.boolean, type_id)?;
            Ok(context.prim.boolean)
        }

        lowering::BinderKind::Array { .. } => Ok(unknown),

        lowering::BinderKind::Record { .. } => Ok(unknown),

        lowering::BinderKind::Parenthesized { parenthesized } => {
            let Some(parenthesized) = parenthesized else { return Ok(unknown) };
            check_binder(state, context, *parenthesized, type_id)
        }
    }
}

fn infer_guarded_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    guarded: &lowering::GuardedExpression,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    match guarded {
        lowering::GuardedExpression::Unconditional { where_expression } => {
            let Some(w) = where_expression else {
                return Ok(context.prim.unknown);
            };
            infer_where_expression(state, context, &w)
        }
        lowering::GuardedExpression::Conditionals { pattern_guarded } => {
            let mut inferred_type = context.prim.unknown;
            for pattern_guarded in pattern_guarded.iter() {
                for pattern_guard in pattern_guarded.pattern_guards.iter() {
                    check_pattern_guard(state, context, pattern_guard)?;
                }
                if let Some(w) = &pattern_guarded.where_expression {
                    inferred_type = infer_where_expression(state, context, w)?;
                }
            }
            Ok(inferred_type)
        }
    }
}

fn check_pattern_guard<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    guard: &lowering::PatternGuard,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(e) = guard.expression else {
        return Ok(());
    };

    let t = infer_expression(state, context, e)?;

    let Some(b) = guard.binder else {
        return Ok(());
    };

    let _ = check_binder(state, context, b, t)?;

    Ok(())
}

fn infer_where_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    where_expression: &lowering::WhereExpression,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    for binding in where_expression.bindings.iter() {
        check_let_binding(state, context, binding)?;
    }

    let Some(expression) = where_expression.expression else {
        return Ok(context.prim.unknown);
    };

    infer_expression(state, context, expression)
}

fn check_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expr_id: lowering::ExpressionId,
    expected: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let inferred = infer_expression(state, context, expr_id)?;
    let _ = unification::subsumes(state, context, inferred, expected)?;
    Ok(inferred)
}

fn infer_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expr_id: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let unknown = context.prim.unknown;

    let Some(kind) = context.lowered.info.get_expression_kind(expr_id) else {
        return Ok(unknown);
    };

    match kind {
        lowering::ExpressionKind::Typed { expression, type_ } => {
            let Some(e) = expression else { return Ok(unknown) };
            let Some(t) = type_ else { return Ok(unknown) };

            let (t, _) = kind::infer_surface_kind(state, context, *t)?;
            check_expression(state, context, *e, t)?;

            Ok(t)
        }

        lowering::ExpressionKind::OperatorChain { .. } => Ok(unknown),

        lowering::ExpressionKind::InfixChain { .. } => Ok(unknown),

        lowering::ExpressionKind::Negate { .. } => Ok(unknown),

        lowering::ExpressionKind::Application { function, arguments } => {
            let Some(function) = function else { return Ok(unknown) };

            let mut function_t = infer_expression(state, context, *function)?;

            for argument in arguments.iter() {
                function_t = check_function_application(state, context, function_t, argument)?;
            }

            Ok(function_t)
        }

        lowering::ExpressionKind::IfThenElse { if_, then, else_ } => {
            if let Some(if_) = if_ {
                check_expression(state, context, *if_, context.prim.boolean)?;
            }

            let result_type = state.fresh_unification_type(context);

            if let Some(then) = then {
                check_expression(state, context, *then, result_type)?;
            }

            if let Some(else_) = else_ {
                check_expression(state, context, *else_, result_type)?;
            }

            Ok(result_type)
        }

        lowering::ExpressionKind::LetIn { bindings, expression } => {
            for binding in bindings.iter() {
                check_let_binding(state, context, binding)?;
            }

            let Some(expression) = expression else { return Ok(unknown) };

            infer_expression(state, context, *expression)
        }

        lowering::ExpressionKind::Lambda { binders, expression } => {
            let mut argument_types = vec![];

            for &binder_id in binders.iter() {
                let argument_type = state.fresh_unification_type(context);
                let _ = check_binder(state, context, binder_id, argument_type)?;
                argument_types.push(argument_type);
            }

            let result_type = if let Some(body) = expression {
                infer_expression(state, context, *body)?
            } else {
                state.fresh_unification_type(context)
            };

            Ok(state.make_function(&argument_types, result_type))
        }

        lowering::ExpressionKind::CaseOf { .. } => Ok(unknown),

        lowering::ExpressionKind::Do { .. } => Ok(unknown),

        lowering::ExpressionKind::Ado { .. } => Ok(unknown),

        lowering::ExpressionKind::Constructor { resolution } => {
            let Some((file_id, term_id)) = resolution else { return Ok(unknown) };
            lookup_file_term(state, context, *file_id, *term_id)
        }

        lowering::ExpressionKind::Variable { resolution } => match resolution {
            Some(lowering::TermVariableResolution::Binder(binder_id)) => {
                Ok(state.lookup_binder(*binder_id).unwrap_or(unknown))
            }
            Some(lowering::TermVariableResolution::Let(let_binding_id)) => {
                Ok(state.lookup_let(*let_binding_id).unwrap_or(unknown))
            }
            Some(lowering::TermVariableResolution::Reference(file_id, term_id)) => {
                lookup_file_term(state, context, *file_id, *term_id)
            }
            None => Ok(unknown),
        },

        lowering::ExpressionKind::OperatorName { resolution } => {
            let Some((file_id, term_id)) = resolution else { return Ok(unknown) };
            lookup_file_term(state, context, *file_id, *term_id)
        }

        lowering::ExpressionKind::Section => Ok(unknown),

        lowering::ExpressionKind::Hole => Ok(unknown),

        lowering::ExpressionKind::String => Ok(context.prim.string),

        lowering::ExpressionKind::Char => Ok(context.prim.char),

        lowering::ExpressionKind::Boolean { .. } => Ok(context.prim.boolean),

        lowering::ExpressionKind::Integer => Ok(context.prim.int),

        lowering::ExpressionKind::Number => Ok(context.prim.number),

        lowering::ExpressionKind::Array { .. } => Ok(unknown),

        lowering::ExpressionKind::Record { .. } => Ok(unknown),

        lowering::ExpressionKind::Parenthesized { parenthesized } => {
            let Some(parenthesized) = parenthesized else { return Ok(unknown) };
            infer_expression(state, context, *parenthesized)
        }

        lowering::ExpressionKind::RecordAccess { .. } => Ok(unknown),

        lowering::ExpressionKind::RecordUpdate { .. } => Ok(unknown),
    }
}

fn lookup_file_term<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: files::FileId,
    term_id: indexing::TermItemId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let term_id = if file_id == context.id {
        if let Some(&k) = state.binding_group.terms.get(&term_id) {
            k
        } else if let Some(&k) = state.checked.terms.get(&term_id) {
            transfer::localize(state, context, k)
        } else {
            context.prim.unknown
        }
    } else {
        let checked = context.queries.checked(file_id)?;
        if let Some(id) = checked.terms.get(&term_id) {
            transfer::localize(state, context, *id)
        } else {
            context.prim.unknown
        }
    };
    Ok(term_id)
}

fn check_function_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function_t: TypeId,
    argument: &lowering::ExpressionArgument,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    match argument {
        lowering::ExpressionArgument::Type(type_argument) => {
            let Some(type_argument) = type_argument else { return Ok(context.prim.unknown) };
            check_function_type_application(state, context, function_t, *type_argument)
        }
        lowering::ExpressionArgument::Term(term_argument) => {
            let Some(term_argument) = term_argument else { return Ok(context.prim.unknown) };
            check_function_term_application(state, context, function_t, *term_argument)
        }
    }
}

fn check_function_type_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function_t: TypeId,
    argument: lowering::TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let function_t = state.normalize_type(function_t);
    match state.storage[function_t] {
        Type::Forall(ForallBinder { kind, .. }, inner) => {
            let (argument_type, _) = kind::check_surface_kind(state, context, argument, kind)?;
            Ok(substitute::substitute_bound(state, argument_type, inner))
        }

        _ => Ok(context.prim.unknown),
    }
}

fn check_function_application_core<Q, A, F>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function_t: TypeId,
    argument_id: A,
    check_argument: F,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
    F: Fn(&mut CheckState, &CheckContext<Q>, A, TypeId) -> QueryResult<TypeId>,
{
    let function_t = state.normalize_type(function_t);
    match state.storage[function_t] {
        Type::Function(argument_type, result_type) => {
            check_argument(state, context, argument_id, argument_type)?;
            Ok(result_type)
        }

        Type::Unification(unification_id) => {
            let argument_u = state.fresh_unification_type(context);
            let result_u = state.fresh_unification_type(context);
            let function_u = state.storage.intern(Type::Function(argument_u, result_u));

            state.unification.solve(unification_id, function_u);
            check_argument(state, context, argument_id, argument_u)?;

            Ok(result_u)
        }

        Type::Forall(ForallBinder { kind, .. }, function_t) => {
            let unification = state.fresh_unification_kinded(kind);
            let function_t = substitute::substitute_bound(state, unification, function_t);
            check_function_application_core(state, context, function_t, argument_id, check_argument)
        }

        _ => Ok(context.prim.unknown),
    }
}

fn check_function_term_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function_t: TypeId,
    expression_id: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    check_function_application_core(state, context, function_t, expression_id, check_expression)
}
fn check_constructor_binder_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    constructor_t: TypeId,
    binder_id: lowering::BinderId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    check_function_application_core(state, context, constructor_t, binder_id, check_binder)
}

fn check_let_binding<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    binding: &lowering::LetBinding,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    match binding {
        lowering::LetBinding::Name { id } => {
            let Some(name) = context.lowered.info.get_let_binding(*id) else {
                return Ok(());
            };
            if let Some(signature) = name.signature {
                let signature = inspect::inspect_signature(state, context, signature)?;
                let name_type = signature.restore(state);
                state.bind_let(*id, name_type);
                check_equations(state, context, signature, &name.equations)?;
            } else {
                let name_type = state.fresh_unification_type(context);
                state.bind_let(*id, name_type);
                infer_equations(state, context, name_type, &name.equations)?;
            }
        }
        lowering::LetBinding::Pattern { binder, where_expression } => {
            let Some(w) = where_expression else {
                return Ok(());
            };

            let t = infer_where_expression(state, context, w)?;

            let Some(b) = binder else {
                return Ok(());
            };

            let _ = check_binder(state, context, *b, t)?;
        }
    }

    Ok(())
}
