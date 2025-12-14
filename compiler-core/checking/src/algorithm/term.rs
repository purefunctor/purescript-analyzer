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
    let minimum_equation_arity =
        equations.iter().map(|equation| equation.binders.len()).min().unwrap_or(0);

    // For `Scc::Recursive` and `Scc::Mutual`, this is likely to be a
    // unification variable already, we create one for `Scc::Base` as
    // an anchor type to be unified against on each equation.
    let pending_type = state.binding_group.lookup_term(item_id);
    let group_type = pending_type.unwrap_or_else(|| state.fresh_unification_type(context));

    if pending_type.is_none() {
        state.binding_group.terms.insert(item_id, group_type);
    }

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
        let expected_type = build_function_type(state, argument_types, result_type);

        check_guarded_expression(state, context, &equation.guarded, result_type)?;
        let _ = unification::unify(state, context, group_type, expected_type)?;
    }

    Ok(())
}

fn build_function_type(state: &mut CheckState, arguments: &[TypeId], result: TypeId) -> TypeId {
    arguments
        .iter()
        .copied()
        .rfold(result, |result, argument| state.storage.intern(Type::Function(argument, result)))
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

        check_guarded_expression(state, context, &equation.guarded, signature.result)?
    }

    debug_assert!(
        state.binding_group.lookup_term(item_id).is_none(),
        "invariant violated: signatured value group appears in binding_group"
    );

    if let Some(variable) = signature.variables.first() {
        state.unbind(variable.level);
    }

    let signature = signature.restore(state);
    state.binding_group.terms.insert(item_id, signature);

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

        lowering::BinderKind::Constructor { .. } => Ok(unknown),

        lowering::BinderKind::Variable { .. } => {
            state.bind_binder(binder_id, type_id);
            Ok(type_id)
        }

        lowering::BinderKind::Named { .. } => Ok(unknown),

        lowering::BinderKind::Wildcard => Ok(unknown),

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

fn check_guarded_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    guarded: &Option<lowering::GuardedExpression>,
    expected: TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(guarded) = guarded else {
        return Ok(());
    };

    match guarded {
        lowering::GuardedExpression::Unconditional { where_expression } => {
            check_where_expression(state, context, where_expression.as_ref(), expected)
        }
        lowering::GuardedExpression::Conditionals { pattern_guarded } => {
            for pattern_guarded in pattern_guarded.iter() {
                let where_expression = &pattern_guarded.where_expression;
                check_where_expression(state, context, where_expression.as_ref(), expected)?;
            }
            Ok(())
        }
    }
}

fn check_where_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    where_expression: Option<&lowering::WhereExpression>,
    expected_type: TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(where_expr) = where_expression else {
        return Ok(());
    };

    // TODO: check let bindings in where_expr.bindings

    if let Some(expression_id) = where_expr.expression {
        check_expression(state, context, expression_id, expected_type)?;
    }

    Ok(())
}

fn check_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expr_id: lowering::ExpressionId,
    expected: TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let inferred = infer_expression(state, context, expr_id)?;
    let _ = unification::unify(state, context, expected, inferred)?;
    Ok(())
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
        lowering::ExpressionKind::Typed { .. } => Ok(unknown),

        lowering::ExpressionKind::OperatorChain { .. } => Ok(unknown),

        lowering::ExpressionKind::InfixChain { .. } => Ok(unknown),

        lowering::ExpressionKind::Negate { .. } => Ok(unknown),

        lowering::ExpressionKind::Application { function, arguments } => {
            let Some(function) = function else { return Ok(unknown) };
            let function_type = infer_expression(state, context, *function)?;
            infer_application_spine(state, context, function_type, arguments)
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

        lowering::ExpressionKind::LetIn { .. } => Ok(unknown),

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

            Ok(build_function_type(state, &argument_types, result_type))
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

        lowering::ExpressionKind::OperatorName { .. } => Ok(unknown),

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

fn infer_application_spine<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut function_type: TypeId,
    arguments: &[lowering::ExpressionArgument],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    for argument in arguments {
        function_type = state.normalize_type(function_type);

        match state.storage[function_type] {
            Type::Forall(ForallBinder { kind, .. }, inner) => {
                let unification = state.fresh_unification_kinded(kind);
                function_type = substitute::substitute_bound(state, unification, inner);
            }
            _ => {}
        }

        function_type = state.normalize_type(function_type);

        match argument {
            lowering::ExpressionArgument::Type(type_argument) => match state.storage[function_type]
            {
                Type::Forall(ForallBinder { kind, .. }, inner) => {
                    if let Some(type_argument) = type_argument {
                        let (argument_type, _) =
                            kind::check_surface_kind(state, context, *type_argument, kind)?;
                        function_type = substitute::substitute_bound(state, argument_type, inner);
                    } else {
                        let unification = state.fresh_unification_kinded(kind);
                        function_type = substitute::substitute_bound(state, unification, inner);
                    }
                }
                _ => {
                    return Ok(context.prim.unknown);
                }
            },
            lowering::ExpressionArgument::Term(term_argument) => match state.storage[function_type]
            {
                Type::Function(argument_type, result_type) => {
                    if let Some(term_argument) = term_argument {
                        check_expression(state, context, *term_argument, argument_type)?;
                    }
                    function_type = result_type;
                }
                Type::Unification(unification_id) => {
                    let argument_u = state.fresh_unification_type(context);
                    let result_u = state.fresh_unification_type(context);
                    let function_u = state.storage.intern(Type::Function(argument_u, result_u));

                    state.unification.solve(unification_id, function_u);

                    if let Some(term_argument) = term_argument {
                        check_expression(state, context, *term_argument, argument_u)?;
                    }

                    function_type = result_u;
                }
                _ => {
                    return Ok(context.prim.unknown);
                }
            },
        }
    }

    Ok(function_type)
}
