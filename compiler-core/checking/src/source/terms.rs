pub mod application;
pub mod collections;
pub mod equations;
pub mod form_ado;
pub mod form_do;
pub mod form_let;
pub mod forms;

use building_types::QueryResult;
use itertools::Itertools;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{TypeId, normalise, toolkit, unification};
use crate::error::ErrorCrumb;
use crate::source::{operator, types};
use crate::state::CheckState;

/// Checks the type of an expression.
pub fn check_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expression: lowering::ExpressionId,
    expected: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    state.with_error_crumb(ErrorCrumb::CheckingExpression(expression), |state| {
        check_expression_quiet(state, context, expression, expected)
    })
}

fn check_expression_quiet<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expression: lowering::ExpressionId,
    expected: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let expected = normalise::normalise(state, context, expected)?;
    let expected = toolkit::skolemise_forall(state, context, expected)?;
    let expected = toolkit::collect_givens(state, context, expected)?;
    if let Some(section_result) = context.sectioned.expressions.get(&expression) {
        check_sectioned_expression(state, context, expression, section_result, expected)
    } else {
        check_expression_core(state, context, expression, expected)
    }
}

fn check_sectioned_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expression: lowering::ExpressionId,
    section_result: &sugar::SectionResult,
    expected: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let mut current = expected;
    let mut parameters = vec![];

    for &section_id in section_result.iter() {
        let decomposed = toolkit::decompose_function(state, context, current)?;
        if let Some((argument_type, result_type)) = decomposed {
            state.checked.nodes.sections.insert(section_id, argument_type);
            parameters.push(argument_type);
            current = result_type;
        } else {
            let parameter = state.fresh_unification(context.queries, context.prim.t);
            let result = state.fresh_unification(context.queries, context.prim.t);

            let function = context.intern_function(parameter, result);
            unification::subtype(state, context, function, current)?;

            parameters.push(parameter);
            current = result;

            state.checked.nodes.sections.insert(section_id, parameter);
        }
    }

    let result_type = infer_expression_core(state, context, expression)?;
    let result_type = toolkit::instantiate_constrained(state, context, result_type)?;

    unification::subtype(state, context, result_type, current)?;

    let function_type = context.intern_function_list(&parameters, result_type);
    Ok(function_type)
}

fn check_expression_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expression: lowering::ExpressionId,
    expected: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let unknown = context.unknown("missing expression");

    let Some(kind) = context.lowered.info.get_expression_kind(expression) else {
        return Ok(unknown);
    };

    match kind {
        lowering::ExpressionKind::Lambda { binders, expression } => {
            forms::check_lambda(state, context, binders, *expression, expected)
        }
        lowering::ExpressionKind::IfThenElse { if_, then, else_ } => {
            forms::check_if_then_else(state, context, *if_, *then, *else_, expected)
        }
        lowering::ExpressionKind::CaseOf { trunk, branches } => {
            forms::check_case_of(state, context, trunk, branches, expected)
        }
        lowering::ExpressionKind::OperatorChain { .. } => {
            let (_, checked_type) =
                operator::check_operator_chain(state, context, expression, expected)?;
            Ok(checked_type)
        }
        lowering::ExpressionKind::LetIn { bindings, expression } => {
            forms::check_let_in(state, context, bindings, *expression, expected)
        }
        lowering::ExpressionKind::Parenthesized { parenthesized } => {
            let Some(parenthesized) = parenthesized else { return Ok(unknown) };
            check_expression(state, context, *parenthesized, expected)
        }
        lowering::ExpressionKind::Array { array } => {
            collections::check_array(state, context, array, expected)
        }
        lowering::ExpressionKind::Record { record } => {
            collections::check_record(state, context, record, expected)
        }
        _ => {
            let inferred = infer_expression_quiet(state, context, expression)?;
            let inferred = toolkit::instantiate_constrained(state, context, inferred)?;
            unification::subtype(state, context, inferred, expected)?;
            Ok(inferred)
        }
    }
}

/// Infers the type of an expression.
pub fn infer_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expression: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    state.with_error_crumb(ErrorCrumb::InferringExpression(expression), |state| {
        infer_expression_quiet(state, context, expression)
    })
}

fn infer_expression_quiet<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expression: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if let Some(section_result) = context.sectioned.expressions.get(&expression) {
        infer_sectioned_expression(state, context, expression, section_result)
    } else {
        infer_expression_core(state, context, expression)
    }
}

fn infer_sectioned_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expression: lowering::ExpressionId,
    section_result: &sugar::SectionResult,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let parameter_types = section_result.iter().map(|&section_id| {
        let parameter_type = state.fresh_unification(context.queries, context.prim.t);
        state.checked.nodes.sections.insert(section_id, parameter_type);
        parameter_type
    });

    let parameter_types = parameter_types.collect_vec();

    let result_type = infer_expression_core(state, context, expression)?;
    let result_type = toolkit::instantiate_constrained(state, context, result_type)?;

    Ok(context.intern_function_list(&parameter_types, result_type))
}

fn infer_expression_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expression: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let unknown = context.unknown("missing expression");

    let Some(kind) = context.lowered.info.get_expression_kind(expression) else {
        return Ok(unknown);
    };

    match kind {
        lowering::ExpressionKind::Typed { expression, type_ } => {
            let Some(e) = expression else { return Ok(unknown) };
            let Some(t) = type_ else { return Ok(unknown) };

            let (t, _) = types::infer_kind(state, context, *t)?;
            check_expression(state, context, *e, t)?;

            Ok(t)
        }

        lowering::ExpressionKind::OperatorChain { .. } => {
            let (_, inferred_type) = operator::infer_operator_chain(state, context, expression)?;
            Ok(inferred_type)
        }

        lowering::ExpressionKind::InfixChain { head, tail } => {
            let Some(head) = *head else { return Ok(unknown) };
            application::infer_infix_chain(state, context, head, tail)
        }

        lowering::ExpressionKind::Negate { negate, expression } => {
            let Some(negate) = negate else { return Ok(unknown) };
            let Some(expression) = expression else { return Ok(unknown) };

            let negate_type = toolkit::lookup_term_variable(state, context, *negate)?;
            application::check_function_term_application(state, context, negate_type, *expression)
        }

        lowering::ExpressionKind::Application { function, arguments } => {
            let Some(function) = function else { return Ok(unknown) };

            let mut function_t = infer_expression(state, context, *function)?;

            for argument in arguments.iter() {
                function_t =
                    application::check_function_application(state, context, function_t, argument)?;
            }

            Ok(function_t)
        }

        lowering::ExpressionKind::IfThenElse { if_, then, else_ } => {
            forms::infer_if_then_else(state, context, *if_, *then, *else_)
        }

        lowering::ExpressionKind::LetIn { bindings, expression } => {
            form_let::check_let_chunks(state, context, bindings)?;

            let Some(expression) = expression else { return Ok(unknown) };

            infer_expression(state, context, *expression)
        }

        lowering::ExpressionKind::Lambda { binders, expression } => {
            forms::infer_lambda(state, context, binders, *expression)
        }

        lowering::ExpressionKind::CaseOf { trunk, branches } => {
            forms::infer_case_of(state, context, trunk, branches)
        }

        lowering::ExpressionKind::Do { bind, discard, statements } => {
            form_do::infer_do(state, context, *bind, *discard, statements)
        }

        lowering::ExpressionKind::Ado { map, apply, pure, statements, expression } => {
            form_ado::infer_ado(state, context, *map, *apply, *pure, statements, *expression)
        }

        lowering::ExpressionKind::Constructor { resolution } => {
            let Some((file_id, term_id)) = resolution else { return Ok(unknown) };
            toolkit::lookup_file_term(state, context, *file_id, *term_id)
        }

        lowering::ExpressionKind::Variable { resolution } => {
            let Some(resolution) = *resolution else { return Ok(unknown) };
            toolkit::lookup_term_variable(state, context, resolution)
        }

        lowering::ExpressionKind::OperatorName { resolution } => {
            let Some((file_id, term_id)) = resolution else { return Ok(unknown) };
            toolkit::lookup_file_term(state, context, *file_id, *term_id)
        }

        lowering::ExpressionKind::Section => {
            if let Some(type_id) = state.checked.nodes.lookup_section(expression) {
                Ok(type_id)
            } else {
                Ok(unknown)
            }
        }

        lowering::ExpressionKind::Hole => Ok(unknown),

        lowering::ExpressionKind::String => Ok(context.prim.string),

        lowering::ExpressionKind::Char => Ok(context.prim.char),

        lowering::ExpressionKind::Boolean { .. } => Ok(context.prim.boolean),

        lowering::ExpressionKind::Integer => Ok(context.prim.int),

        lowering::ExpressionKind::Number => Ok(context.prim.number),

        lowering::ExpressionKind::Array { array } => {
            collections::infer_array(state, context, array)
        }

        lowering::ExpressionKind::Record { record } => {
            collections::infer_record(state, context, record)
        }

        lowering::ExpressionKind::Parenthesized { parenthesized } => {
            let Some(parenthesized) = parenthesized else { return Ok(unknown) };
            infer_expression(state, context, *parenthesized)
        }

        lowering::ExpressionKind::RecordAccess { record, labels } => {
            let Some(record) = *record else { return Ok(unknown) };
            let Some(labels) = labels else { return Ok(unknown) };
            collections::infer_record_access(state, context, record, labels)
        }

        lowering::ExpressionKind::RecordUpdate { record, updates } => {
            let Some(record) = *record else { return Ok(unknown) };
            collections::infer_record_update(state, context, record, updates)
        }
    }
}
