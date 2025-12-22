//! Implements the type checker.


use building_types::QueryResult;
use indexing::TermItemId;
use itertools::Itertools;
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{inspect, kind, operator, substitute, transfer, unification};
use crate::core::{RowField, RowType, Type, TypeId};
use crate::error::ErrorKind;

pub fn infer_equations<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
    equations: &[lowering::Equation],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let group_type = state
        .binding_group
        .lookup_term(item_id)
        .expect("invariant violated: invalid binding_group in type inference");

    infer_equations_core(state, context, group_type, equations)
}

fn infer_equations_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    group_type: TypeId,
    equations: &[lowering::Equation],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let minimum_equation_arity =
        equations.iter().map(|equation| equation.binders.len()).min().unwrap_or(0);

    for equation in equations {
        let mut argument_types = vec![];
        for &binder_id in equation.binders.iter() {
            let argument_type = infer_binder(state, context, binder_id)?;
            argument_types.push(argument_type);
        }

        let result_type = state.fresh_unification_type(context);

        // Only use the minimum number of binders across equations.
        let argument_types = &argument_types[..minimum_equation_arity];
        let equation_type = state.make_function(argument_types, result_type);
        let _ = unification::subtype(state, context, equation_type, group_type)?;

        if let Some(guarded) = &equation.guarded {
            let inferred_type = infer_guarded_expression(state, context, guarded)?;
            let _ = unification::subtype(state, context, inferred_type, result_type)?;
        }
    }

    Ok(())
}

pub fn check_equations<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature_id: lowering::TypeId,
    signature: inspect::InspectSignature,
    equations: &[lowering::Equation],
) -> Result<(), building_types::QueryError>
where
    Q: ExternalQueries,
{
    let expected_arity = signature.arguments.len();

    for equation in equations {
        let equation_arity = equation.binders.len();

        if equation_arity > expected_arity {
            let expected = expected_arity as u32;
            let actual = equation_arity as u32;
            state.insert_error(ErrorKind::TooManyBinders {
                signature: signature_id,
                expected,
                actual,
            });
        }

        for (&binder_id, &argument_type) in equation.binders.iter().zip(&signature.arguments) {
            let _ = check_binder(state, context, binder_id, argument_type)?;
        }

        if equation_arity > expected_arity {
            let extra_binders = &equation.binders[expected_arity..];
            for &binder_id in extra_binders {
                let _ = infer_binder(state, context, binder_id)?;
            }
        }

        // Compute expected result type based on how many binders there
        // are on each equation, wrapping remaining arguments if partial.
        //
        // foo :: forall a. a -> a -> Int
        // foo = \a b -> a + b
        // foo a = \b -> a + b
        // foo a b = a + b
        //
        // signature.arguments := [a, a]
        // signature.result    := Int
        //
        // expected_type :=
        //   0 binders := forall a. a -> a -> Int
        //   1 binder  := a -> Int
        //   2 binders := Int
        //
        // This matters for type synonyms that expand to functions. The
        // return type synonym introduces hidden function arrows that
        // increase the expected arity after expansion.
        //
        // type ReturnsInt a = a -> Int
        //
        // bar :: forall a. ReturnsInt a -> ReturnsInt a
        // bar = \f -> f
        // bar f = f
        // bar f a = f a
        //
        // signature.arguments := [ReturnsInt a, a]
        // signature.result    := Int
        //
        // expected_type :=
        //   0 binders := forall a. ReturnsInt a -> ReturnsInt a
        //   1 binder  := ReturnsInt a
        //   2 binders := Int
        let expected_type = if equation_arity == 0 {
            signature.restore(state)
        } else if equation_arity >= expected_arity {
            signature.result
        } else {
            let remaining_arguments = &signature.arguments[equation_arity..];
            remaining_arguments.iter().rfold(signature.result, |result, &argument| {
                state.storage.intern(Type::Function(argument, result))
            })
        };

        if let Some(guarded) = &equation.guarded {
            let inferred_type = infer_guarded_expression(state, context, guarded)?;
            let _ = unification::subtype(state, context, inferred_type, expected_type)?;
        }
    }

    if let Some(variable) = signature.variables.first() {
        state.unbind(variable.level);
    }

    Ok(())
}

pub fn infer_binder<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    binder_id: lowering::BinderId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let unknown = context.prim.unknown;

    let Some(kind) = context.lowered.info.get_binder_kind(binder_id) else {
        return Ok(unknown);
    };

    match kind {
        lowering::BinderKind::Typed { binder, type_ } => {
            let Some(b) = binder else { return Ok(unknown) };
            let Some(t) = type_ else { return Ok(unknown) };

            let (t, _) = kind::infer_surface_kind(state, context, *t)?;
            check_binder(state, context, *b, t)?;

            Ok(t)
        }

        lowering::BinderKind::OperatorChain { .. } => {
            let (_, inferred_type) = operator::infer_operator_chain(state, context, binder_id)?;
            Ok(inferred_type)
        }

        lowering::BinderKind::Integer => Ok(context.prim.int),

        lowering::BinderKind::Number => Ok(context.prim.number),

        lowering::BinderKind::Constructor { resolution, arguments } => {
            let Some((file_id, term_id)) = resolution else { return Ok(unknown) };

            let constructor_t = lookup_file_term(state, context, *file_id, *term_id)?;

            // Instantiate nullary constructors to avoid polymorphic types.
            // Non-nullary constructors are instantiated during application.
            if arguments.is_empty() {
                return instantiate_type(state, context, constructor_t);
            }

            let mut constructor_t = constructor_t;
            for &argument in arguments.iter() {
                constructor_t =
                    check_constructor_binder_application(state, context, constructor_t, argument)?;
            }

            Ok(constructor_t)
        }

        lowering::BinderKind::Variable { .. } => {
            let type_id = state.fresh_unification_type(context);
            state.bind_binder(binder_id, type_id);
            Ok(type_id)
        }

        lowering::BinderKind::Named { binder, .. } => {
            let Some(binder) = binder else { return Ok(unknown) };

            let type_id = infer_binder(state, context, *binder)?;
            state.bind_binder(binder_id, type_id);

            Ok(type_id)
        }

        lowering::BinderKind::Wildcard => {
            let type_id = state.fresh_unification_type(context);
            Ok(type_id)
        }

        lowering::BinderKind::String => Ok(context.prim.string),

        lowering::BinderKind::Char => Ok(context.prim.char),

        lowering::BinderKind::Boolean { .. } => Ok(context.prim.boolean),

        lowering::BinderKind::Array { array } => {
            let inferred_type = state.fresh_unification_type(context);

            for binder in array.iter() {
                let element_type = infer_binder(state, context, *binder)?;
                unification::subtype(state, context, element_type, inferred_type)?;
            }

            let array_type =
                state.storage.intern(Type::Application(context.prim.array, inferred_type));
            Ok(array_type)
        }

        lowering::BinderKind::Record { record } => {
            let mut fields = vec![];

            for field in record.iter() {
                match field {
                    lowering::BinderRecordItem::RecordField { name, value } => {
                        let Some(name) = name else { continue };
                        let Some(value) = value else { continue };

                        let label = SmolStr::clone(name);
                        let id = infer_binder(state, context, *value)?;

                        fields.push(RowField { label, id });
                    }
                    lowering::BinderRecordItem::RecordPun { id, name } => {
                        let Some(name) = name else { continue };

                        let label = SmolStr::clone(name);
                        let field_type = state.fresh_unification_type(context);
                        state.bind_pun(*id, field_type);

                        fields.push(RowField { label, id: field_type });
                    }
                }
            }

            let row_type = RowType::from_unsorted(fields, None);
            let row_type = state.storage.intern(Type::Row(row_type));

            let record_type =
                state.storage.intern(Type::Application(context.prim.record, row_type));

            Ok(record_type)
        }

        lowering::BinderKind::Parenthesized { parenthesized } => {
            let Some(parenthesized) = parenthesized else { return Ok(unknown) };
            infer_binder(state, context, *parenthesized)
        }
    }
}

pub fn check_binder<Q>(
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
        lowering::BinderKind::Typed { .. } => {
            let inferred_type = infer_binder(state, context, binder_id)?;
            let _ = unification::subtype(state, context, inferred_type, type_id)?;
            Ok(inferred_type)
        }

        lowering::BinderKind::OperatorChain { .. } => {
            let inferred_type = infer_binder(state, context, binder_id)?;
            let _ = unification::subtype(state, context, inferred_type, type_id)?;
            Ok(inferred_type)
        }

        lowering::BinderKind::Integer => {
            let inferred_type = infer_binder(state, context, binder_id)?;
            let _ = unification::unify(state, context, inferred_type, type_id)?;
            Ok(inferred_type)
        }

        lowering::BinderKind::Number => {
            let inferred_type = infer_binder(state, context, binder_id)?;
            let _ = unification::unify(state, context, inferred_type, type_id)?;
            Ok(inferred_type)
        }

        lowering::BinderKind::Constructor { .. } => {
            let inferred_type = infer_binder(state, context, binder_id)?;
            let _ = unification::subtype(state, context, inferred_type, type_id)?;
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
            let inferred_type = infer_binder(state, context, binder_id)?;
            let _ = unification::unify(state, context, inferred_type, type_id)?;
            Ok(inferred_type)
        }

        lowering::BinderKind::Char => {
            let inferred_type = infer_binder(state, context, binder_id)?;
            let _ = unification::unify(state, context, inferred_type, type_id)?;
            Ok(inferred_type)
        }

        lowering::BinderKind::Boolean { .. } => {
            let inferred_type = infer_binder(state, context, binder_id)?;
            let _ = unification::unify(state, context, inferred_type, type_id)?;
            Ok(inferred_type)
        }

        lowering::BinderKind::Array { .. } => {
            let inferred_type = infer_binder(state, context, binder_id)?;
            let _ = unification::subtype(state, context, inferred_type, type_id)?;
            Ok(inferred_type)
        }

        lowering::BinderKind::Record { .. } => {
            let inferred_type = infer_binder(state, context, binder_id)?;
            let _ = unification::subtype(state, context, inferred_type, type_id)?;
            Ok(inferred_type)
        }

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
            infer_where_expression(state, context, w)
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

pub fn check_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expr_id: lowering::ExpressionId,
    expected: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let inferred = infer_expression(state, context, expr_id)?;
    let _ = unification::subtype(state, context, inferred, expected)?;
    Ok(inferred)
}

pub fn infer_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expr_id: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if let Some(section_result) = context.sectioned.expressions.get(&expr_id) {
        infer_sectioned_expression(state, context, expr_id, section_result)
    } else {
        infer_expression_core(state, context, expr_id)
    }
}

fn infer_sectioned_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expr_id: lowering::ExpressionId,
    section_result: &sugar::SectionResult,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let parameter_types = section_result.iter().map(|&section_id| {
        let parameter_type = state.fresh_unification_type(context);
        state.env_section.insert(section_id, parameter_type);
        parameter_type
    });

    let parameter_types = parameter_types.collect_vec();
    let result_type = infer_expression_core(state, context, expr_id)?;

    Ok(state.make_function(&parameter_types, result_type))
}

fn infer_expression_core<Q>(
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

        lowering::ExpressionKind::OperatorChain { .. } => {
            let (_, inferred_type) = operator::infer_operator_chain(state, context, expr_id)?;
            Ok(inferred_type)
        }

        lowering::ExpressionKind::InfixChain { head, tail } => {
            let Some(head) = *head else { return Ok(unknown) };

            let mut infix_type = infer_expression(state, context, head)?;

            for lowering::InfixPair { tick, element } in tail.iter() {
                let Some(tick) = *tick else { return Ok(unknown) };
                let Some(element) = *element else { return Ok(unknown) };

                let tick_type = infer_expression(state, context, tick)?;

                let applied_tick = check_function_application_core(
                    state,
                    context,
                    tick_type,
                    infix_type,
                    |state, context, infix_type, expected_type| {
                        let _ = unification::subtype(state, context, infix_type, expected_type)?;
                        Ok(infix_type)
                    },
                )?;

                infix_type =
                    check_function_term_application(state, context, applied_tick, element)?;
            }

            Ok(infix_type)
        }

        lowering::ExpressionKind::Negate { negate, expression } => {
            let Some(negate) = negate else { return Ok(unknown) };
            let Some(expression) = expression else { return Ok(unknown) };

            let negate_type = lookup_term_variable(state, context, *negate)?;
            check_function_term_application(state, context, negate_type, *expression)
        }

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

        lowering::ExpressionKind::CaseOf { trunk, branches } => {
            let inferred_type = state.fresh_unification_type(context);

            let mut trunk_types = vec![];
            for trunk in trunk.iter() {
                let trunk_type = infer_expression(state, context, *trunk)?;
                trunk_types.push(trunk_type);
            }

            for branch in branches.iter() {
                for (binder, trunk) in branch.binders.iter().zip(&trunk_types) {
                    let _ = check_binder(state, context, *binder, *trunk)?;
                }
                if let Some(guarded) = &branch.guarded_expression {
                    let guarded_type = infer_guarded_expression(state, context, guarded)?;
                    let _ = unification::subtype(state, context, inferred_type, guarded_type)?;
                }
            }

            Ok(inferred_type)
        }

        lowering::ExpressionKind::Do { bind, discard, statements } => {
            let Some(bind) = bind else { return Ok(unknown) };
            let Some(discard) = discard else { return Ok(unknown) };

            let bind_type = lookup_term_variable(state, context, *bind)?;
            let discard_type = lookup_term_variable(state, context, *discard)?;

            // First, perform a forward pass where variable bindings are
            // bound to unification variables and let bindings are checked.
            // This is like inferring the lambda in a desugared representation.
            let mut do_statements = vec![];
            for statement in statements.iter() {
                match statement {
                    lowering::DoStatement::Bind { binder, expression } => {
                        let binder_type = if let Some(binder) = binder {
                            infer_binder(state, context, *binder)?
                        } else {
                            state.fresh_unification_type(context)
                        };
                        do_statements.push((Some(binder_type), *expression));
                    }
                    lowering::DoStatement::Let { statements } => {
                        for statement in statements.iter() {
                            check_let_binding(state, context, statement)?;
                        }
                    }
                    lowering::DoStatement::Discard { expression } => {
                        do_statements.push((None, *expression));
                    }
                }
            }

            let [bind_statements @ .., (_, pure_expression)] = &do_statements[..] else {
                unreachable!("invariant violated: empty do_statements");
            };

            let Some(pure_expression) = pure_expression else {
                return Ok(unknown);
            };

            // With the binders and let-bound names in scope, infer
            // the type of the last expression as our starting point.
            //
            // main = do
            //   pure 42
            //   y <- pure "Hello!"
            //   pure $ Message y
            //
            // accumulated_type := Effect Message
            let mut accumulated_type = infer_expression(state, context, *pure_expression)?;

            // Then, infer do statements in reverse order to emulate
            // inside-out type inference for desugared do statements.
            for (binder, expression) in bind_statements.iter().rev() {
                accumulated_type = if let Some(binder) = binder {
                    // This applies bind_type to expression_type to get
                    // bind_applied, which is then applied to lambda_type
                    // to get the accumulated_type and to solve ?y.
                    //
                    // bind_type        := m a -> (a -> m b) -> m b
                    // expression_type  := Effect String
                    //
                    // bind_applied     := (String -> Effect b) -> Effect b
                    // lambda_type      := ?y -> Effect Message
                    //
                    // accumulated_type := Effect Message
                    infer_do_bind(
                        state,
                        context,
                        bind_type,
                        accumulated_type,
                        *expression,
                        *binder,
                    )?
                } else {
                    // This applies discard_type to expression_type to
                    // get discard_applied, which is then deconstructed
                    // to subsume against the `Effect b`.
                    //
                    // discard_type     := m a -> (a -> m b) -> m b
                    // expression_type  := Effect Int
                    //
                    // discard_applied  := (Int -> Effect b) -> Effect b
                    // accumulated_type := Effect Message
                    //
                    // accumulated_type <: Effect b
                    infer_do_discard(state, context, discard_type, accumulated_type, *expression)?
                }
            }

            Ok(accumulated_type)
        }

        lowering::ExpressionKind::Ado { map, apply, statements, expression } => {
            let Some(map) = map else { return Ok(unknown) };
            let Some(apply) = apply else { return Ok(unknown) };

            let map_type = lookup_term_variable(state, context, *map)?;
            let apply_type = lookup_term_variable(state, context, *apply)?;

            // First, perform a forward pass where variable bindings are
            // bound to unification variables and let bindings are checked.
            // This is like inferring the lambda in a desugared representation.
            let mut binder_types = vec![];
            let mut expressions = vec![];
            for statement in statements.iter() {
                match statement {
                    lowering::DoStatement::Bind { binder, expression } => {
                        let binder_type = if let Some(binder) = binder {
                            infer_binder(state, context, *binder)?
                        } else {
                            state.fresh_unification_type(context)
                        };
                        binder_types.push(binder_type);
                        expressions.push(*expression);
                    }
                    lowering::DoStatement::Let { statements } => {
                        for statement in statements.iter() {
                            check_let_binding(state, context, statement)?;
                        }
                    }
                    lowering::DoStatement::Discard { expression } => {
                        let binder_type = state.fresh_unification_type(context);
                        binder_types.push(binder_type);
                        expressions.push(*expression);
                    }
                }
            }

            assert_eq!(binder_types.len(), expressions.len());

            if expressions.is_empty() {
                return if let Some(expression) = expression {
                    infer_expression(state, context, *expression)
                } else {
                    Ok(unknown)
                };
            }

            // With the binders and let-bound names in scope, infer
            // the type of the final expression as our starting point.
            //
            // main = ado
            //   pure 1
            //   y <- pure "Hello!"
            //   in Message y
            //
            // expression_type := Message
            let expression_type = if let Some(expression) = expression {
                infer_expression(state, context, *expression)?
            } else {
                state.fresh_unification_type(context)
            };

            // Create a function type using the binder types collected
            // from the forward pass. We made sure to allocate unification
            // variables for the discard statements too.
            //
            // lambda_type := ?discard -> ?y -> Message
            let lambda_type = state.make_function(&binder_types, expression_type);

            let [expression, tail_expressions @ ..] = &expressions[..] else {
                unreachable!("invariant violated: empty ado_statements");
            };

            // This applies map_type to the lambda_type that we just built
            // and then to the inferred type of the first expression.
            //
            // map_type         := (a -> b) -> f a -> f b
            // lambda_type      := ?discard -> ?y -> Message
            //
            // map_applied      := f ?discard -> f (?y -> Message)
            // expression_type  := f Int
            //
            // accumulated_type := f (?y -> Message)
            let mut accumulated_type =
                infer_ado_map(state, context, map_type, lambda_type, *expression)?;

            //
            // This applies apply_type to the accumulated_type, and then to the
            // inferred type of the expression to update the accumulated_type.
            //
            // apply_type       := f (a -> b) -> f a -> f b
            // accumulated_type := f (?y -> Message)
            //
            // accumulated_type := f ?y -> f Message
            // expression_type  := f String
            //
            // accumulated_type := f Message
            for expression in tail_expressions {
                accumulated_type =
                    infer_ado_apply(state, context, apply_type, accumulated_type, *expression)?;
            }

            Ok(accumulated_type)
        }

        lowering::ExpressionKind::Constructor { resolution } => {
            let Some((file_id, term_id)) = resolution else { return Ok(unknown) };
            lookup_file_term(state, context, *file_id, *term_id)
        }

        lowering::ExpressionKind::Variable { resolution } => {
            let Some(resolution) = *resolution else { return Ok(unknown) };
            lookup_term_variable(state, context, resolution)
        }

        lowering::ExpressionKind::OperatorName { resolution } => {
            let Some((file_id, term_id)) = resolution else { return Ok(unknown) };
            lookup_file_term(state, context, *file_id, *term_id)
        }

        lowering::ExpressionKind::Section => {
            if let Some(&type_id) = state.env_section.get(&expr_id) {
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
            let inferred_type = state.fresh_unification_type(context);

            for expression in array.iter() {
                let element_type = infer_expression(state, context, *expression)?;
                unification::subtype(state, context, element_type, inferred_type)?;
            }

            let array_type =
                state.storage.intern(Type::Application(context.prim.array, inferred_type));

            Ok(array_type)
        }

        lowering::ExpressionKind::Record { record } => {
            let mut fields = vec![];

            for field in record.iter() {
                match field {
                    lowering::ExpressionRecordItem::RecordField { name, value } => {
                        let Some(name) = name else { continue };
                        let Some(value) = value else { continue };

                        let label = SmolStr::clone(name);
                        let id = infer_expression(state, context, *value)?;

                        // Instantiate to avoid polymorphic types in record fields.
                        let id = instantiate_type(state, context, id)?;

                        fields.push(RowField { label, id });
                    }
                    lowering::ExpressionRecordItem::RecordPun { name, resolution } => {
                        let Some(name) = name else { continue };
                        let Some(resolution) = resolution else { continue };

                        let label = SmolStr::clone(name);
                        let id = lookup_term_variable(state, context, *resolution)?;

                        // Instantiate to avoid polymorphic types in record fields.
                        let id = instantiate_type(state, context, id)?;

                        fields.push(RowField { label, id });
                    }
                }
            }

            let row_type = RowType::from_unsorted(fields, None);
            let row_type = state.storage.intern(Type::Row(row_type));

            let record_type =
                state.storage.intern(Type::Application(context.prim.record, row_type));

            Ok(record_type)
        }

        lowering::ExpressionKind::Parenthesized { parenthesized } => {
            let Some(parenthesized) = parenthesized else { return Ok(unknown) };
            infer_expression(state, context, *parenthesized)
        }

        lowering::ExpressionKind::RecordAccess { record, labels } => {
            let Some(record) = record else { return Ok(unknown) };
            let Some(labels) = labels else { return Ok(unknown) };

            let mut current_type = infer_expression(state, context, *record)?;

            for label in labels.iter() {
                let label = SmolStr::clone(label);

                let field_type = state.fresh_unification_type(context);

                let row_type_kind =
                    state.storage.intern(Type::Application(context.prim.row, context.prim.t));

                let tail_type = state.fresh_unification_kinded(row_type_kind);

                let row_type = RowType::from_unsorted(
                    vec![RowField { label, id: field_type }],
                    Some(tail_type),
                );

                let row_type = state.storage.intern(Type::Row(row_type));
                let record_type =
                    state.storage.intern(Type::Application(context.prim.record, row_type));

                unification::subtype(state, context, current_type, record_type)?;
                current_type = field_type;
            }

            Ok(current_type)
        }

        lowering::ExpressionKind::RecordUpdate { record, updates } => {
            let Some(record) = record else { return Ok(unknown) };

            let (input_fields, output_fields, tail) =
                infer_record_updates(state, context, updates)?;

            let input_row = RowType::from_unsorted(input_fields, Some(tail));
            let input_row = state.storage.intern(Type::Row(input_row));
            let input_record =
                state.storage.intern(Type::Application(context.prim.record, input_row));

            let output_row = RowType::from_unsorted(output_fields, Some(tail));
            let output_row = state.storage.intern(Type::Row(output_row));
            let output_record =
                state.storage.intern(Type::Application(context.prim.record, output_row));

            check_expression(state, context, *record, input_record)?;

            Ok(output_record)
        }
    }
}

fn infer_record_updates<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    updates: &[lowering::RecordUpdate],
) -> QueryResult<(Vec<RowField>, Vec<RowField>, TypeId)>
where
    Q: ExternalQueries,
{
    let mut input_fields = vec![];
    let mut output_fields = vec![];

    for update in updates {
        match update {
            lowering::RecordUpdate::Leaf { name, expression } => {
                let Some(name) = name else { continue };
                let label = SmolStr::clone(name);

                let input_id = state.fresh_unification_type(context);
                let output_id = if let Some(expression) = expression {
                    infer_expression(state, context, *expression)?
                } else {
                    context.prim.unknown
                };

                input_fields.push(RowField { label: label.clone(), id: input_id });
                output_fields.push(RowField { label, id: output_id });
            }
            lowering::RecordUpdate::Branch { name, updates } => {
                let Some(name) = name else { continue };
                let label = SmolStr::clone(name);

                let (in_f, out_f, tail) = infer_record_updates(state, context, updates)?;

                let in_row = RowType::from_unsorted(in_f, Some(tail));
                let in_row = state.storage.intern(Type::Row(in_row));
                let in_id = state.storage.intern(Type::Application(context.prim.record, in_row));

                let out_row = RowType::from_unsorted(out_f, Some(tail));
                let out_row = state.storage.intern(Type::Row(out_row));
                let out_id = state.storage.intern(Type::Application(context.prim.record, out_row));

                input_fields.push(RowField { label: label.clone(), id: in_id });
                output_fields.push(RowField { label, id: out_id });
            }
        }
    }

    let row_type_kind = state.storage.intern(Type::Application(context.prim.row, context.prim.t));
    let tail = state.fresh_unification_kinded(row_type_kind);

    Ok((input_fields, output_fields, tail))
}

fn lookup_term_variable<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    resolution: lowering::TermVariableResolution,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    match resolution {
        lowering::TermVariableResolution::Binder(binder_id) => {
            Ok(state.lookup_binder(binder_id).unwrap_or(context.prim.unknown))
        }
        lowering::TermVariableResolution::Let(let_binding_id) => {
            Ok(state.lookup_let(let_binding_id).unwrap_or(context.prim.unknown))
        }
        lowering::TermVariableResolution::RecordPun(pun_id) => {
            Ok(state.lookup_pun(pun_id).unwrap_or(context.prim.unknown))
        }
        lowering::TermVariableResolution::Reference(file_id, term_id) => {
            lookup_file_term(state, context, file_id, term_id)
        }
    }
}

fn infer_ado_map<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    map_type: TypeId,
    lambda_type: TypeId,
    expression: Option<lowering::ExpressionId>,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let Some(expression) = expression else {
        return Ok(context.prim.unknown);
    };

    let expression_type = infer_expression(state, context, expression)?;

    let map_applied = check_function_application_core(
        state,
        context,
        map_type,
        lambda_type,
        |state, context, lambda_type, expected_type| {
            let _ = unification::subtype(state, context, lambda_type, expected_type)?;
            Ok(lambda_type)
        },
    )?;

    check_function_application_core(
        state,
        context,
        map_applied,
        expression_type,
        |state, context, expression_type, expected_type| {
            let _ = unification::subtype(state, context, expression_type, expected_type)?;
            Ok(expression_type)
        },
    )
}

fn infer_ado_apply<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    apply_type: TypeId,
    accumulated_type: TypeId,
    expression: Option<lowering::ExpressionId>,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let Some(expression) = expression else {
        return Ok(context.prim.unknown);
    };

    let expression_type = infer_expression(state, context, expression)?;

    let apply_applied = check_function_application_core(
        state,
        context,
        apply_type,
        accumulated_type,
        |state, context, accumulated_type, expected_type| {
            let _ = unification::subtype(state, context, accumulated_type, expected_type)?;
            Ok(accumulated_type)
        },
    )?;

    check_function_application_core(
        state,
        context,
        apply_applied,
        expression_type,
        |state, context, expression_type, expected_type| {
            let _ = unification::subtype(state, context, expression_type, expected_type)?;
            Ok(expression_type)
        },
    )
}

fn infer_do_bind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    bind_type: TypeId,
    accumulated_type: TypeId,
    expression: Option<lowering::ExpressionId>,
    binder_type: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let Some(expression) = expression else {
        return Ok(context.prim.unknown);
    };

    let expression_type = infer_expression(state, context, expression)?;
    let lambda_type = state.make_function(&[binder_type], accumulated_type);

    let bind_applied = check_function_application_core(
        state,
        context,
        bind_type,
        expression_type,
        |state, context, expression_type, expected_type| {
            let _ = unification::subtype(state, context, expression_type, expected_type)?;
            Ok(expression_type)
        },
    )?;

    check_function_application_core(
        state,
        context,
        bind_applied,
        lambda_type,
        |state, context, lambda_type, expected_type| {
            let _ = unification::subtype(state, context, lambda_type, expected_type)?;
            Ok(lambda_type)
        },
    )
}

fn infer_do_discard<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    discard_type: TypeId,
    accumulated_type: TypeId,
    expression: Option<lowering::ExpressionId>,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let Some(expression) = expression else {
        return Ok(context.prim.unknown);
    };

    let expression_type = infer_expression(state, context, expression)?;

    let discard_applied = check_function_application_core(
        state,
        context,
        discard_type,
        expression_type,
        |state, context, expression_type, expected_type| {
            let _ = unification::subtype(state, context, expression_type, expected_type)?;
            Ok(expression_type)
        },
    )?;

    let result_type = check_function_application_core(
        state,
        context,
        discard_applied,
        (),
        |_, _, _, continuation_type| Ok(continuation_type),
    )?;

    let _ = unification::subtype(state, context, accumulated_type, result_type)?;

    Ok(accumulated_type)
}

pub fn lookup_file_term<Q>(
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
        Type::Forall(ref binder, inner) => {
            let binder_level = binder.level;
            let binder_kind = binder.kind;

            let (argument_type, _) =
                kind::check_surface_kind(state, context, argument, binder_kind)?;
            Ok(substitute::substitute_bound(state, binder_level, argument_type, inner))
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
    F: FnOnce(&mut CheckState, &CheckContext<Q>, A, TypeId) -> QueryResult<TypeId>,
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

        Type::Forall(ref binder, inner) => {
            let binder_level = binder.level;
            let binder_kind = binder.kind;

            let unification = state.fresh_unification_kinded(binder_kind);
            let function_t = substitute::substitute_bound(state, binder_level, unification, inner);
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
            if let Some(signature_id) = name.signature {
                let signature = inspect::inspect_signature(state, context, signature_id)?;
                let name_type = signature.restore(state);
                state.bind_let(*id, name_type);
                check_equations(state, context, signature_id, signature, &name.equations)?;
            } else {
                let name_type = state.fresh_unification_type(context);
                state.bind_let(*id, name_type);
                infer_equations_core(state, context, name_type, &name.equations)?;
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

fn instantiate_type<Q>(
    state: &mut CheckState,
    _context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let mut current_id = id;

    while let normalized_id = state.normalize_type(current_id)
        && let Type::Forall(ref binder, inner_id) = state.storage[normalized_id]
    {
        let binder_level = binder.level;
        let binder_kind = binder.kind;

        let unification = state.fresh_unification_kinded(binder_kind);
        current_id = substitute::substitute_bound(state, binder_level, unification, inner_id);
    }

    Ok(current_id)
}
