use std::sync::Arc;

use building_types::QueryResult;
use itertools::{EitherOrBoth, Itertools};
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::unification::ElaborationMode;
use crate::algorithm::{binder, kind, operator, term, toolkit, unification};
use crate::core::{RowField, RowType, Type, TypeId};
use crate::error::{ErrorKind, ErrorStep};

#[derive(Copy, Clone, Debug)]
enum BinderMode {
    Infer,
    Check { expected_type: TypeId, elaboration: ElaborationMode },
}

pub fn infer_binder<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    binder_id: lowering::BinderId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::InferringBinder(binder_id), |state| {
        binder_core(state, context, binder_id, BinderMode::Infer)
    })
}

pub fn check_binder<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    binder_id: lowering::BinderId,
    expected_type: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let elaboration = ElaborationMode::Yes;
    state.with_error_step(ErrorStep::CheckingBinder(binder_id), |state| {
        binder_core(state, context, binder_id, BinderMode::Check { expected_type, elaboration })
    })
}

pub fn check_argument_binder<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    binder_id: lowering::BinderId,
    expected_type: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let elaboration = ElaborationMode::No;
    state.with_error_step(ErrorStep::CheckingBinder(binder_id), |state| {
        binder_core(state, context, binder_id, BinderMode::Check { expected_type, elaboration })
    })
}

fn binder_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    binder_id: lowering::BinderId,
    mode: BinderMode,
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
            match mode {
                BinderMode::Check { elaboration: ElaborationMode::No, .. } => {
                    check_argument_binder(state, context, *b, t)?;
                }
                _ => {
                    check_binder(state, context, *b, t)?;
                }
            }

            if let BinderMode::Check { expected_type, elaboration } = mode {
                unification::subtype_with_mode(state, context, t, expected_type, elaboration)?;
            }

            Ok(t)
        }

        lowering::BinderKind::OperatorChain { .. } => {
            let (_, inferred_type) = operator::infer_operator_chain(state, context, binder_id)?;

            if let BinderMode::Check { expected_type, elaboration } = mode {
                unification::subtype_with_mode(
                    state,
                    context,
                    inferred_type,
                    expected_type,
                    elaboration,
                )?;
            }

            Ok(inferred_type)
        }

        lowering::BinderKind::Integer { .. } => {
            let inferred_type = context.prim.int;

            if let BinderMode::Check { expected_type, .. } = mode {
                unification::unify(state, context, inferred_type, expected_type)?;
            }

            Ok(inferred_type)
        }

        lowering::BinderKind::Number { .. } => {
            let inferred_type = context.prim.number;

            if let BinderMode::Check { expected_type, .. } = mode {
                unification::unify(state, context, inferred_type, expected_type)?;
            }

            Ok(inferred_type)
        }

        lowering::BinderKind::Constructor { resolution, arguments } => {
            let Some((file_id, term_id)) = resolution else { return Ok(unknown) };

            let mut constructor_t = term::lookup_file_term(state, context, *file_id, *term_id)?;

            // Instantiate nullary constructors to avoid polymorphic types.
            // Non-nullary constructors are instantiated during application.
            let inferred_type = if arguments.is_empty() {
                constructor_t = toolkit::instantiate_forall(state, constructor_t);
                toolkit::collect_constraints(state, constructor_t)
            } else {
                for &argument in arguments.iter() {
                    constructor_t = binder::check_constructor_binder_application(
                        state,
                        context,
                        constructor_t,
                        argument,
                    )?;
                }
                constructor_t
            };

            if let BinderMode::Check { expected_type, elaboration } = mode {
                unification::subtype_with_mode(
                    state,
                    context,
                    inferred_type,
                    expected_type,
                    elaboration,
                )?;
                Ok(expected_type)
            } else {
                Ok(inferred_type)
            }
        }

        lowering::BinderKind::Variable { .. } => {
            let type_id = match mode {
                BinderMode::Infer => state.fresh_unification_type(context),
                BinderMode::Check { expected_type, .. } => expected_type,
            };
            state.term_scope.bind_binder(binder_id, type_id);
            Ok(type_id)
        }

        lowering::BinderKind::Named { binder, .. } => {
            let Some(binder) = binder else { return Ok(unknown) };

            let type_id = match mode {
                BinderMode::Infer => infer_binder(state, context, *binder)?,
                BinderMode::Check { expected_type, elaboration } => match elaboration {
                    ElaborationMode::Yes => check_binder(state, context, *binder, expected_type)?,
                    ElaborationMode::No => {
                        check_argument_binder(state, context, *binder, expected_type)?
                    }
                },
            };
            state.term_scope.bind_binder(binder_id, type_id);

            Ok(type_id)
        }

        lowering::BinderKind::Wildcard => match mode {
            BinderMode::Infer => Ok(state.fresh_unification_type(context)),
            BinderMode::Check { expected_type, .. } => Ok(expected_type),
        },

        lowering::BinderKind::String { .. } => {
            let inferred_type = context.prim.string;

            if let BinderMode::Check { expected_type, .. } = mode {
                unification::unify(state, context, inferred_type, expected_type)?;
            }

            Ok(inferred_type)
        }

        lowering::BinderKind::Char { .. } => {
            let inferred_type = context.prim.char;

            if let BinderMode::Check { expected_type, .. } = mode {
                unification::unify(state, context, inferred_type, expected_type)?;
            }

            Ok(inferred_type)
        }

        lowering::BinderKind::Boolean { .. } => {
            let inferred_type = context.prim.boolean;

            if let BinderMode::Check { expected_type, .. } = mode {
                unification::unify(state, context, inferred_type, expected_type)?;
            }

            Ok(inferred_type)
        }

        lowering::BinderKind::Array { array } => {
            let element_type = state.fresh_unification_type(context);

            for binder in array.iter() {
                let binder_type = infer_binder(state, context, *binder)?;
                unification::subtype_with_mode(
                    state,
                    context,
                    binder_type,
                    element_type,
                    ElaborationMode::No,
                )?;
            }

            let array_type =
                state.storage.intern(Type::Application(context.prim.array, element_type));

            if let BinderMode::Check { expected_type, elaboration } = mode {
                unification::subtype_with_mode(
                    state,
                    context,
                    array_type,
                    expected_type,
                    elaboration,
                )?;
            }

            Ok(array_type)
        }

        lowering::BinderKind::Record { record } => {
            if let BinderMode::Check { expected_type, elaboration } = mode {
                check_record_binder(state, context, binder_id, record, expected_type, elaboration)
            } else {
                infer_record_binder(state, context, binder_id, record)
            }
        }

        lowering::BinderKind::Parenthesized { parenthesized } => {
            let Some(parenthesized) = parenthesized else { return Ok(unknown) };
            binder_core(state, context, *parenthesized, mode)
        }
    }
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
    term::check_function_application_core(state, context, constructor_t, binder_id, check_binder)
}

enum PatternItem {
    Field(lowering::BinderId),
    Pun(lowering::RecordPunId),
}

fn collect_pattern_items(record: &[lowering::BinderRecordItem]) -> Vec<(SmolStr, PatternItem)> {
    let mut items = vec![];
    for field in record {
        match field {
            lowering::BinderRecordItem::RecordField { name, value } => {
                let Some(name) = name else { continue };
                let Some(value) = value else { continue };
                let name = SmolStr::clone(name);
                items.push((name, PatternItem::Field(*value)));
            }
            lowering::BinderRecordItem::RecordPun { id, name } => {
                let Some(name) = name else { continue };
                let name = SmolStr::clone(name);
                items.push((name, PatternItem::Pun(*id)));
            }
        }
    }
    items.sort_by(|a, b| a.0.cmp(&b.0));
    items
}

fn check_pattern_item<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item: &PatternItem,
    expected_type: TypeId,
    elaboration: ElaborationMode,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    match *item {
        PatternItem::Field(binder_id) => match elaboration {
            ElaborationMode::Yes => {
                check_binder(state, context, binder_id, expected_type)?;
            }
            ElaborationMode::No => {
                check_argument_binder(state, context, binder_id, expected_type)?;
            }
        },
        PatternItem::Pun(pun_id) => {
            state.term_scope.bind_pun(pun_id, expected_type);
        }
    }
    Ok(())
}

fn infer_record_binder<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    binder_id: lowering::BinderId,
    record: &[lowering::BinderRecordItem],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
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

                state.term_scope.bind_pun(*id, field_type);
                fields.push(RowField { label, id: field_type });
            }
        }
    }

    let row_tail = state.fresh_unification_kinded(context.prim.row_type);
    let row_type = RowType::from_unsorted(fields, Some(row_tail));
    let row_type = state.storage.intern(Type::Row(row_type));
    let record_type = state.storage.intern(Type::Application(context.prim.record, row_type));

    state.term_scope.bind_binder(binder_id, record_type);
    Ok(record_type)
}

fn extract_expected_row<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expected_type: TypeId,
) -> QueryResult<Option<RowType>>
where
    Q: ExternalQueries,
{
    let expected_type = toolkit::normalise_expand_type(state, context, expected_type)?;
    let &Type::Application(function, argument) = &state.storage[expected_type] else {
        return Ok(None);
    };
    if function != context.prim.record {
        return Ok(None);
    }
    let row = toolkit::normalise_expand_type(state, context, argument)?;
    let Type::Row(row) = &state.storage[row] else {
        return Ok(None);
    };
    Ok(Some(RowType::clone(row)))
}

fn check_record_binder<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    binder_id: lowering::BinderId,
    record: &[lowering::BinderRecordItem],
    expected_type: TypeId,
    elaboration: ElaborationMode,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let pattern_items = collect_pattern_items(record);

    let expected_type = toolkit::normalise_expand_type(state, context, expected_type)?;

    let expected_row = if let &Type::Application(function, _) = &state.storage[expected_type]
        && function == context.prim.record
    {
        extract_expected_row(state, context, expected_type)?
    } else {
        None
    };

    let Some(expected_row) = expected_row else {
        let result = infer_record_binder(state, context, binder_id, record)?;
        unification::unify(state, context, result, expected_type)?;
        return Ok(expected_type);
    };

    let mut extra_fields = vec![];

    let patterns = pattern_items.iter();
    let expected = expected_row.fields.iter();

    for pair in patterns.merge_join_by(expected, |pattern, expected| pattern.0.cmp(&expected.label))
    {
        match pair {
            // If a label exists in both, perform checking
            EitherOrBoth::Both((_, item), expected) => {
                check_pattern_item(state, context, item, expected.id, elaboration)?;
            }
            // If a label only exists in the pattern, track it
            EitherOrBoth::Left((label, item)) => {
                let id = state.fresh_unification_type(context);
                check_pattern_item(state, context, item, id, elaboration)?;

                let label = SmolStr::clone(label);
                extra_fields.push(RowField { label, id });
            }
            // If a label only exists in the type, do nothing
            EitherOrBoth::Right(_) => (),
        }
    }

    if !extra_fields.is_empty() {
        if let Some(tail) = expected_row.tail {
            let row_tail = state.fresh_unification_kinded(context.prim.row_type);

            let row_type = RowType::from_unsorted(extra_fields, Some(row_tail));
            let row_type = state.storage.intern(Type::Row(row_type));

            unification::unify(state, context, tail, row_type)?;
        } else {
            let labels = extra_fields.into_iter().map(|field| field.label);
            state.insert_error(ErrorKind::AdditionalProperty { labels: Arc::from_iter(labels) });
        }
    }

    state.term_scope.bind_binder(binder_id, expected_type);
    Ok(expected_type)
}
