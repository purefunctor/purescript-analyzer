use building_types::QueryResult;
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{RowField, Type, TypeId, normalise, toolkit, unification};
use crate::state::CheckState;

fn infer_record_field_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expression: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let id = super::infer_expression(state, context, expression)?;

    let Some(kind) = context.lowered.info.get_expression_kind(expression) else {
        return Ok(id);
    };

    if should_instantiate_record_field(kind) {
        let id = toolkit::instantiate_unifications(state, context, id)?;
        toolkit::collect_wanteds(state, context, id)
    } else {
        Ok(id)
    }
}

fn should_instantiate_record_field(kind: &lowering::ExpressionKind) -> bool {
    if matches!(
        kind,
        lowering::ExpressionKind::Constructor { .. }
            | lowering::ExpressionKind::Variable { .. }
            | lowering::ExpressionKind::OperatorName { .. }
    ) {
        return true;
    }

    if let lowering::ExpressionKind::Application { arguments, .. } = kind
        && let Some(lowering::ExpressionArgument::Type(_)) = arguments.iter().next()
    {
        return true;
    }

    false
}

fn instantiate_variable<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    resolution: lowering::TermVariableResolution,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let id = toolkit::lookup_term_variable(state, context, resolution)?;
    let id = toolkit::instantiate_unifications(state, context, id)?;
    toolkit::collect_wanteds(state, context, id)
}

#[derive(Copy, Clone, Debug)]
enum ArrayMode {
    Infer,
    Check { element: TypeId },
}

#[derive(Copy, Clone)]
enum RecordMode<'a> {
    Infer,
    Check { expected_fields: &'a [RowField] },
}

pub fn infer_array<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    array: &[lowering::ExpressionId],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    array_core(state, context, array, ArrayMode::Infer)
}

pub fn check_array<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    array: &[lowering::ExpressionId],
    expected: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if let Some(element) = expected_array_element(state, context, expected)? {
        array_core(state, context, array, ArrayMode::Check { element })?;
        return Ok(expected);
    }

    let inferred = array_core(state, context, array, ArrayMode::Infer)?;
    unification::subtype(state, context, inferred, expected)?;
    Ok(inferred)
}

fn expected_array_element<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expected: TypeId,
) -> QueryResult<Option<TypeId>>
where
    Q: ExternalQueries,
{
    let expected = normalise::expand(state, context, expected)?;
    let Type::Application(constructor, element) = context.lookup_type(expected) else {
        return Ok(None);
    };

    let constructor = normalise::expand(state, context, constructor)?;
    if constructor == context.prim.array { Ok(Some(element)) } else { Ok(None) }
}

fn array_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    array: &[lowering::ExpressionId],
    mode: ArrayMode,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let element = match mode {
        ArrayMode::Infer => state.fresh_unification(context.queries, context.prim.t),
        ArrayMode::Check { element } => element,
    };

    for expression in array {
        match mode {
            ArrayMode::Infer => {
                let inferred = super::infer_expression(state, context, *expression)?;
                unification::subtype(state, context, inferred, element)?;
            }
            ArrayMode::Check { .. } => {
                super::check_expression(state, context, *expression, element)?;
            }
        }
    }

    Ok(context.intern_application(context.prim.array, element))
}

pub fn infer_record<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    record: &[lowering::ExpressionRecordItem],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    record_core(state, context, record, RecordMode::Infer)
}

pub fn check_record<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    record: &[lowering::ExpressionRecordItem],
    expected: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let normalised = normalise::expand(state, context, expected)?;
    if let Type::Application(constructor, row_type) = context.lookup_type(normalised) {
        let constructor = normalise::expand(state, context, constructor)?;
        if constructor == context.prim.record {
            let row_type = normalise::expand(state, context, row_type)?;
            if let Type::Row(row_id) = context.lookup_type(row_type) {
                let expected_fields = context.lookup_row_type(row_id);
                let record_type = record_core(
                    state,
                    context,
                    record,
                    RecordMode::Check { expected_fields: &expected_fields.fields },
                )?;
                unification::subtype(state, context, record_type, expected)?;
                return Ok(record_type);
            }
        }
    }

    let inferred = infer_record(state, context, record)?;
    unification::subtype(state, context, inferred, expected)?;
    Ok(inferred)
}

fn find_expected_field(expected_fields: &[RowField], label: &SmolStr) -> Option<TypeId> {
    expected_fields.iter().find(|field| field.label == *label).map(|field| field.id)
}

fn expected_record_field(mode: RecordMode<'_>, label: &SmolStr) -> Option<TypeId> {
    match mode {
        RecordMode::Infer => None,
        RecordMode::Check { expected_fields } => find_expected_field(expected_fields, label),
    }
}

fn record_field_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    name: &SmolStr,
    value: lowering::ExpressionId,
    mode: RecordMode<'_>,
) -> QueryResult<RowField>
where
    Q: ExternalQueries,
{
    let label = SmolStr::clone(name);

    let id = if let Some(expected_type) = expected_record_field(mode, &label) {
        super::check_expression(state, context, value, expected_type)?;
        expected_type
    } else {
        infer_record_field_expression(state, context, value)?
    };

    Ok(RowField { label, id })
}

fn record_pun_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    pun: lowering::RecordPunId,
    name: &SmolStr,
    resolution: lowering::TermVariableResolution,
    mode: RecordMode<'_>,
) -> QueryResult<RowField>
where
    Q: ExternalQueries,
{
    let label = SmolStr::clone(name);

    let id = if let Some(expected_type) = expected_record_field(mode, &label) {
        let id = toolkit::lookup_term_variable(state, context, resolution)?;
        unification::subtype(state, context, id, expected_type)?;
        expected_type
    } else {
        instantiate_variable(state, context, resolution)?
    };

    state.checked.nodes.puns.insert(pun, id);

    Ok(RowField { label, id })
}

fn record_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    record: &[lowering::ExpressionRecordItem],
    mode: RecordMode<'_>,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let mut fields = vec![];

    for field in record.iter() {
        let field = match field {
            lowering::ExpressionRecordItem::RecordField { name, value } => {
                let Some(name) = name else { continue };
                let Some(value) = value else { continue };
                record_field_type(state, context, name, *value, mode)?
            }
            lowering::ExpressionRecordItem::RecordPun { id, name, resolution } => {
                let Some(name) = name else { continue };
                let Some(resolution) = resolution else { continue };
                record_pun_type(state, context, *id, name, *resolution, mode)?
            }
        };

        fields.push(field);
    }

    let row_type = context.intern_row(fields, None);
    Ok(context.intern_application(context.prim.record, row_type))
}

pub fn infer_record_access<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    record: lowering::ExpressionId,
    labels: &[SmolStr],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let mut current_type = super::infer_expression(state, context, record)?;

    for label in labels.iter() {
        let label = SmolStr::clone(label);

        let field_type = state.fresh_unification(context.queries, context.prim.t);
        let tail_type = state.fresh_unification(context.queries, context.prim.row_type);

        let row_type = context.intern_row([RowField { label, id: field_type }], Some(tail_type));
        let record_type = context.intern_application(context.prim.record, row_type);

        unification::subtype(state, context, current_type, record_type)?;
        current_type = field_type;
    }

    Ok(current_type)
}

pub fn infer_record_update<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    record: lowering::ExpressionId,
    updates: &[lowering::RecordUpdate],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let (input_fields, output_fields, tail) = infer_record_updates(state, context, updates)?;

    let input_row = context.intern_row(input_fields, Some(tail));
    let input_record = context.intern_application(context.prim.record, input_row);

    let output_row = context.intern_row(output_fields, Some(tail));
    let output_record = context.intern_application(context.prim.record, output_row);

    super::check_expression(state, context, record, input_record)?;

    Ok(output_record)
}

pub fn infer_record_updates<Q>(
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

                let input_id = state.fresh_unification(context.queries, context.prim.t);
                let output_id = if let Some(expression) = expression {
                    infer_record_field_expression(state, context, *expression)?
                } else {
                    context.unknown("missing record update expression")
                };

                input_fields.push(RowField { label: label.clone(), id: input_id });
                output_fields.push(RowField { label, id: output_id });
            }
            lowering::RecordUpdate::Branch { name, updates } => {
                let Some(name) = name else { continue };
                let label = SmolStr::clone(name);

                let (in_f, out_f, tail) = infer_record_updates(state, context, updates)?;

                let in_row = context.intern_row(in_f, Some(tail));
                let in_id = context.intern_application(context.prim.record, in_row);

                let out_row = context.intern_row(out_f, Some(tail));
                let out_id = context.intern_application(context.prim.record, out_row);

                input_fields.push(RowField { label: label.clone(), id: in_id });
                output_fields.push(RowField { label, id: out_id });
            }
        }
    }

    let tail = state.fresh_unification(context.queries, context.prim.row_type);

    Ok((input_fields, output_fields, tail))
}
