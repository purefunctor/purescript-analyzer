use building_types::QueryResult;
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{RowField, RowType, Type, TypeId, normalise, toolkit, unification};
use crate::state::CheckState;

pub fn infer_array<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    array: &[lowering::ExpressionId],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let inferred_type = state.fresh_unification(context.queries, context.prim.t);

    for expression in array.iter() {
        let element_type = super::infer_expression(state, context, *expression)?;
        unification::subtype(state, context, element_type, inferred_type)?;
    }

    let array_type = context.intern_application(context.prim.array, inferred_type);

    Ok(array_type)
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
    let normalised = normalise::normalise(state, context, expected)?;
    if let Type::Application(constructor, element_type) = context.lookup_type(normalised) {
        let constructor = normalise::normalise(state, context, constructor)?;
        if constructor == context.prim.array {
            for expression in array.iter() {
                super::check_expression(state, context, *expression, element_type)?;
            }
            return Ok(expected);
        }
    }

    let inferred = infer_array(state, context, array)?;
    unification::subtype(state, context, inferred, expected)?;
    Ok(inferred)
}

pub fn infer_record<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    record: &[lowering::ExpressionRecordItem],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let mut fields = vec![];

    for field in record.iter() {
        match field {
            lowering::ExpressionRecordItem::RecordField { name, value } => {
                let Some(name) = name else { continue };
                let Some(value) = value else { continue };

                let label = SmolStr::clone(name);
                let id = super::infer_expression(state, context, *value)?;
                let id = toolkit::instantiate_unifications(state, context, id)?;
                let id = toolkit::collect_wanteds(state, context, id)?;

                fields.push(RowField { label, id });
            }
            lowering::ExpressionRecordItem::RecordPun { name, resolution } => {
                let Some(name) = name else { continue };
                let Some(resolution) = resolution else { continue };

                let label = SmolStr::clone(name);
                let id = toolkit::lookup_term_variable(state, context, *resolution)?;
                let id = toolkit::instantiate_unifications(state, context, id)?;
                let id = toolkit::collect_wanteds(state, context, id)?;

                fields.push(RowField { label, id });
            }
        }
    }

    let row_type = RowType::new(fields, None);
    let row_type_id = context.intern_row_type(row_type);
    let row_type = context.intern_row(row_type_id);
    let record_type = context.intern_application(context.prim.record, row_type);

    Ok(record_type)
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
    let normalised = normalise::normalise(state, context, expected)?;
    if let Type::Application(constructor, row_type) = context.lookup_type(normalised) {
        let constructor = normalise::normalise(state, context, constructor)?;
        if constructor == context.prim.record {
            let row_type = normalise::normalise(state, context, row_type)?;
            if let Type::Row(row_id) = context.lookup_type(row_type) {
                let expected_fields = context.lookup_row_type(row_id);

                let mut fields = vec![];

                for field in record.iter() {
                    match field {
                        lowering::ExpressionRecordItem::RecordField { name, value } => {
                            let Some(name) = name else { continue };
                            let Some(value) = value else { continue };

                            let label = SmolStr::clone(name);

                            let expected_field_type = expected_fields
                                .fields
                                .iter()
                                .find(|f| f.label == label)
                                .map(|f| f.id);

                            let id = if let Some(expected_type) = expected_field_type {
                                super::check_expression(state, context, *value, expected_type)?
                            } else {
                                let id = super::infer_expression(state, context, *value)?;
                                let id = toolkit::instantiate_unifications(state, context, id)?;
                                toolkit::collect_wanteds(state, context, id)?
                            };

                            fields.push(RowField { label, id });
                        }
                        lowering::ExpressionRecordItem::RecordPun { name, resolution } => {
                            let Some(name) = name else { continue };
                            let Some(resolution) = resolution else { continue };

                            let label = SmolStr::clone(name);

                            let expected_field_type = expected_fields
                                .fields
                                .iter()
                                .find(|f| f.label == label)
                                .map(|f| f.id);

                            let id = toolkit::lookup_term_variable(state, context, *resolution)?;

                            let id = if let Some(expected_type) = expected_field_type {
                                unification::subtype(state, context, id, expected_type)?;
                                id
                            } else {
                                let id = toolkit::instantiate_unifications(state, context, id)?;
                                toolkit::collect_wanteds(state, context, id)?
                            };

                            fields.push(RowField { label, id });
                        }
                    }
                }

                let row_type = RowType::new(fields, None);
                let row_type_id = context.intern_row_type(row_type);
                let row_type = context.intern_row(row_type_id);
                let record_type = context.intern_application(context.prim.record, row_type);

                unification::subtype(state, context, record_type, expected)?;
                return Ok(record_type);
            }
        }
    }

    let inferred = infer_record(state, context, record)?;
    unification::subtype(state, context, inferred, expected)?;
    Ok(inferred)
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

        let row_type = RowType::new(vec![RowField { label, id: field_type }], Some(tail_type));
        let row_type_id = context.intern_row_type(row_type);
        let row_type = context.intern_row(row_type_id);
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

    let input_row = RowType::new(input_fields, Some(tail));
    let input_row_id = context.intern_row_type(input_row);
    let input_row = context.intern_row(input_row_id);
    let input_record = context.intern_application(context.prim.record, input_row);

    let output_row = RowType::new(output_fields, Some(tail));
    let output_row_id = context.intern_row_type(output_row);
    let output_row = context.intern_row(output_row_id);
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
                    super::infer_expression(state, context, *expression)?
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

                let in_row = RowType::new(in_f, Some(tail));
                let in_row_id = context.intern_row_type(in_row);
                let in_row = context.intern_row(in_row_id);
                let in_id = context.intern_application(context.prim.record, in_row);

                let out_row = RowType::new(out_f, Some(tail));
                let out_row_id = context.intern_row_type(out_row);
                let out_row = context.intern_row(out_row_id);
                let out_id = context.intern_application(context.prim.record, out_row);

                input_fields.push(RowField { label: label.clone(), id: in_id });
                output_fields.push(RowField { label, id: out_id });
            }
        }
    }

    let tail = state.fresh_unification(context.queries, context.prim.row_type);

    Ok((input_fields, output_fields, tail))
}
