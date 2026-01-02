use building_types::QueryResult;
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{kind, operator, term, unification};
use crate::core::{RowField, RowType, Type, TypeId};
use crate::error::ErrorStep;

#[derive(Copy, Clone, Debug)]
enum BinderMode {
    Infer,
    Check { expected_type: TypeId },
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
    state.with_error_step(ErrorStep::CheckingBinder(binder_id), |state| {
        binder_core(state, context, binder_id, BinderMode::Check { expected_type })
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
            check_binder(state, context, *b, t)?;

            if let BinderMode::Check { expected_type } = mode {
                unification::subtype(state, context, t, expected_type)?;
            }

            Ok(t)
        }

        lowering::BinderKind::OperatorChain { .. } => {
            let (_, inferred_type) = operator::infer_operator_chain(state, context, binder_id)?;

            if let BinderMode::Check { expected_type } = mode {
                unification::subtype(state, context, inferred_type, expected_type)?;
            }

            Ok(inferred_type)
        }

        lowering::BinderKind::Integer => {
            let inferred_type = context.prim.int;

            if let BinderMode::Check { expected_type } = mode {
                unification::unify(state, context, inferred_type, expected_type)?;
            }

            Ok(inferred_type)
        }

        lowering::BinderKind::Number => {
            let inferred_type = context.prim.number;

            if let BinderMode::Check { expected_type } = mode {
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
                term::instantiate_type(state, context, constructor_t)?
            } else {
                for &argument in arguments.iter() {
                    constructor_t = term::check_constructor_binder_application(
                        state,
                        context,
                        constructor_t,
                        argument,
                    )?;
                }
                constructor_t
            };

            if let BinderMode::Check { expected_type } = mode {
                unification::subtype(state, context, inferred_type, expected_type)?;
                Ok(expected_type)
            } else {
                Ok(inferred_type)
            }
        }

        lowering::BinderKind::Variable { .. } => {
            let type_id = match mode {
                BinderMode::Infer => state.fresh_unification_type(context),
                BinderMode::Check { expected_type } => expected_type,
            };
            state.term_scope.bind_binder(binder_id, type_id);
            Ok(type_id)
        }

        lowering::BinderKind::Named { binder, .. } => {
            let Some(binder) = binder else { return Ok(unknown) };

            let type_id = match mode {
                BinderMode::Infer => infer_binder(state, context, *binder)?,
                BinderMode::Check { expected_type } => {
                    check_binder(state, context, *binder, expected_type)?
                }
            };
            state.term_scope.bind_binder(binder_id, type_id);

            Ok(type_id)
        }

        lowering::BinderKind::Wildcard => match mode {
            BinderMode::Infer => Ok(state.fresh_unification_type(context)),
            BinderMode::Check { expected_type } => Ok(expected_type),
        },

        lowering::BinderKind::String => {
            let inferred_type = context.prim.string;

            if let BinderMode::Check { expected_type } = mode {
                unification::unify(state, context, inferred_type, expected_type)?;
            }

            Ok(inferred_type)
        }

        lowering::BinderKind::Char => {
            let inferred_type = context.prim.char;

            if let BinderMode::Check { expected_type } = mode {
                unification::unify(state, context, inferred_type, expected_type)?;
            }

            Ok(inferred_type)
        }

        lowering::BinderKind::Boolean { .. } => {
            let inferred_type = context.prim.boolean;

            if let BinderMode::Check { expected_type } = mode {
                unification::unify(state, context, inferred_type, expected_type)?;
            }

            Ok(inferred_type)
        }

        lowering::BinderKind::Array { array } => {
            let element_type = state.fresh_unification_type(context);

            for binder in array.iter() {
                let binder_type = infer_binder(state, context, *binder)?;
                unification::subtype(state, context, binder_type, element_type)?;
            }

            let array_type =
                state.storage.intern(Type::Application(context.prim.array, element_type));

            if let BinderMode::Check { expected_type } = mode {
                unification::subtype(state, context, array_type, expected_type)?;
            }

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
                        state.term_scope.bind_pun(*id, field_type);

                        fields.push(RowField { label, id: field_type });
                    }
                }
            }

            let row_type = RowType::from_unsorted(fields, None);
            let row_type = state.storage.intern(Type::Row(row_type));

            let record_type =
                state.storage.intern(Type::Application(context.prim.record, row_type));

            if let BinderMode::Check { expected_type } = mode {
                unification::subtype(state, context, record_type, expected_type)?;
            }

            Ok(record_type)
        }

        lowering::BinderKind::Parenthesized { parenthesized } => {
            let Some(parenthesized) = parenthesized else { return Ok(unknown) };
            binder_core(state, context, *parenthesized, mode)
        }
    }
}
