use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::substitute::SubstituteName;
use crate::core::{Type, TypeId, normalise, toolkit};
use crate::state::CheckState;

use super::tools;

pub(super) fn generate_field_constraints<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    data_file: FileId,
    data_id: TypeItemId,
    derived_type: TypeId,
    class: (FileId, TypeItemId),
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let class1 = if context.known_types.eq == Some(class) {
        context.known_types.eq1
    } else if context.known_types.ord == Some(class) {
        context.known_types.ord1
    } else {
        None
    };

    let (_, arguments) = toolkit::extract_type_application(state, context, derived_type)?;

    for constructor_id in tools::lookup_data_constructors(context, data_file, data_id)? {
        let constructor_t = toolkit::lookup_file_term(state, context, data_file, constructor_id)?;
        let field_types = instantiate_constructor_fields(state, context, constructor_t, &arguments)?;
        for field_type in field_types {
            generate_constraint(state, context, field_type, class, class1)?;
        }
    }

    Ok(())
}

fn instantiate_constructor_fields<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    constructor_t: TypeId,
    arguments: &[TypeId],
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    let mut current = constructor_t;
    let mut arguments = arguments.iter().copied();

    loop {
        current = normalise::normalise(state, context, current)?;
        let Type::Forall(binder_id, inner) = context.lookup_type(current) else {
            break;
        };

        let binder = context.lookup_forall_binder(binder_id);
        let replacement = arguments.next().unwrap_or_else(|| {
            state.fresh_rigid(context.queries, binder.kind)
        });
        current = SubstituteName::one(state, context, binder.name, replacement, inner)?;
    }

    let toolkit::InspectFunction { arguments, .. } = toolkit::inspect_function(state, context, current)?;
    Ok(arguments)
}

fn generate_constraint<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
    class: (FileId, TypeItemId),
    class1: Option<(FileId, TypeItemId)>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let type_id = normalise::normalise(state, context, type_id)?;

    match context.lookup_type(type_id) {
        Type::Application(function, argument) => {
            let function = normalise::normalise(state, context, function)?;
            if function == context.prim.record {
                generate_constraint(state, context, argument, class, class1)?;
            } else if is_type_to_type_variable(state, context, function)? {
                if let Some(class1) = class1 {
                    tools::emit_constraint(context, state, class1, function);
                }
                tools::emit_constraint(context, state, class, argument);
            } else {
                tools::emit_constraint(context, state, class, type_id);
            }
        }
        Type::Row(row_id) => {
            let row = context.lookup_row_type(row_id);
            for field in row.fields.iter() {
                generate_constraint(state, context, field.id, class, class1)?;
            }
            if let Some(tail) = row.tail {
                generate_constraint(state, context, tail, class, class1)?;
            }
        }
        _ => tools::emit_constraint(context, state, class, type_id),
    }

    Ok(())
}

fn is_type_to_type_variable<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let type_id = normalise::normalise(state, context, type_id)?;
    let kind = match context.lookup_type(type_id) {
        Type::Rigid(_, _, kind) => kind,
        Type::Unification(unification_id) => state.unifications.get(unification_id).kind,
        _ => return Ok(false),
    };

    Ok(normalise::normalise(state, context, kind)? == context.prim.type_to_type)
}
