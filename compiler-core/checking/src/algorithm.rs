pub mod inspect;
pub mod items;
pub mod kind;
pub mod quantify;
pub mod state;
pub mod substitute;
pub mod transfer;
pub mod unification;

use building_types::QueryResult;
use files::FileId;
use lowering::Scc;

use crate::core::{Type, TypeId};
use crate::{CheckedModule, ExternalQueries};

pub(crate) fn check_source(
    queries: &impl ExternalQueries,
    file_id: FileId,
) -> QueryResult<CheckedModule> {
    let mut state = state::CheckState::default();
    let context = state::CheckContext::new(queries, &mut state, file_id)?;

    for scc in &context.lowered.type_scc {
        match scc {
            Scc::Base(id) => {
                items::check_type_item(&mut state, &context, *id)?;
                state.commit_binding_group(&context);
            }
            Scc::Recursive(id) => {
                state.type_binding_group(&context, [*id]);
                items::check_type_item(&mut state, &context, *id)?;
                state.commit_binding_group(&context);
            }
            Scc::Mutual(mutual) => {
                state.type_binding_group(&context, mutual);
                for id in mutual {
                    items::check_type_item(&mut state, &context, *id)?;
                }
                state.commit_binding_group(&context);
            }
        }
    }

    Ok(state.checked)
}

pub(crate) fn check_prim(
    queries: &impl ExternalQueries,
    file_id: FileId,
) -> QueryResult<CheckedModule> {
    let mut checked_module = CheckedModule::default();
    let resolved = queries.resolved(file_id)?;

    let lookup_type = |name: &str| {
        let prim_type = resolved.exports.lookup_type(name);
        prim_type.unwrap_or_else(|| unreachable!("invariant violated: {name} not in Prim"))
    };

    let type_core = {
        let (file_id, item_id) = lookup_type("Type");
        queries.intern_type(Type::Constructor(file_id, item_id))
    };

    let row_core = {
        let (file_id, item_id) = lookup_type("Row");
        queries.intern_type(Type::Constructor(file_id, item_id))
    };

    let constraint_core = {
        let (file_id, item_id) = lookup_type("Constraint");
        queries.intern_type(Type::Constructor(file_id, item_id))
    };

    let type_to_type_core = queries.intern_type(Type::Function(type_core, type_core));
    let function_core = queries.intern_type(Type::Function(type_core, type_to_type_core));

    let row_type_core = queries.intern_type(Type::Application(row_core, type_core));
    let record_core = queries.intern_type(Type::Function(row_type_core, type_core));

    let mut insert_type = |name: &str, id: TypeId| {
        let (_, item_id) = lookup_type(name);
        checked_module.types.insert(item_id, id)
    };

    insert_type("Type", type_core);
    insert_type("Function", function_core);
    insert_type("Array", type_to_type_core);
    insert_type("Record", record_core);
    insert_type("Number", type_core);
    insert_type("Int", type_core);
    insert_type("String", type_core);
    insert_type("Char", type_core);
    insert_type("Boolean", type_core);
    insert_type("Partial", constraint_core);
    insert_type("Constraint", type_core);
    insert_type("Symbol", type_core);
    insert_type("Row", type_to_type_core);

    Ok(checked_module)
}
