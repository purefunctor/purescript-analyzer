use itertools::Itertools;

use crate::{
    ExternalQueries,
    check::CheckState,
    core::{Type, TypeId, TypeStorage, Variable},
};

pub fn print<S>(queries: &impl ExternalQueries, state: &CheckState<S>, id: TypeId) -> String
where
    S: TypeStorage,
{
    let kind = state.storage.index(id);
    match kind {
        Type::Application(function, argument) => {
            let function = print(queries, state, *function);
            let argument = print(queries, state, *argument);
            format!("({function} {argument})")
        }
        Type::Constructor(file_id, type_id) => {
            let indexed = queries.indexed(*file_id).unwrap();
            let name = indexed.items[*type_id].name.as_ref();
            name.map(|name| name.to_string()).unwrap_or_else(|| "<InvalidName>".to_string())
        }
        Type::Forall(binder, inner) => {
            let level = binder.level;
            let kind = print(queries, state, binder.kind);
            let inner = print(queries, state, *inner);
            format!("(forall ({level} :: {kind}) {inner})")
        }
        Type::Function(argument, result) => {
            let argument = print(queries, state, *argument);
            let result = print(queries, state, *result);
            format!("({argument} -> {result})")
        }
        Type::KindApplication(function, argument) => {
            let function = print(queries, state, *function);
            let argument = print(queries, state, *argument);
            format!("({function} @{argument})")
        }
        Type::Lambda(body) => {
            let body = print(queries, state, *body);
            format!("(Λ. {body})")
        }
        Type::Pruning(unification, variables) => {
            let variables = variables.iter().join(", ");
            format!("?{unification}[{variables}]")
        }
        Type::Unification(unique, spine) => {
            if spine.is_empty() {
                format!("?{unique}")
            } else {
                let spine = spine.iter().map(|id| print(queries, state, *id)).join(", ");
                format!("?{unique}[{spine}]")
            }
        }
        Type::Variable(variable) => match variable {
            Variable::Implicit(level) => format!("{level}"),
            Variable::Skolem(level) => format!("~{level}"),
            Variable::Bound(index) => format!("{index}"),
            Variable::Free(name) => format!("{name}"),
        },
        Type::Unknown => "???".to_string(),
    }
}
