use crate::{
    External,
    check::{CheckContext, CheckState},
    core::{Type, TypeId, TypeStorage, Variable},
};

pub fn pretty_print<S>(
    external: &impl External,
    state: &CheckState<S>,
    context: &CheckContext,
    id: TypeId,
) -> String
where
    S: TypeStorage,
{
    let kind = state.storage.index(id);
    match kind {
        Type::Application(function, argument) => {
            let function = pretty_print(external, state, context, *function);
            let argument = pretty_print(external, state, context, *argument);
            format!("({function} {argument})")
        }
        Type::Constructor(file_id, type_id) => {
            let indexed = external.indexed(*file_id).unwrap();
            let name = indexed.items[*type_id].name.as_ref();
            name.map(|name| name.to_string()).unwrap_or_else(|| "<InvalidName>".to_string())
        }
        Type::Forall(binder, inner) => {
            let level = binder.level;
            let kind = pretty_print(external, state, context, binder.kind);
            let inner = pretty_print(external, state, context, *inner);
            format!("(forall ({level} :: {kind}) {inner})")
        }
        Type::Function(argument, result) => {
            let argument = pretty_print(external, state, context, *argument);
            let result = pretty_print(external, state, context, *result);
            format!("({argument} -> {result})")
        }
        Type::Unification(unique) => {
            format!("?{unique}")
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
