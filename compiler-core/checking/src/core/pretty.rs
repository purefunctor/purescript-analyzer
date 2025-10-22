use itertools::Itertools;

use crate::{
    ExternalQueries,
    check::{CheckContext, CheckState},
    core::{Type, TypeId, Variable},
};

pub fn print<Q>(context: &CheckContext<Q>, state: &mut CheckState, id: TypeId) -> String
where
    Q: ExternalQueries,
{
    let id = state.normalize_type(id);
    match state.storage[id] {
        Type::Application(mut function, argument) => {
            let mut arguments = vec![argument];

            while let Type::Application(inner_function, argument) = state.storage[function] {
                function = inner_function;
                arguments.push(argument);
            }

            let function = print(context, state, function);
            let arguments =
                arguments.iter().rev().map(|argument| print(context, state, *argument)).join(" ");

            format!("({function} {arguments})")
        }

        Type::Constructor(file_id, type_id) => {
            let indexed = context.queries.indexed(file_id).unwrap();
            let name = indexed.items[type_id].name.as_ref();
            name.map(|name| name.to_string()).unwrap_or_else(|| "<InvalidName>".to_string())
        }

        Type::Forall(ref binder, inner) => {
            let level = binder.level;
            let kind = print(context, state, binder.kind);
            let inner = print(context, state, inner);
            format!("(forall ({level} :: {kind}) {inner})")
        }

        Type::Function(argument, mut result) => {
            let mut arguments = vec![argument];

            while let Type::Function(argument, inner_result) = state.storage[result] {
                result = inner_result;
                arguments.push(argument);
            }

            let result = print(context, state, result);
            let arguments = arguments
                .iter()
                .rev()
                .map(|argument| print(context, state, *argument))
                .join(" -> ");

            format!("({arguments} -> {result})")
        }

        Type::KindApplication(mut function, argument) => {
            let mut arguments = vec![argument];

            while let Type::KindApplication(inner_function, argument) = state.storage[function] {
                function = inner_function;
                arguments.push(argument);
            }

            let function = print(context, state, function);
            let arguments = arguments
                .iter()
                .rev()
                .map(|argument| format!("@{}", print(context, state, *argument)))
                .join(" ");

            format!("({function} {arguments})")
        }

        Type::Unification(unification_id) => {
            let unification = state.unification.get(unification_id);
            format!("?{unification_id}[{}]", unification.domain)
        }

        Type::Variable(ref variable) => match variable {
            Variable::Implicit(level) => format!("{level}"),
            Variable::Skolem(level) => format!("~{level}"),
            Variable::Bound(index) => format!("{index}"),
            Variable::Free(name) => format!("{name}"),
        },

        Type::Unknown => "???".to_string(),
    }
}
