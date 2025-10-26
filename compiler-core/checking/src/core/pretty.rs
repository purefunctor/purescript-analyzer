use std::fmt::Write;

use itertools::Itertools;

use crate::{
    ExternalQueries,
    check::{CheckContext, CheckState},
    core::{ForallBinder, Type, TypeId, Variable},
};

pub fn print<Q>(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> String
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

            let function = print(state, context, function);
            let arguments =
                arguments.iter().rev().map(|argument| print(state, context, *argument)).join(" ");

            format!("({function} {arguments})")
        }

        Type::Constructor(file_id, type_id) => {
            let indexed = context.queries.indexed(file_id).unwrap();
            let name = indexed.items[type_id].name.as_ref();
            name.map(|name| name.to_string()).unwrap_or_else(|| "<InvalidName>".to_string())
        }

        Type::Forall(ref binder, inner) => {
            let mut binders = vec![binder.clone()];
            let mut current = inner;

            while let Type::Forall(ref binder, inner) = state.storage[current] {
                binders.push(binder.clone());
                current = inner;
            }

            let mut buffer = String::default();

            write!(&mut buffer, "forall").unwrap();
            for ForallBinder { name, kind, .. } in binders {
                let kind = print(state, context, kind);
                write!(&mut buffer, " ({name} :: {kind})").unwrap();
            }

            let current = print(state, context, current);
            write!(&mut buffer, " {current}").unwrap();

            buffer
        }

        Type::Function(argument, mut result) => {
            let mut arguments = vec![argument];

            while let Type::Function(argument, inner_result) = state.storage[result] {
                result = inner_result;
                arguments.push(argument);
            }

            let result = print(state, context, result);
            let arguments = arguments
                .iter()
                .rev()
                .map(|argument| print(state, context, *argument))
                .join(" -> ");

            format!("({arguments} -> {result})")
        }

        Type::KindApplication(mut function, argument) => {
            let mut arguments = vec![argument];

            while let Type::KindApplication(inner_function, argument) = state.storage[function] {
                function = inner_function;
                arguments.push(argument);
            }

            let function = print(state, context, function);
            let arguments = arguments
                .iter()
                .rev()
                .map(|argument| format!("@{}", print(state, context, *argument)))
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
