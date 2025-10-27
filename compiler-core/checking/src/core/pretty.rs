use std::fmt::Write;

use itertools::Itertools;

use crate::ExternalQueries;
use crate::check::{CheckContext, CheckState};
use crate::core::{ForallBinder, Type, TypeId, Variable};

pub fn print_local<Q>(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> String
where
    Q: ExternalQueries,
{
    let queries = context.queries;
    let mut source = TraversalSource::Local { state, queries };
    traverse(&mut source, id)
}

pub fn print_global<Q>(queries: &Q, id: TypeId) -> String
where
    Q: ExternalQueries,
{
    let mut source = TraversalSource::Global { queries };
    traverse(&mut source, id)
}

enum TraversalSource<'a, Q: ExternalQueries> {
    Local { state: &'a mut CheckState, queries: &'a Q },
    Global { queries: &'a Q },
}

impl<'a, Q: ExternalQueries> TraversalSource<'a, Q> {
    fn lookup(&mut self, id: TypeId) -> Type {
        match self {
            TraversalSource::Local { state, .. } => {
                let id = state.normalize_type(id);
                state.storage[id].clone()
            }
            TraversalSource::Global { queries } => queries.lookup_type(id),
        }
    }

    fn queries(&self) -> &Q {
        match self {
            TraversalSource::Local { queries, .. } => queries,
            TraversalSource::Global { queries } => queries,
        }
    }
}

fn traverse<'a, Q: ExternalQueries>(source: &mut TraversalSource<'a, Q>, id: TypeId) -> String {
    match source.lookup(id) {
        Type::Application(mut function, argument) => {
            let mut arguments = vec![argument];

            while let Type::Application(inner_function, argument) = source.lookup(function) {
                function = inner_function;
                arguments.push(argument);
            }

            let function = traverse(source, function);
            let arguments =
                arguments.iter().rev().map(|argument| traverse(source, *argument)).join(" ");

            format!("({function} {arguments})")
        }

        Type::Constructor(file_id, type_id) => {
            let indexed = source.queries().indexed(file_id).unwrap();
            let name = indexed.items[type_id].name.as_ref();
            name.map(|name| name.to_string()).unwrap_or_else(|| "<InvalidName>".to_string())
        }

        Type::Forall(ref binder, mut inner) => {
            let binder = binder.clone();
            let mut binders = vec![binder];

            while let Type::Forall(ref binder, inner_inner) = source.lookup(inner) {
                let binder = binder.clone();
                binders.push(binder);
                inner = inner_inner;
            }

            let mut buffer = String::default();

            write!(&mut buffer, "forall").unwrap();
            for ForallBinder { name, kind, .. } in binders {
                let kind = traverse(source, kind);
                write!(&mut buffer, " ({name} :: {kind})").unwrap();
            }

            let inner = traverse(source, inner);
            write!(&mut buffer, " {inner}").unwrap();

            buffer
        }

        Type::Function(argument, mut result) => {
            let mut arguments = vec![argument];

            while let Type::Function(argument, inner_result) = source.lookup(result) {
                result = inner_result;
                arguments.push(argument);
            }

            let result = traverse(source, result);
            let arguments =
                arguments.iter().rev().map(|argument| traverse(source, *argument)).join(" -> ");

            format!("({arguments} -> {result})")
        }

        Type::KindApplication(mut function, argument) => {
            let mut arguments = vec![argument];

            while let Type::KindApplication(inner_function, argument) = source.lookup(function) {
                function = inner_function;
                arguments.push(argument);
            }

            let function = traverse(source, function);
            let arguments = arguments
                .iter()
                .rev()
                .map(|argument| format!("@{}", traverse(source, *argument)))
                .join(" ");

            format!("({function} {arguments})")
        }

        Type::Unification(unification_id) => match source {
            TraversalSource::Local { state, .. } => {
                let unification = state.unification.get(unification_id);
                format!("?{unification_id}[{}]", unification.domain)
            }
            TraversalSource::Global { .. } => {
                format!("?{unification_id}[<Global>]")
            }
        },

        Type::Variable(ref variable) => match variable {
            Variable::Implicit(level) => format!("{level}"),
            Variable::Skolem(level) => format!("~{level}"),
            Variable::Bound(index) => format!("{index}"),
            Variable::Free(name) => format!("{name}"),
        },

        Type::Unknown => "???".to_string(),
    }
}
