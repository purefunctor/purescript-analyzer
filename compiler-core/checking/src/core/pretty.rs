use std::fmt::Write;

use itertools::Itertools;

use crate::{
    ExternalQueries,
    check::{CheckContext, CheckState},
    core::{ForallBinder, Type, TypeId, Variable},
};

pub fn print_local<Q>(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> String
where
    Q: ExternalQueries,
{
    traverse::<Q, Local>(state, context, id)
}

pub fn print_global<Q>(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> String
where
    Q: ExternalQueries,
{
    traverse::<Q, Global>(state, context, id)
}

struct Local;
struct Global;

trait TraversalExt<Q>
where
    Q: ExternalQueries,
{
    fn lookup(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> Type;
}

impl<Q> TraversalExt<Q> for Local
where
    Q: ExternalQueries,
{
    fn lookup(state: &mut CheckState, _context: &CheckContext<Q>, id: TypeId) -> Type {
        let id = state.normalize_type(id);
        state.storage[id].clone()
    }
}

impl<Q> TraversalExt<Q> for Global
where
    Q: ExternalQueries,
{
    fn lookup(_state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> Type {
        context.queries.lookup_type(id)
    }
}

fn traverse<Q, E>(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> String
where
    Q: ExternalQueries,
    E: TraversalExt<Q>,
{
    match E::lookup(state, context, id) {
        Type::Application(mut function, argument) => {
            let mut arguments = vec![argument];

            while let Type::Application(inner_function, argument) =
                E::lookup(state, context, function)
            {
                function = inner_function;
                arguments.push(argument);
            }

            let function = traverse::<Q, E>(state, context, function);
            let arguments = arguments
                .iter()
                .rev()
                .map(|argument| traverse::<Q, E>(state, context, *argument))
                .join(" ");

            format!("({function} {arguments})")
        }

        Type::Constructor(file_id, type_id) => {
            let indexed = context.queries.indexed(file_id).unwrap();
            let name = indexed.items[type_id].name.as_ref();
            name.map(|name| name.to_string()).unwrap_or_else(|| "<InvalidName>".to_string())
        }

        Type::Forall(ref binder, mut inner) => {
            let binder = binder.clone();
            let mut binders = vec![binder];

            while let Type::Forall(ref binder, inner_inner) = E::lookup(state, context, inner) {
                let binder = binder.clone();
                binders.push(binder);
                inner = inner_inner;
            }

            let mut buffer = String::default();

            write!(&mut buffer, "forall").unwrap();
            for ForallBinder { name, kind, .. } in binders {
                let kind = traverse::<Q, E>(state, context, kind);
                write!(&mut buffer, " ({name} :: {kind})").unwrap();
            }

            let inner = traverse::<Q, E>(state, context, inner);
            write!(&mut buffer, " {inner}").unwrap();

            buffer
        }

        Type::Function(argument, mut result) => {
            let mut arguments = vec![argument];

            while let Type::Function(argument, inner_result) = E::lookup(state, context, result) {
                result = inner_result;
                arguments.push(argument);
            }

            let result = traverse::<Q, E>(state, context, result);
            let arguments = arguments
                .iter()
                .rev()
                .map(|argument| traverse::<Q, E>(state, context, *argument))
                .join(" -> ");

            format!("({arguments} -> {result})")
        }

        Type::KindApplication(mut function, argument) => {
            let mut arguments = vec![argument];

            while let Type::KindApplication(inner_function, argument) =
                E::lookup(state, context, function)
            {
                function = inner_function;
                arguments.push(argument);
            }

            let function = traverse::<Q, E>(state, context, function);
            let arguments = arguments
                .iter()
                .rev()
                .map(|argument| format!("@{}", traverse::<Q, E>(state, context, *argument)))
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
