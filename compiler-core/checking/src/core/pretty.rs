use std::fmt::Write;

use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::core::debruijn;
use crate::core::{ForallBinder, Type, TypeId, Variable};

pub fn print_local<Q>(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> String
where
    Q: ExternalQueries,
{
    let queries = context.queries;
    let mut source = TraversalSource::Local { state, queries };
    let config = TraversalConfig { use_names: true };
    traverse(&mut source, &config, id)
}

pub fn print_global<Q>(queries: &Q, id: TypeId) -> String
where
    Q: ExternalQueries,
{
    let mut source = TraversalSource::Global { queries };
    let config = TraversalConfig { use_names: true };
    traverse(&mut source, &config, id)
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

struct TraversalConfig {
    use_names: bool,
}

struct TraversalContext<'a> {
    config: &'a TraversalConfig,
    names: FxHashMap<u32, String>,
    depth: debruijn::Size,
}

impl<'a> TraversalContext<'a> {
    fn new(config: &'a TraversalConfig) -> Self {
        Self { config, names: FxHashMap::default(), depth: debruijn::Size(0) }
    }
}

fn traverse<Q: ExternalQueries>(
    source: &mut TraversalSource<'_, Q>,
    config: &TraversalConfig,
    id: TypeId,
) -> String {
    let mut context = TraversalContext::new(config);
    traverse_inner(source, &mut context, id)
}

fn traverse_inner<'a, Q: ExternalQueries>(
    source: &mut TraversalSource<'a, Q>,
    context: &mut TraversalContext,
    id: TypeId,
) -> String {
    match source.lookup(id) {
        Type::Application(mut function, argument) => {
            let mut arguments = vec![argument];

            while let Type::Application(inner_function, argument) = source.lookup(function) {
                function = inner_function;
                arguments.push(argument);
            }

            let function = traverse_inner(source, context, function);
            let arguments = arguments
                .iter()
                .rev()
                .map(|argument| traverse_inner(source, context, *argument))
                .join(" ");

            format!("({function} {arguments})")
        }

        Type::Constructor(file_id, type_id) => {
            let indexed = source.queries().indexed(file_id).unwrap();
            let name = indexed.items[type_id].name.as_ref();
            name.map(|name| name.to_string()).unwrap_or_else(|| "<InvalidName>".to_string())
        }

        Type::Forall(ref binder, mut inner) => {
            let binder = binder.clone();
            let mut binders = vec![binder.clone()];

            while let Type::Forall(ref binder, inner_inner) = source.lookup(inner) {
                binders.push(binder.clone());
                inner = inner_inner;
            }

            let previous_depth = context.depth;
            let mut buffer = String::default();

            write!(&mut buffer, "forall").unwrap();
            for ForallBinder { name, kind, .. } in &binders {
                let kind = traverse_inner(source, context, *kind);
                write!(&mut buffer, " ({name} :: {kind})").unwrap();
                context.names.insert(context.depth.0, name.to_string());
                context.depth = debruijn::Size(context.depth.0 + 1);
            }

            let inner = traverse_inner(source, context, inner);
            context.depth = previous_depth;

            write!(&mut buffer, ". {inner}").unwrap();

            buffer
        }

        Type::Function(argument, mut result) => {
            let mut arguments = vec![argument];

            while let Type::Function(argument, inner_result) = source.lookup(result) {
                result = inner_result;
                arguments.push(argument);
            }

            let result = traverse_inner(source, context, result);
            let arguments = arguments
                .iter()
                .map(|argument| traverse_inner(source, context, *argument))
                .join(" -> ");

            format!("({arguments} -> {result})")
        }

        Type::KindApplication(mut function, argument) => {
            let mut arguments = vec![argument];

            while let Type::KindApplication(inner_function, argument) = source.lookup(function) {
                function = inner_function;
                arguments.push(argument);
            }

            let function = traverse_inner(source, context, function);
            let arguments = arguments
                .iter()
                .rev()
                .map(|argument| format!("@{}", traverse_inner(source, context, *argument)))
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

        Type::Variable(ref variable) => render_variable(variable, context),

        Type::Unknown => "???".to_string(),
    }
}

fn render_variable(variable: &Variable, context: &TraversalContext) -> String {
    match variable {
        Variable::Implicit(level) => format!("{level}"),
        Variable::Skolem(level, _kind) => format!("~{level} :: <kind>"),
        Variable::Bound(index) => {
            if context.config.use_names {
                let Some(level) = index.to_level(context.depth) else {
                    return format!("{index}");
                };
                let name = context.names.get(&level.0).cloned();
                name.unwrap_or_else(|| format!("{index}"))
            } else {
                format!("{index}")
            }
        }
        Variable::Free(name) => format!("{name}"),
    }
}
