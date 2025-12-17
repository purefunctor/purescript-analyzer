use std::fmt::Write;

use itertools::Itertools;
use lowering::StringKind;
use rustc_hash::FxHashMap;

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::core::{ForallBinder, RowField, RowType, Type, TypeId, Variable, debruijn};

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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Top,
    Constraint,
    Function,
    Application,
    Atom,
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
    traverse_precedence(source, &mut context, Precedence::Top, id)
}

fn parens_if(condition: bool, s: String) -> String {
    if condition { format!("({s})") } else { s }
}

fn lookup_type_name<Q: ExternalQueries>(
    source: &TraversalSource<Q>,
    file_id: files::FileId,
    type_id: indexing::TypeItemId,
) -> Option<String> {
    let indexed = source.queries().indexed(file_id).ok()?;
    indexed.items[type_id].name.as_ref().map(|name| name.to_string())
}

fn traverse_precedence<'a, Q: ExternalQueries>(
    source: &mut TraversalSource<'a, Q>,
    context: &mut TraversalContext,
    precedence: Precedence,
    id: TypeId,
) -> String {
    match source.lookup(id) {
        Type::Application(mut function, argument) => {
            if is_record_constructor(source, function) {
                return match source.lookup(argument) {
                    Type::Row(RowType { fields, tail }) => {
                        let body = traverse_row_body(source, context, &fields, tail);
                        format!("{{ {body} }}")
                    }
                    _ => {
                        let inner = traverse_precedence(source, context, Precedence::Top, argument);
                        format!("{{ | {inner} }}")
                    }
                };
            }

            let mut arguments = vec![argument];

            while let Type::Application(inner_function, argument) = source.lookup(function) {
                function = inner_function;
                arguments.push(argument);
            }

            let function = traverse_precedence(source, context, Precedence::Application, function);
            let arguments = arguments
                .iter()
                .rev()
                .map(|argument| traverse_precedence(source, context, Precedence::Atom, *argument))
                .join(" ");

            parens_if(precedence > Precedence::Application, format!("{function} {arguments}"))
        }

        Type::Constrained(constraint, mut inner) => {
            let mut constraints = vec![constraint];

            while let Type::Constrained(constraint, inner_inner) = source.lookup(inner) {
                constraints.push(constraint);
                inner = inner_inner;
            }

            let inner = traverse_precedence(source, context, Precedence::Constraint, inner);
            let constraints = constraints
                .iter()
                .map(|c| traverse_precedence(source, context, Precedence::Application, *c))
                .join(" => ");

            parens_if(precedence > Precedence::Constraint, format!("{constraints} => {inner}"))
        }

        Type::Constructor(file_id, type_id) => lookup_type_name(source, file_id, type_id)
            .unwrap_or_else(|| "<InvalidName>".to_string()),

        Type::Forall(ref binder, mut inner) => {
            let binder = binder.clone();
            let mut binders = vec![binder];

            while let Type::Forall(ref binder, inner_inner) = source.lookup(inner) {
                let binder = binder.clone();
                binders.push(binder);
                inner = inner_inner;
            }

            let previous_depth = context.depth;
            let mut buffer = String::default();

            write!(&mut buffer, "forall").unwrap();
            for ForallBinder { name, kind, .. } in &binders {
                let kind = traverse_precedence(source, context, Precedence::Top, *kind);
                write!(&mut buffer, " ({name} :: {kind})").unwrap();
                context.names.insert(context.depth.0, name.to_string());
                context.depth = debruijn::Size(context.depth.0 + 1);
            }

            let inner = traverse_precedence(source, context, Precedence::Top, inner);
            context.depth = previous_depth;

            write!(&mut buffer, ". {inner}").unwrap();

            parens_if(precedence > Precedence::Top, buffer)
        }

        Type::Function(argument, mut result) => {
            let mut arguments = vec![argument];

            while let Type::Function(argument, inner_result) = source.lookup(result) {
                result = inner_result;
                arguments.push(argument);
            }

            let result = traverse_precedence(source, context, Precedence::Function, result);
            let arguments = arguments
                .iter()
                .map(|argument| {
                    traverse_precedence(source, context, Precedence::Application, *argument)
                })
                .join(" -> ");

            parens_if(precedence > Precedence::Function, format!("{arguments} -> {result}"))
        }

        Type::Integer(integer) => parens_if(integer.is_negative(), integer.to_string()),

        Type::KindApplication(mut function, argument) => {
            let mut arguments = vec![argument];

            while let Type::KindApplication(inner_function, argument) = source.lookup(function) {
                function = inner_function;
                arguments.push(argument);
            }

            let function = traverse_precedence(source, context, Precedence::Application, function);
            let arguments = arguments
                .iter()
                .rev()
                .map(|argument| {
                    let argument =
                        traverse_precedence(source, context, Precedence::Atom, *argument);
                    format!("@{argument}")
                })
                .join(" ");

            parens_if(precedence > Precedence::Application, format!("{function} {arguments}"))
        }

        Type::Kinded(inner, kind) => {
            let inner = traverse_precedence(source, context, Precedence::Application, inner);
            let kind = traverse_precedence(source, context, Precedence::Top, kind);
            parens_if(precedence > Precedence::Atom, format!("{inner} :: {kind}"))
        }

        Type::Operator(file_id, type_id) => lookup_type_name(source, file_id, type_id)
            .unwrap_or_else(|| "<InvalidName>".to_string()),

        Type::OperatorApplication(file_id, type_id, left, right) => {
            let operator = lookup_type_name(source, file_id, type_id)
                .unwrap_or_else(|| "<InvalidName>".to_string());

            let left = traverse_precedence(source, context, Precedence::Application, left);
            let right = traverse_precedence(source, context, Precedence::Application, right);

            parens_if(precedence > Precedence::Application, format!("{left} {operator} {right}"))
        }

        Type::String(kind, string) => match kind {
            StringKind::String => format!("\"{string}\""),
            StringKind::RawString => format!("\"\"\"{string}\"\"\""),
        },

        Type::SynonymApplication(_, file_id, type_id, ref arguments) => {
            let function = lookup_type_name(source, file_id, type_id)
                .unwrap_or_else(|| "<InvalidName>".to_string());

            let arguments = arguments
                .iter()
                .map(|&argument| traverse_precedence(source, context, Precedence::Atom, argument))
                .join(" ");

            if arguments.is_empty() {
                function
            } else {
                parens_if(precedence > Precedence::Application, format!("{function} {arguments}"))
            }
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

        Type::Row(RowType { fields, tail }) => {
            if fields.is_empty() && tail.is_none() {
                return "()".to_string();
            }

            let body = traverse_row_body(source, context, &fields, tail);
            format!("( {body} )")
        }

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

fn traverse_row_body<Q: ExternalQueries>(
    source: &mut TraversalSource<'_, Q>,
    context: &mut TraversalContext,
    fields: &[RowField],
    tail: Option<TypeId>,
) -> String {
    let fields = fields
        .iter()
        .map(|RowField { label, id }| {
            let label_type = traverse_precedence(source, context, Precedence::Top, *id);
            format!("{label} :: {label_type}")
        })
        .join(", ");

    let tail = tail.map(|tail| traverse_precedence(source, context, Precedence::Top, tail));

    match tail {
        Some(tail) if fields.is_empty() => format!("| {tail}"),
        Some(tail) => format!("{fields} | {tail}"),
        None => fields,
    }
}

fn is_record_constructor<Q: ExternalQueries>(source: &mut TraversalSource<Q>, id: TypeId) -> bool {
    if let Type::Constructor(file_id, type_id) = source.lookup(id)
        && file_id == source.queries().prim_id()
            && let Some(name) = lookup_type_name(source, file_id, type_id) {
                return name == "Record";
            }
    false
}
