//! Implements the pretty printer.

use itertools::Itertools;
use lowering::StringKind;
use pretty::{Arena, DocAllocator, DocBuilder};
use rustc_hash::FxHashMap;

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::core::{ForallBinder, RowField, RowType, Type, TypeId, Variable, debruijn};

type Doc<'a> = DocBuilder<'a, Arena<'a>, ()>;

pub struct PrettyConfig {
    pub width: usize,
}

impl Default for PrettyConfig {
    fn default() -> PrettyConfig {
        PrettyConfig { width: 100 }
    }
}

pub fn print_local<Q>(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> String
where
    Q: ExternalQueries,
{
    print_local_with_config(state, context, id, &PrettyConfig::default())
}

pub fn print_local_with_config<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
    config: &PrettyConfig,
) -> String
where
    Q: ExternalQueries,
{
    let queries = context.queries;

    let arena = Arena::new();
    let mut source = TraversalSource::Local { state, queries };
    let mut context = TraversalContext::new();

    let mut output = String::default();
    let document = traverse_precedence(&arena, &mut source, &mut context, Precedence::Top, id);
    document.render_fmt(config.width, &mut output).unwrap();
    output
}

pub fn print_global<Q>(queries: &Q, id: TypeId) -> String
where
    Q: ExternalQueries,
{
    print_global_with_config(queries, id, &PrettyConfig::default())
}

pub fn print_global_with_config<Q>(queries: &Q, id: TypeId, config: &PrettyConfig) -> String
where
    Q: ExternalQueries,
{
    let arena = Arena::new();
    let mut source = TraversalSource::Global { queries };
    let mut context = TraversalContext::new();

    let mut output = String::default();
    let document = traverse_precedence(&arena, &mut source, &mut context, Precedence::Top, id);
    document.render_fmt(config.width, &mut output).unwrap();
    output
}

pub fn print_signature_local<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    name: &str,
    id: TypeId,
) -> String
where
    Q: ExternalQueries,
{
    print_signature_local_with_config(state, context, name, id, &PrettyConfig::default())
}

pub fn print_signature_local_with_config<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    name: &str,
    id: TypeId,
    config: &PrettyConfig,
) -> String
where
    Q: ExternalQueries,
{
    let queries = context.queries;
    let arena = Arena::new();
    let mut source = TraversalSource::Local { state, queries };
    let mut ctx = TraversalContext::new();
    let type_doc = traverse_precedence(&arena, &mut source, &mut ctx, Precedence::Top, id);

    let type_break = arena.line().append(type_doc).nest(2);
    let doc = arena.text(name).append(arena.text(" ::")).append(type_break).group();

    let mut output = String::new();
    doc.render_fmt(config.width, &mut output).unwrap();
    output
}

pub fn print_signature_global<Q>(queries: &Q, name: &str, id: TypeId) -> String
where
    Q: ExternalQueries,
{
    print_signature_global_with_config(queries, name, id, &PrettyConfig::default())
}

pub fn print_signature_global_with_config<Q>(
    queries: &Q,
    name: &str,
    id: TypeId,
    config: &PrettyConfig,
) -> String
where
    Q: ExternalQueries,
{
    let arena = Arena::new();
    let mut source = TraversalSource::Global { queries };
    let mut ctx = TraversalContext::new();
    let type_doc = traverse_precedence(&arena, &mut source, &mut ctx, Precedence::Top, id);

    let type_break = arena.line().append(type_doc).nest(2);
    let doc = arena.text(name).append(arena.text(" ::")).append(type_break).group();

    let mut output = String::new();
    doc.render_fmt(config.width, &mut output).unwrap();
    output
}

enum TraversalSource<'a, Q>
where
    Q: ExternalQueries,
{
    Local { state: &'a mut CheckState, queries: &'a Q },
    Global { queries: &'a Q },
}

impl<'a, Q> TraversalSource<'a, Q>
where
    Q: ExternalQueries,
{
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Top,
    Constraint,
    Function,
    Application,
    Atom,
}

struct TraversalContext {
    names: FxHashMap<u32, String>,
    depth: debruijn::Size,
}

impl TraversalContext {
    fn new() -> TraversalContext {
        TraversalContext { names: FxHashMap::default(), depth: debruijn::Size(0) }
    }
}

fn parens_if<'a>(arena: &'a Arena<'a>, condition: bool, doc: Doc<'a>) -> Doc<'a> {
    if condition { arena.text("(").append(doc).append(arena.text(")")) } else { doc }
}

fn lookup_type_name<Q>(
    source: &TraversalSource<Q>,
    file_id: files::FileId,
    type_id: indexing::TypeItemId,
) -> Option<String>
where
    Q: ExternalQueries,
{
    let indexed = source.queries().indexed(file_id).ok()?;
    indexed.items[type_id].name.as_ref().map(|name| name.to_string())
}

fn traverse_precedence<'a, Q>(
    arena: &'a Arena<'a>,
    source: &mut TraversalSource<'_, Q>,
    context: &mut TraversalContext,
    precedence: Precedence,
    id: TypeId,
) -> Doc<'a>
where
    Q: ExternalQueries,
{
    match source.lookup(id) {
        Type::Application(mut function, argument) => {
            if is_record_constructor(source, function) {
                return match source.lookup(argument) {
                    Type::Row(RowType { fields, tail }) => {
                        format_record(arena, source, context, &fields, tail)
                    }
                    _ => {
                        let inner =
                            traverse_precedence(arena, source, context, Precedence::Top, argument);
                        arena.text("{| ").append(inner).append(arena.text(" }"))
                    }
                };
            }

            let mut arguments = vec![argument];

            while let Type::Application(inner_function, argument) = source.lookup(function) {
                function = inner_function;
                arguments.push(argument);
            }

            let func_doc =
                traverse_precedence(arena, source, context, Precedence::Application, function);

            let arg_docs = arguments
                .iter()
                .rev()
                .map(|&arg| traverse_precedence(arena, source, context, Precedence::Atom, arg))
                .collect_vec();

            let args_doc = arg_docs
                .into_iter()
                .fold(arena.nil(), |acc, arg| acc.append(arena.line()).append(arg));

            let doc = func_doc.append(args_doc.nest(2)).group();
            parens_if(arena, precedence > Precedence::Application, doc)
        }

        Type::Constrained(constraint, mut inner) => {
            let mut constraints = vec![constraint];

            while let Type::Constrained(constraint, inner_inner) = source.lookup(inner) {
                constraints.push(constraint);
                inner = inner_inner;
            }

            let constraint_docs = constraints
                .iter()
                .map(|&c| traverse_precedence(arena, source, context, Precedence::Application, c))
                .collect_vec();

            let inner_doc =
                traverse_precedence(arena, source, context, Precedence::Constraint, inner);

            let trailing_fat_arrow = arena.text(" =>").append(arena.line());
            let constraints_doc = constraint_docs
                .into_iter()
                .fold(arena.nil(), |acc, c| acc.append(c).append(trailing_fat_arrow.clone()));

            let doc = constraints_doc.append(inner_doc).group();
            parens_if(arena, precedence > Precedence::Constraint, doc)
        }

        Type::Constructor(file_id, type_id) => {
            let name = lookup_type_name(source, file_id, type_id)
                .unwrap_or_else(|| "<InvalidName>".to_string());
            arena.text(name)
        }

        Type::Forall(ref binder, mut inner) => {
            let binder = binder.clone();
            let mut binders = vec![binder];

            while let Type::Forall(ref binder, inner_inner) = source.lookup(inner) {
                let binder = binder.clone();
                binders.push(binder);
                inner = inner_inner;
            }

            let previous_depth = context.depth;

            let binder_docs = binders
                .iter()
                .map(|ForallBinder { name, kind, .. }| {
                    let kind_doc =
                        traverse_precedence(arena, source, context, Precedence::Top, *kind);
                    context.names.insert(context.depth.0, name.to_string());
                    context.depth = debruijn::Size(context.depth.0 + 1);

                    // Group each binder so it stays together as an atomic unit
                    arena
                        .text("(")
                        .append(arena.text(name.to_string()))
                        .append(arena.text(" :: "))
                        .append(kind_doc)
                        .append(arena.text(")"))
                        .group()
                })
                .collect_vec();

            // Build binders with fill behavior: each binder independently decides
            // whether it fits on the current line or needs to break.
            // Structure: binder1 + group(line + binder2) + group(line + binder3) + ...
            let mut iter = binder_docs.into_iter();
            let binders_doc = if let Some(first) = iter.next() {
                iter.fold(first, |acc, binder| {
                    acc.append(arena.line().append(binder).nest(2).group())
                })
            } else {
                arena.nil()
            };

            let forall_header = arena.text("forall ").append(binders_doc).append(arena.text("."));

            let inner_doc = traverse_precedence(arena, source, context, Precedence::Top, inner);
            context.depth = previous_depth;

            let body_break = arena.line().append(inner_doc).nest(2);
            let doc = forall_header.append(body_break).group();

            parens_if(arena, precedence > Precedence::Top, doc)
        }

        Type::Function(argument, mut result) => {
            let mut arguments = vec![argument];

            while let Type::Function(argument, inner_result) = source.lookup(result) {
                result = inner_result;
                arguments.push(argument);
            }

            let arg_docs = arguments
                .iter()
                .map(|&arg| {
                    traverse_precedence(arena, source, context, Precedence::Application, arg)
                })
                .collect_vec();

            let result_doc =
                traverse_precedence(arena, source, context, Precedence::Function, result);

            let trailing_arrow = arena.text(" ->").append(arena.line());
            let args_doc = arg_docs
                .into_iter()
                .fold(arena.nil(), |acc, arg| acc.append(arg).append(trailing_arrow.clone()));

            let doc = args_doc.append(result_doc).group();
            parens_if(arena, precedence > Precedence::Function, doc)
        }

        Type::Integer(integer) => {
            let doc = arena.text(integer.to_string());
            parens_if(arena, integer.is_negative(), doc)
        }

        Type::KindApplication(mut function, argument) => {
            let mut arguments = vec![argument];

            while let Type::KindApplication(inner_function, argument) = source.lookup(function) {
                function = inner_function;
                arguments.push(argument);
            }

            let func_doc =
                traverse_precedence(arena, source, context, Precedence::Application, function);

            let arg_docs = arguments
                .iter()
                .rev()
                .map(|&arg| traverse_precedence(arena, source, context, Precedence::Atom, arg))
                .collect_vec();

            let args_doc = arg_docs.into_iter().fold(arena.nil(), |acc, arg| {
                acc.append(arena.line()).append(arena.text("@")).append(arg)
            });

            let doc = func_doc.append(args_doc.nest(2)).group();
            parens_if(arena, precedence > Precedence::Application, doc)
        }

        Type::Kinded(inner, kind) => {
            let inner_doc =
                traverse_precedence(arena, source, context, Precedence::Application, inner);
            let kind_doc = traverse_precedence(arena, source, context, Precedence::Top, kind);
            let doc = inner_doc.append(arena.text(" :: ")).append(kind_doc);
            parens_if(arena, precedence > Precedence::Atom, doc)
        }

        Type::Operator(file_id, type_id) => {
            let name = lookup_type_name(source, file_id, type_id)
                .unwrap_or_else(|| "<InvalidName>".to_string());
            arena.text(name)
        }

        Type::OperatorApplication(file_id, type_id, left, right) => {
            let operator = lookup_type_name(source, file_id, type_id)
                .unwrap_or_else(|| "<InvalidName>".to_string());

            let left_doc =
                traverse_precedence(arena, source, context, Precedence::Application, left);
            let right_doc =
                traverse_precedence(arena, source, context, Precedence::Application, right);

            let doc = left_doc
                .append(arena.text(" "))
                .append(arena.text(operator))
                .append(arena.text(" "))
                .append(right_doc);
            parens_if(arena, precedence > Precedence::Application, doc)
        }

        Type::String(kind, string) => match kind {
            StringKind::String => arena.text(format!("\"{}\"", string)),
            StringKind::RawString => arena.text(format!("\"\"\"{}\"\"\"", string)),
        },

        Type::SynonymApplication(_, file_id, type_id, ref arguments) => {
            let function = lookup_type_name(source, file_id, type_id)
                .unwrap_or_else(|| "<InvalidName>".to_string());

            if arguments.is_empty() {
                return arena.text(function);
            }

            let func_doc = arena.text(function);

            let arg_docs = arguments
                .iter()
                .map(|&arg| traverse_precedence(arena, source, context, Precedence::Atom, arg))
                .collect_vec();

            let args_doc = arg_docs
                .into_iter()
                .fold(arena.nil(), |acc, arg| acc.append(arena.line()).append(arg));

            let doc = func_doc.append(args_doc.nest(2)).group();
            parens_if(arena, precedence > Precedence::Application, doc)
        }

        Type::Unification(unification_id) => match source {
            TraversalSource::Local { state, .. } => {
                let unification = state.unification.get(unification_id);
                arena.text(format!("?{}[{}]", unification_id, unification.depth))
            }
            TraversalSource::Global { .. } => arena.text(format!("?{}[<Global>]", unification_id)),
        },

        Type::Row(RowType { fields, tail }) => {
            if fields.is_empty() && tail.is_none() {
                return arena.text("()");
            }
            format_row(arena, source, context, &fields, tail)
        }

        Type::Variable(ref variable) => render_variable(arena, source, context, variable),

        Type::Unknown => arena.text("???"),
    }
}

fn render_variable<'a, Q>(
    arena: &'a Arena<'a>,
    source: &mut TraversalSource<'_, Q>,
    context: &mut TraversalContext,
    variable: &Variable,
) -> Doc<'a>
where
    Q: ExternalQueries,
{
    match variable {
        Variable::Skolem(level, kind) => {
            let kind_doc = traverse_precedence(arena, source, context, Precedence::Top, *kind);
            arena
                .text("(")
                .append(arena.text(format!("~{}", level)))
                .append(arena.text(" :: "))
                .append(kind_doc)
                .append(arena.text(")"))
        }
        Variable::Bound(level, kind) => {
            let name = context.names.get(&level.0).cloned();
            let name_doc = arena.text(name.unwrap_or_else(|| format!("{}", level)));
            let kind_doc = traverse_precedence(arena, source, context, Precedence::Top, *kind);
            arena
                .text("(")
                .append(name_doc)
                .append(arena.text(" :: "))
                .append(kind_doc)
                .append(arena.text(")"))
        }
        Variable::Free(name) => arena.text(format!("{}", name)),
    }
}

fn format_record<'a, Q>(
    arena: &'a Arena<'a>,
    source: &mut TraversalSource<'_, Q>,
    context: &mut TraversalContext,
    fields: &[RowField],
    tail: Option<TypeId>,
) -> Doc<'a>
where
    Q: ExternalQueries,
{
    if fields.is_empty() && tail.is_none() {
        return arena.text("{}");
    }

    let body = format_row_body(arena, source, context, fields, tail);

    arena.text("{ ").append(body).append(arena.line()).append(arena.text("}")).group()
}

fn format_row<'a, Q>(
    arena: &'a Arena<'a>,
    source: &mut TraversalSource<'_, Q>,
    context: &mut TraversalContext,
    fields: &[RowField],
    tail: Option<TypeId>,
) -> Doc<'a>
where
    Q: ExternalQueries,
{
    let body = format_row_body(arena, source, context, fields, tail);

    arena.text("( ").append(body).append(arena.line()).append(arena.text(")")).group()
}

fn format_row_body<'a, Q>(
    arena: &'a Arena<'a>,
    source: &mut TraversalSource<'_, Q>,
    context: &mut TraversalContext,
    fields: &[RowField],
    tail: Option<TypeId>,
) -> Doc<'a>
where
    Q: ExternalQueries,
{
    if fields.is_empty() {
        return if let Some(tail) = tail {
            let tail_doc = traverse_precedence(arena, source, context, Precedence::Top, tail);
            arena.text("| ").append(tail_doc)
        } else {
            arena.nil()
        };
    }

    let field_docs = fields
        .iter()
        .map(|field| {
            let field_type = traverse_precedence(arena, source, context, Precedence::Top, field.id);
            (field.label.to_string(), field_type)
        })
        .collect_vec();

    let format_field = |arena: &'a Arena<'a>, label: String, ty: Doc<'a>| {
        let type_continuation = arena.line().append(ty).nest(2).group();
        arena.text(label).append(arena.text(" ::")).append(type_continuation).align()
    };

    let mut iter = field_docs.into_iter();
    let (first_label, first_type) = iter.next().unwrap();
    let first_field = format_field(arena, first_label, first_type);

    let leading_comma = arena.hardline().append(arena.text(", "));
    let leading_comma = leading_comma.flat_alt(arena.text(", "));

    let rest_fields = iter.fold(arena.nil(), |acc, (label, ty)| {
        acc.append(leading_comma.clone()).append(format_field(arena, label, ty))
    });

    let fields_doc = first_field.append(rest_fields);

    if let Some(tail) = tail {
        let tail_doc = traverse_precedence(arena, source, context, Precedence::Top, tail);

        let leading_pipe = arena.hardline().append(arena.text("| "));
        let leading_pipe = leading_pipe.flat_alt(arena.text(" | "));

        fields_doc.append(leading_pipe).append(tail_doc)
    } else {
        fields_doc
    }
}

fn is_record_constructor<Q>(source: &mut TraversalSource<Q>, id: TypeId) -> bool
where
    Q: ExternalQueries,
{
    if let Type::Constructor(file_id, type_id) = source.lookup(id)
        && file_id == source.queries().prim_id()
        && let Some(name) = lookup_type_name(source, file_id, type_id)
    {
        return name == "Record";
    }
    false
}
