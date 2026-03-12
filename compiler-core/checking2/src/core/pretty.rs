//! Implements the pretty printer for core types.

use itertools::Itertools;
use lowering::StringKind;
use pretty::{Arena, DocAllocator, DocBuilder};
use rustc_hash::FxHashMap;
use smol_str::{SmolStr, SmolStrBuilder};

use crate::core::{
    ForallBinder, ForallBinderId, Name, RowField, RowType, RowTypeId, SmolStrId, Type, TypeId,
};
use crate::{CheckedModule, ExternalQueries};

type Doc<'a> = DocBuilder<'a, Arena<'a>, ()>;

pub struct Pretty<'a, Q> {
    queries: &'a Q,
    width: usize,
    signature: Option<&'a str>,
    checked: &'a CheckedModule,
}

impl<'a, Q: ExternalQueries> Pretty<'a, Q> {
    pub fn new(queries: &'a Q, checked: &'a CheckedModule) -> Self {
        Pretty { queries, width: 100, signature: None, checked }
    }

    pub fn width(mut self, width: usize) -> Self {
        self.width = width;
        self
    }

    pub fn signature(mut self, name: &'a str) -> Self {
        self.signature = Some(name);
        self
    }

    pub fn render(self, id: TypeId) -> SmolStr {
        let arena = Arena::new();
        let mut printer = Printer::new(&arena, self.queries, &self.checked.names);

        let document = if let Some(name) = self.signature {
            printer.signature(name, id)
        } else {
            printer.traverse(Precedence::Top, id)
        };

        let mut output = SmolStrBuilder::new();
        document
            .render_fmt(self.width, &mut output)
            .expect("critical failure: failed to render type");
        output.finish()
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

struct Printer<'a, Q>
where
    Q: ExternalQueries,
{
    arena: &'a Arena<'a>,
    queries: &'a Q,
    names: &'a FxHashMap<Name, SmolStrId>,
}

impl<'a, Q> Printer<'a, Q>
where
    Q: ExternalQueries,
{
    fn new(
        arena: &'a Arena<'a>,
        queries: &'a Q,
        names: &'a FxHashMap<Name, SmolStrId>,
    ) -> Printer<'a, Q> {
        Printer { arena, queries, names }
    }

    fn lookup_type(&self, id: TypeId) -> Type {
        self.queries.lookup_type(id)
    }

    fn lookup_forall_binder(&self, id: ForallBinderId) -> ForallBinder {
        self.queries.lookup_forall_binder(id)
    }

    fn lookup_row_type(&self, id: RowTypeId) -> RowType {
        self.queries.lookup_row_type(id)
    }

    fn lookup_smol_str(&self, id: SmolStrId) -> smol_str::SmolStr {
        self.queries.lookup_smol_str(id)
    }

    fn lookup_type_name(
        &self,
        file_id: files::FileId,
        type_id: indexing::TypeItemId,
    ) -> Option<String> {
        let indexed = self.queries.indexed(file_id).ok()?;
        indexed.items[type_id].name.as_ref().map(|name| name.to_string())
    }

    fn is_record_constructor(&self, id: TypeId) -> bool {
        if let Type::Constructor(file_id, type_id) = self.lookup_type(id)
            && file_id == self.queries.prim_id()
            && let Some(name) = self.lookup_type_name(file_id, type_id)
        {
            return name == "Record";
        }
        false
    }

    fn parens_if(&self, condition: bool, doc: Doc<'a>) -> Doc<'a> {
        if condition { self.arena.text("(").append(doc).append(self.arena.text(")")) } else { doc }
    }

    fn signature(&mut self, name: &str, id: TypeId) -> Doc<'a> {
        let signature = self.traverse(Precedence::Top, id);
        let signature = self.arena.line().append(signature).nest(2);
        self.arena.text(format!("{name} ::")).append(signature).group()
    }

    fn traverse(&mut self, precedence: Precedence, id: TypeId) -> Doc<'a> {
        match self.lookup_type(id) {
            Type::Application(function, argument) => {
                self.traverse_application(precedence, function, argument)
            }

            Type::KindApplication(function, argument) => {
                self.traverse_kind_application(precedence, function, argument)
            }

            Type::Forall(binder_id, inner) => self.traverse_forall(precedence, binder_id, inner),

            Type::Constrained(constraint, inner) => {
                self.traverse_constrained(precedence, constraint, inner)
            }

            Type::Function(argument, result) => {
                self.traverse_function(precedence, argument, result)
            }

            Type::Kinded(inner, kind) => {
                let inner = self.traverse(Precedence::Application, inner);
                let kind = self.traverse(Precedence::Top, kind);
                let kinded = inner.append(self.arena.text(" :: ")).append(kind);
                self.parens_if(precedence > Precedence::Atom, kinded)
            }

            Type::Constructor(file_id, type_id) => {
                let name = self
                    .lookup_type_name(file_id, type_id)
                    .unwrap_or_else(|| "<InvalidName>".to_string());
                self.arena.text(name)
            }

            Type::Integer(integer) => {
                let negative = integer.is_negative();
                let integer = self.arena.text(format!("{integer}"));
                self.parens_if(negative, integer)
            }

            Type::String(kind, string_id) => {
                let string = self.lookup_smol_str(string_id);
                match kind {
                    StringKind::String => self.arena.text(format!("\"{string}\"")),
                    StringKind::RawString => self.arena.text(format!("\"\"\"{string}\"\"\"")),
                }
            }

            Type::Row(row_id) => {
                let row = self.lookup_row_type(row_id);
                if row.fields.is_empty() && row.tail.is_none() {
                    return self.arena.text("()");
                }
                self.format_row(&row.fields, row.tail)
            }

            Type::Rigid(name, _, kind) => {
                let text = match self.names.get(&name) {
                    Some(&id) => self.lookup_smol_str(id),
                    None => name.as_text(),
                };
                let kind = self.traverse(Precedence::Top, kind);
                self.arena.text(format!("({text} :: ")).append(kind).append(self.arena.text(")"))
            }

            Type::Unification(unification_id) => self.arena.text(format!("?{unification_id}")),

            Type::Free(name_id) => {
                let name = self.lookup_smol_str(name_id);
                self.arena.text(format!("{name}"))
            }

            Type::Unknown(name_id) => {
                let name = self.lookup_smol_str(name_id);
                self.arena.text(format!("?[{name}]"))
            }
        }
    }
}

impl<'a, Q> Printer<'a, Q>
where
    Q: ExternalQueries,
{
    fn traverse_application(
        &mut self,
        precedence: Precedence,
        mut function: TypeId,
        argument: TypeId,
    ) -> Doc<'a> {
        if self.is_record_constructor(function) {
            return self.format_record_application(argument);
        }

        let mut arguments = vec![argument];

        while let Type::Application(inner_function, argument) = self.lookup_type(function) {
            function = inner_function;
            arguments.push(argument);
        }

        let function = self.traverse(Precedence::Application, function);

        let arguments = arguments
            .iter()
            .rev()
            .map(|&argument| self.traverse(Precedence::Atom, argument))
            .collect_vec();

        let arguments = arguments.into_iter().fold(self.arena.nil(), |builder, argument| {
            builder.append(self.arena.line()).append(argument)
        });

        let application = function.append(arguments.nest(2)).group();
        self.parens_if(precedence > Precedence::Application, application)
    }

    fn format_record_application(&mut self, argument: TypeId) -> Doc<'a> {
        match self.lookup_type(argument) {
            Type::Row(row_id) => {
                let row = self.lookup_row_type(row_id);
                self.format_record(&row.fields, row.tail)
            }
            _ => {
                let inner = self.traverse(Precedence::Top, argument);
                self.arena.text("{| ").append(inner).append(self.arena.text(" }"))
            }
        }
    }

    fn traverse_kind_application(
        &mut self,
        precedence: Precedence,
        mut function: TypeId,
        argument: TypeId,
    ) -> Doc<'a> {
        let mut arguments = vec![argument];

        while let Type::KindApplication(inner_function, argument) = self.lookup_type(function) {
            function = inner_function;
            arguments.push(argument);
        }

        let function = self.traverse(Precedence::Application, function);

        let arguments = arguments
            .iter()
            .rev()
            .map(|&argument| self.traverse(Precedence::Atom, argument))
            .collect_vec();

        let arguments = arguments.into_iter().fold(self.arena.nil(), |builder, argument| {
            builder.append(self.arena.line()).append(self.arena.text("@")).append(argument)
        });

        let application = function.append(arguments.nest(2)).group();
        self.parens_if(precedence > Precedence::Application, application)
    }
}

impl<'a, Q> Printer<'a, Q>
where
    Q: ExternalQueries,
{
    fn traverse_forall(
        &mut self,
        precedence: Precedence,
        binder_id: ForallBinderId,
        mut inner: TypeId,
    ) -> Doc<'a> {
        let binder = self.lookup_forall_binder(binder_id);
        let mut binders = vec![binder];

        while let Type::Forall(next_binder_id, next_inner) = self.lookup_type(inner) {
            binders.push(self.lookup_forall_binder(next_binder_id));
            inner = next_inner;
        }

        let binders = binders
            .iter()
            .map(|binder| {
                let text = match self.names.get(&binder.name) {
                    Some(&id) => self.lookup_smol_str(id),
                    None => binder.name.as_text(),
                };
                let kind = self.traverse(Precedence::Top, binder.kind);
                self.arena
                    .text(format!("({} :: ", text))
                    .append(kind)
                    .append(self.arena.text(")"))
                    .group()
            })
            .collect_vec();

        let mut binders = binders.into_iter();
        let binders = if let Some(first) = binders.next() {
            binders.fold(first, |builder, binder| {
                builder.append(self.arena.line().append(binder).nest(2).group())
            })
        } else {
            self.arena.nil()
        };

        let header = self.arena.text("forall ").append(binders).append(self.arena.text("."));
        let inner = self.traverse(Precedence::Top, inner);
        let inner = self.arena.line().append(inner).nest(2);
        let forall = header.append(inner).group();

        self.parens_if(precedence > Precedence::Top, forall)
    }

    fn traverse_constrained(
        &mut self,
        precedence: Precedence,
        constraint: TypeId,
        mut inner: TypeId,
    ) -> Doc<'a> {
        let mut constraints = vec![constraint];

        while let Type::Constrained(constraint, next_inner) = self.lookup_type(inner) {
            constraints.push(constraint);
            inner = next_inner;
        }

        let constraints = constraints
            .iter()
            .map(|&constraint| self.traverse(Precedence::Application, constraint))
            .collect_vec();

        let inner = self.traverse(Precedence::Constraint, inner);

        let arrow = self.arena.text(" =>").append(self.arena.line());
        let constraints = constraints.into_iter().fold(self.arena.nil(), |builder, constraint| {
            builder.append(constraint).append(arrow.clone())
        });

        let constraints = constraints.append(inner).group();
        self.parens_if(precedence > Precedence::Constraint, constraints)
    }

    fn traverse_function(
        &mut self,
        precedence: Precedence,
        argument: TypeId,
        mut result: TypeId,
    ) -> Doc<'a> {
        let mut arguments = vec![argument];

        while let Type::Function(argument, next_result) = self.lookup_type(result) {
            result = next_result;
            arguments.push(argument);
        }

        let arguments = arguments
            .iter()
            .map(|&argument| self.traverse(Precedence::Application, argument))
            .collect_vec();

        let result = self.traverse(Precedence::Function, result);

        let arrow = self.arena.text(" ->").append(self.arena.line());
        let arguments = arguments.into_iter().fold(self.arena.nil(), |builder, argument| {
            builder.append(argument).append(arrow.clone())
        });

        let function = arguments.append(result).group();
        self.parens_if(precedence > Precedence::Function, function)
    }
}

impl<'a, Q> Printer<'a, Q>
where
    Q: ExternalQueries,
{
    fn format_record(&mut self, fields: &[RowField], tail: Option<TypeId>) -> Doc<'a> {
        if fields.is_empty() && tail.is_none() {
            return self.arena.text("{}");
        }
        let body = self.format_row_body(fields, tail);
        self.arena
            .text("{ ")
            .append(body)
            .append(self.arena.line())
            .append(self.arena.text("}"))
            .group()
    }

    fn format_row(&mut self, fields: &[RowField], tail: Option<TypeId>) -> Doc<'a> {
        let body = self.format_row_body(fields, tail);
        self.arena
            .text("( ")
            .append(body)
            .append(self.arena.line())
            .append(self.arena.text(")"))
            .group()
    }

    fn format_row_body(&mut self, fields: &[RowField], tail: Option<TypeId>) -> Doc<'a> {
        if fields.is_empty() {
            return if let Some(tail) = tail {
                let tail = self.traverse(Precedence::Top, tail);
                self.arena.text("| ").append(tail)
            } else {
                self.arena.nil()
            };
        }

        let fields = fields
            .iter()
            .map(|field| {
                let field_type = self.traverse(Precedence::Top, field.id);
                (field.label.to_string(), field_type)
            })
            .collect_vec();

        let format_field = |arena: &'a Arena<'a>, label: String, field_type: Doc<'a>| {
            let field_type = arena.line().append(field_type).nest(2).group();
            arena.text(format!("{label} ::")).append(field_type).align()
        };

        let mut fields = fields.into_iter();
        let (first_label, first_type) = fields.next().unwrap();
        let first = format_field(self.arena, first_label, first_type);

        let leading_comma = self.arena.hardline().append(self.arena.text(", "));
        let leading_comma = leading_comma.flat_alt(self.arena.text(", "));

        let fields = fields.fold(first, |builder, (label, field_type)| {
            builder
                .append(leading_comma.clone())
                .append(format_field(self.arena, label, field_type))
        });

        if let Some(tail) = tail {
            let tail = self.traverse(Precedence::Top, tail);
            let leading_pipe = self.arena.hardline().append(self.arena.text("| "));
            let leading_pipe = leading_pipe.flat_alt(self.arena.text(" | "));
            fields.append(leading_pipe).append(tail)
        } else {
            fields
        }
    }
}
