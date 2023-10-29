use la_arena::Arena;
use pretty::{BoxAllocator, DocAllocator, DocBuilder, Pretty};

use crate::names::{ModuleName, NameRef, Qualified};

use super::{Binder, Expr, ExprId, Literal, Type};

pub struct PrettyPrinter<'a> {
    expr_arena: &'a Arena<Expr>,
    binder_arena: &'a Arena<Binder>,
    type_arena: &'a Arena<Type>,
    allocator: BoxAllocator,
}

impl<'a> PrettyPrinter<'a> {
    pub fn new(
        expr_arena: &'a Arena<Expr>,
        binder_arena: &'a Arena<Binder>,
        type_arena: &'a Arena<Type>,
    ) -> PrettyPrinter<'a> {
        let allocator = BoxAllocator;
        PrettyPrinter { expr_arena, binder_arena, type_arena, allocator }
    }

    fn kinded<'b>(
        &'b self,
        kind: impl Pretty<'b, BoxAllocator>,
        value: impl Pretty<'b, BoxAllocator>,
    ) -> DocBuilder<'b, BoxAllocator> {
        kind.pretty(&self.allocator)
            .append(self.allocator.line())
            .append(value.pretty(&self.allocator))
            .nest(2)
            .group()
            .parens()
    }

    pub fn module_name<'b>(&'b self, module_name: &'b ModuleName) -> DocBuilder<'b, BoxAllocator> {
        self.allocator.intersperse(
            module_name.iter().map(|segment| self.allocator.text(segment.as_str())),
            self.allocator.text("."),
        )
    }

    pub fn expr_constructor<'b>(
        &'b self,
        constructor: &'b Qualified<NameRef>,
    ) -> DocBuilder<'b, BoxAllocator> {
        match &constructor.prefix {
            Some(module_name) => {
                self.module_name(module_name).append(".").append(constructor.value.as_ref())
            }
            None => {
                self.allocator.text("?.").append(self.allocator.text(constructor.value.as_ref()))
            }
        }
    }

    pub fn expr_literal<'b>(
        &'b self,
        literal: &'b Literal<ExprId>,
    ) -> DocBuilder<'b, BoxAllocator> {
        match literal {
            Literal::Array(array) => {
                let kind = self.allocator.text("Array");
                let array = self
                    .allocator
                    .intersperse(
                        array.iter().map(|element_id| self.expr(*element_id)),
                        self.allocator.line(),
                    )
                    .nest(1)
                    .group()
                    .brackets();
                self.kinded(kind, array)
            }
            Literal::Record(_) => {
                let kind = self.allocator.text("Record");
                self.kinded(kind, self.allocator.text("..."))
            }
            Literal::Int(int) => {
                let kind = self.allocator.text("Int");
                let int = self.allocator.text(format!("{}", int));
                self.kinded(kind, int)
            }
            Literal::Number(number) => {
                let kind = self.allocator.text("Number");
                let number = self.allocator.text(number.as_ref());
                self.kinded(kind, number)
            }
            Literal::String(string) => {
                let kind = self.allocator.text("Number");
                let string = self.allocator.text(string.as_ref());
                self.kinded(kind, string)
            }
            Literal::Char(character) => {
                let kind = self.allocator.text("Char");
                let character = self.allocator.text(character.as_ref());
                self.kinded(kind, character)
            }
            Literal::Boolean(boolean) => {
                let kind = self.allocator.text("Boolean");
                let boolean = self.allocator.text(if *boolean { "true" } else { "false" });
                self.kinded(kind, boolean)
            }
        }
    }

    pub fn expr_variable<'b>(
        &'b self,
        variable: &'b Qualified<NameRef>,
    ) -> DocBuilder<'b, BoxAllocator> {
        match &variable.prefix {
            Some(module_name) => {
                self.module_name(module_name).append(".").append(variable.value.as_ref())
            }
            None => self.allocator.text("?.").append(self.allocator.text(variable.value.as_ref())),
        }
    }

    pub fn expr(&self, expr_id: ExprId) -> DocBuilder<BoxAllocator> {
        match &self.expr_arena[expr_id] {
            Expr::Application(head_id, spine_id) => {
                let kind = self.allocator.text("Application");
                let head = self.expr(*head_id);
                let spine = spine_id.iter().map(|spine_id| self.expr(*spine_id));
                self.allocator
                    .intersperse([kind, head].into_iter().chain(spine), self.allocator.line())
                    .nest(2)
                    .group()
                    .parens()
            }
            Expr::Constructor(constructor) => {
                let kind = self.allocator.text("Constructor");
                let constructor = self.expr_constructor(constructor);
                self.kinded(kind, constructor)
            }
            Expr::LetIn(_, _) => {
                let kind = self.allocator.text("LetIn");
                self.kinded(kind, self.allocator.text("..."))
            }
            Expr::Literal(literal) => {
                let kind = self.allocator.text("Literal");
                let literal = self.expr_literal(literal);
                self.kinded(kind, literal)
            }
            Expr::Variable(variable) => {
                let kind = self.allocator.text("Variable");
                let variable = self.expr_variable(variable);
                self.kinded(kind, variable)
            }
        }
    }
}
