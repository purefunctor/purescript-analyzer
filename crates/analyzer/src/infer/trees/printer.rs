use pretty::{BoxAllocator, DocAllocator, DocBuilder};

use crate::InferDatabase;

use super::{Primitive, Type, TypeId, Unification};

pub struct PrettyPrinter<'a> {
    db: &'a dyn InferDatabase,
    allocator: BoxAllocator,
}

impl<'a> PrettyPrinter<'a> {
    pub fn new(db: &'a dyn InferDatabase) -> PrettyPrinter<'a> {
        PrettyPrinter { db, allocator: BoxAllocator }
    }

    pub fn ty(&self, ty: TypeId) -> DocBuilder<BoxAllocator> {
        match self.db.lookup_intern_type(ty) {
            Type::Application(constructor_ty, argument_ty) => {
                self.ty_application(constructor_ty, argument_ty)
            }
            Type::Function(argument_ty, result_ty) => self.ty_function(argument_ty, result_ty),
            Type::Primitive(primitive) => self.ty_primitive(primitive),
            Type::Unification(unification) => self.ty_unification(unification),
            Type::NotImplemented => self.ty_not_implemented(),
        }
    }

    fn ty_application(
        &self,
        constructor_ty: TypeId,
        argument_ty: TypeId,
    ) -> DocBuilder<BoxAllocator> {
        let constructor_is_application =
            matches!(self.db.lookup_intern_type(constructor_ty), Type::Application(_, _));

        let argument_is_application =
            matches!(self.db.lookup_intern_type(argument_ty), Type::Application(_, _));

        let constructor = if constructor_is_application {
            self.ty(constructor_ty).parens()
        } else {
            self.ty(constructor_ty)
        };
        let argument = if argument_is_application {
            self.ty(argument_ty).parens()
        } else {
            self.ty(argument_ty)
        };

        constructor.append(self.allocator.space()).append(argument)
    }

    fn ty_function(&self, argument_ty: TypeId, result_ty: TypeId) -> DocBuilder<BoxAllocator> {
        let argument_is_function =
            matches!(self.db.lookup_intern_type(argument_ty), Type::Function(_, _));

        let argument =
            if argument_is_function { self.ty(argument_ty).parens() } else { self.ty(argument_ty) };
        let separator = self.allocator.text(" -> ");
        let result = self.ty(result_ty);

        argument.append(separator).append(result)
    }

    fn ty_primitive(&self, primitive: Primitive) -> DocBuilder<BoxAllocator> {
        match primitive {
            Primitive::Int => self.allocator.text("Int"),
            Primitive::Number => self.allocator.text("Number"),
            Primitive::Char => self.allocator.text("Char"),
            Primitive::String => self.allocator.text("String"),
            Primitive::Boolean => self.allocator.text("Boolean"),
        }
    }

    fn ty_unification(&self, unification: Unification) -> DocBuilder<BoxAllocator> {
        self.allocator.text("?").append(self.allocator.text(format!("{}", unification.index)))
    }

    fn ty_not_implemented(&self) -> DocBuilder<BoxAllocator> {
        self.allocator.text("!")
    }
}
