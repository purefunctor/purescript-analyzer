//! Implements lowering from [`surface::Type`] to [`infer::Type`]
//!
//! [`surface::Type`]: crate::surface::Type
//! [`infer::Type`]: crate::infer::Type

use la_arena::Arena;

use crate::{
    names::{NameRef, Qualified},
    surface, InferDatabase,
};

use super::{Primitive, Type, TypeId};

pub(crate) struct LowerContext<'a> {
    db: &'a dyn InferDatabase,
    type_arena: &'a Arena<surface::Type>,
}

impl<'a> LowerContext<'a> {
    pub(crate) fn new(
        db: &'a dyn InferDatabase,
        type_arena: &'a Arena<surface::Type>,
    ) -> LowerContext<'a> {
        LowerContext { db, type_arena }
    }

    pub(crate) fn lower_type(&self, type_id: surface::TypeId) -> TypeId {
        match &self.type_arena[type_id] {
            surface::Type::Arrow(arguments, result) => self.lower_type_arrow(arguments, *result),
            surface::Type::Application(constructor, arguments) => {
                self.lower_type_application(*constructor, arguments)
            }
            surface::Type::Constructor(constructor) => self.lower_type_constructor(constructor),
            surface::Type::Parenthesized(parenthesized) => self.lower_type(*parenthesized),
            surface::Type::Variable(_) => self.db.intern_type(Type::NotImplemented),
        }
    }

    fn lower_type_arrow(&self, arguments: &[surface::TypeId], result: surface::TypeId) -> TypeId {
        arguments
            .iter()
            .copied()
            .map(|argument| self.lower_type(argument))
            .rev()
            .fold(self.lower_type(result), |result_ty, argument_ty| {
                self.db.intern_type(Type::Function(argument_ty, result_ty))
            })
    }

    fn lower_type_application(
        &self,
        constructor: surface::TypeId,
        arguments: &[surface::TypeId],
    ) -> TypeId {
        arguments.iter().copied().map(|argument| self.lower_type(argument)).fold(
            self.lower_type(constructor),
            |constructor_ty, argument_ty| {
                self.db.intern_type(Type::Application(constructor_ty, argument_ty))
            },
        )
    }

    fn lower_type_constructor(&self, constructor: &Qualified<NameRef>) -> TypeId {
        if constructor.prefix.is_none() {
            match constructor.value.as_ref() {
                "Int" => self.db.intern_type(Type::Primitive(Primitive::Int)),
                "Number" => self.db.intern_type(Type::Primitive(Primitive::Number)),
                "Char" => self.db.intern_type(Type::Primitive(Primitive::Char)),
                "String" => self.db.intern_type(Type::Primitive(Primitive::String)),
                "Boolean" => self.db.intern_type(Type::Primitive(Primitive::Boolean)),
                _ => self.db.intern_type(Type::NotImplemented),
            }
        } else {
            todo!("Implement name resolution during lowering.")
        }
    }
}
