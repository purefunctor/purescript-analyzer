//! Implements lowering from [`surface::Type`] to [`infer::Type`]
//!
//! [`surface::Type`]: crate::surface::Type
//! [`infer::Type`]: crate::infer::Type

use la_arena::Arena;

use crate::{
    infer::{Primitive, Type, TypeId},
    scope::Resolutions,
    surface, InferDatabase,
};

struct LowerContext<'env> {
    db: &'env dyn InferDatabase,
    type_arena: &'env Arena<surface::Type>,
    resolutions: &'env Resolutions,
}

impl<'env> LowerContext<'env> {
    fn lower_type(&self, type_id: surface::TypeId) -> TypeId {
        match &self.type_arena[type_id] {
            surface::Type::Arrow(arguments, result) => {
                let arguments = arguments.iter().map(|argument| self.lower_type(*argument));
                let result = self.lower_type(*result);
                arguments.rev().fold(result, |result, argument| {
                    self.db.intern_type(Type::Function(argument, result))
                })
            }
            surface::Type::Application(constructor, arguments) => {
                let constructor = self.lower_type(*constructor);
                let arguments = arguments.iter().map(|argument| self.lower_type(*argument));
                arguments.fold(constructor, |constructor, argument| {
                    self.db.intern_type(Type::Application(constructor, argument))
                })
            }
            surface::Type::Constructor(constructor) => {
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
            surface::Type::Parenthesized(parenthesized) => self.lower_type(*parenthesized),
            surface::Type::Variable(_) => self.db.intern_type(Type::NotImplemented),
        }
    }
}

pub(super) fn lower_type(
    db: &dyn InferDatabase,
    type_arena: &Arena<surface::Type>,
    resolutions: &Resolutions,
    type_id: surface::TypeId,
) -> TypeId {
    LowerContext { db, type_arena, resolutions }.lower_type(type_id)
}
