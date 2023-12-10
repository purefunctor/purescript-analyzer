//! Implements lowering from [`surface::Type`] to [`infer::Type`]
//!
//! [`surface::Type`]: crate::surface::Type
//! [`infer::Type`]: crate::infer::Type

use la_arena::Arena;

use crate::{
    infer::{Primitive, Type, TypeId},
    surface, InferDatabase,
};

pub(super) fn lower_type(
    db: &dyn InferDatabase,
    type_arena: &Arena<surface::Type>,
    type_id: surface::TypeId,
) -> TypeId {
    match &type_arena[type_id] {
        surface::Type::Arrow(arguments, result) => {
            let arguments = arguments.iter().map(|argument| lower_type(db, type_arena, *argument));
            let result = lower_type(db, type_arena, *result);
            arguments
                .rev()
                .fold(result, |result, argument| db.intern_type(Type::Function(argument, result)))
        }
        surface::Type::Application(constructor, arguments) => {
            let constructor = lower_type(db, type_arena, *constructor);
            let arguments = arguments.iter().map(|argument| lower_type(db, type_arena, *argument));
            arguments.fold(constructor, |constructor, argument| {
                db.intern_type(Type::Application(constructor, argument))
            })
        }
        surface::Type::Constructor(constructor) => {
            if constructor.prefix.is_none() {
                match constructor.value.as_ref() {
                    "Int" => db.intern_type(Type::Primitive(Primitive::Int)),
                    "Number" => db.intern_type(Type::Primitive(Primitive::Number)),
                    "Char" => db.intern_type(Type::Primitive(Primitive::Char)),
                    "String" => db.intern_type(Type::Primitive(Primitive::String)),
                    "Boolean" => db.intern_type(Type::Primitive(Primitive::Boolean)),
                    _ => db.intern_type(Type::NotImplemented),
                }
            } else {
                todo!("Implement name resolution during lowering.")
            }
        }
        surface::Type::Parenthesized(parenthesized) => lower_type(db, type_arena, *parenthesized),
        surface::Type::Variable(_) => db.intern_type(Type::NotImplemented),
    }
}
