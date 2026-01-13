//! Generic constraint generation for derive instance with higher-kinded support.
//!
//! When deriving classes like `Eq` for types with higher-kinded type
//! parameters, we need to emit the corresponding `*1` constraints like
//! `Eq1` for type variables of kind `Type -> Type`.

use files::FileId;
use indexing::TypeItemId;

use crate::ExternalQueries;
use crate::algorithm::derive::tools;
use crate::algorithm::fold::Zonk;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::core::{RowType, Type, TypeId, Variable};

/// Generates constraints for a field type, handling higher-kinded type variables.
///
/// Given `f :: Type -> Type` and deriving `Eq`
///
/// For a field type like `f Int`
/// - Emits `Eq1 f` instead of `Eq (f Int)`
///
/// For a field type like `f (g Int)`
/// - Emits `Eq1 f` instead of `Eq (f (g Int))`
/// - Emits `Eq1 g` instead of `Eq (g Int)`
///
/// For nominal types like `Array (f Int)`
/// - Emits `Class (Array (f Int))`
///
/// For records like `{ a :: f Int }`
/// - Each field is traversed, emits `Eq1 f`
pub fn generate_constraint<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
    class: (FileId, TypeItemId),
    class1: Option<(FileId, TypeItemId)>,
) where
    Q: ExternalQueries,
{
    let type_id = state.normalize_type(type_id);
    match state.storage[type_id].clone() {
        Type::Application(function, argument) => {
            let function = state.normalize_type(function);
            if function == context.prim.record {
                generate_constraint(state, context, argument, class, class1);
            } else if is_variable_type_type(state, context, function) {
                if let Some(class1) = class1 {
                    tools::emit_constraint(state, class1, function);
                }
                generate_constraint(state, context, argument, class, class1);
            } else {
                tools::emit_constraint(state, class, type_id);
            }
        }
        Type::Row(RowType { ref fields, .. }) => {
            for field in fields.iter() {
                generate_constraint(state, context, field.id, class, class1);
            }
        }
        _ => {
            tools::emit_constraint(state, class, type_id);
        }
    }
}

fn is_variable_type_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
) -> bool
where
    Q: ExternalQueries,
{
    let type_id = state.normalize_type(type_id);

    let Type::Variable(ref variable) = state.storage[type_id] else {
        return false;
    };

    let Some(kind) = lookup_variable_kind(state, variable) else {
        return false;
    };

    Zonk::on(state, kind) == context.prim.type_to_type
}

fn lookup_variable_kind(state: &CheckState, variable: &Variable) -> Option<TypeId> {
    match variable {
        Variable::Skolem(_, kind) => Some(*kind),
        Variable::Bound(level) => state.type_scope.kinds.get(*level).copied(),
        Variable::Free(_) => None,
    }
}
