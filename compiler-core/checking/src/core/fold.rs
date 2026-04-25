//! Implements type folding for the core representation.

use building_types::QueryResult;

use crate::context::CheckContext;
use crate::core::{ForallBinder, Type, TypeId, normalise};
use crate::state::CheckState;
use crate::{ExternalQueries, safe_loop};

pub enum FoldAction {
    Replace(TypeId),
    ReplaceThen(TypeId),
    Continue,
}

pub trait TypeFold {
    fn transform<Q: ExternalQueries>(
        &mut self,
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: TypeId,
        t: &Type,
    ) -> QueryResult<FoldAction>;

    fn transform_binder(&mut self, _binder: &mut ForallBinder) {}
}

pub fn fold_type<Q, F>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
    folder: &mut F,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
    F: TypeFold,
{
    let mut id = normalise::normalise(state, context, id)?;

    safe_loop! {
        let t = context.lookup_type(id);
        match folder.transform(state, context, id, &t)? {
            FoldAction::Replace(id) => return Ok(id),
            FoldAction::ReplaceThen(then_id) => {
                let then_id = normalise::normalise(state, context, then_id)?;
                if then_id == id {
                    break;
                }
                id = then_id;
                continue;
            }
            FoldAction::Continue => {
                break;
            },
        }
    }

    let t = context.lookup_type(id);
    let result = match t {
        Type::Application(function, argument) => {
            let function = fold_type(state, context, function, folder)?;
            let argument = fold_type(state, context, argument, folder)?;
            context.intern_application(function, argument)
        }
        Type::KindApplication(function, argument) => {
            let function = fold_type(state, context, function, folder)?;
            let argument = fold_type(state, context, argument, folder)?;
            context.intern_kind_application(function, argument)
        }
        Type::Forall(binder_id, inner) => {
            let mut binder = context.lookup_forall_binder(binder_id);
            folder.transform_binder(&mut binder);
            binder.kind = fold_type(state, context, binder.kind, folder)?;
            let inner = fold_type(state, context, inner, folder)?;
            let binder_id = context.intern_forall_binder(binder);
            context.intern_forall(binder_id, inner)
        }
        Type::Constrained(constraint, inner) => {
            let constraint = fold_type(state, context, constraint, folder)?;
            let inner = fold_type(state, context, inner, folder)?;
            context.intern_constrained(constraint, inner)
        }
        Type::Function(argument, result) => {
            let argument = fold_type(state, context, argument, folder)?;
            let result = fold_type(state, context, result, folder)?;
            context.intern_function(argument, result)
        }
        Type::Kinded(inner, kind) => {
            let inner = fold_type(state, context, inner, folder)?;
            let kind = fold_type(state, context, kind, folder)?;
            context.intern_kinded(inner, kind)
        }
        Type::Constructor(_, _) => id,
        Type::Integer(_) | Type::String(_, _) => id,
        Type::Row(row_id) => {
            let row = context.lookup_row_type(row_id);
            let mut fields = row.fields.to_vec();
            for field in fields.iter_mut() {
                field.id = fold_type(state, context, field.id, folder)?;
            }
            let tail = row.tail.map(|tail| fold_type(state, context, tail, folder)).transpose()?;
            context.intern_row(fields, tail)
        }
        Type::Rigid(name, depth, kind) => {
            let kind = fold_type(state, context, kind, folder)?;
            context.intern_rigid(name, depth, kind)
        }
        Type::Unification(_) | Type::Free(_) | Type::Unknown(_) => id,
    };

    Ok(result)
}
