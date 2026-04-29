//! Implements type folding for the core representation.

use building_types::QueryResult;

use crate::context::CheckContext;
use crate::core::{ForallBinder, RowField, RowTypeId, Type, TypeId, normalise};
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
            let (mut fields, tail) = flatten_row(state, context, row_id)?;
            for field in fields.iter_mut() {
                field.id = fold_type(state, context, field.id, folder)?;
            }
            let tail = tail.map(|tail| fold_type(state, context, tail, folder)).transpose()?;
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

/// Flatten a row type into a single row.
///
/// In most cases, row fields are stored contiguously in the [`RowType`] struct.
/// However, the constraint solver or other parts of the compiler may produce
/// the following transient structure:
///
/// ```purescript
/// ?t9 = ( t9 :: Type | ?t8 )
/// ?t8 = ( t8 :: Type | ?t8 )
/// ?t7 = ( t7 :: Type | ?t6 )
/// ```
///
/// This is effectively a linked list of row fields, and in exceptional cases,
/// recursive traversal of this structure is ineffective and leads to stack overflow.
///
/// This function flattens a given [`RowType`] by traversing the [`RowType::tail`]
/// and collecting fields, building the ideal canonical representation for a row type.
fn flatten_row<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    row_id: RowTypeId,
) -> QueryResult<(Vec<RowField>, Option<TypeId>)>
where
    Q: ExternalQueries,
{
    let mut row = context.lookup_row_type(row_id);
    let mut fields = row.fields.to_vec();

    safe_loop! {
        let Some(tail) = row.tail else {
            break;
        };

        let tail = normalise::normalise(state, context, tail)?;
        let Type::Row(row_id) = context.lookup_type(tail) else {
            break;
        };

        row = context.lookup_row_type(row_id);
        fields.extend(row.fields.iter().cloned());
    }

    Ok((fields, row.tail))
}
