//! Implements type walking for the core representation.

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::normalise::normalise;
use crate::core::{ForallBinder, Type, TypeId};
use crate::state::CheckState;

pub enum WalkAction {
    Stop,
    Continue,
}

pub trait TypeWalker {
    fn visit<Q: ExternalQueries>(
        &mut self,
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: TypeId,
        t: &Type,
    ) -> QueryResult<WalkAction>;

    fn visit_binder(&mut self, _binder: &ForallBinder) {}
}

pub fn walk_type<Q, W>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
    walker: &mut W,
) -> QueryResult<()>
where
    Q: ExternalQueries,
    W: TypeWalker,
{
    let id = normalise(state, context, id)?;
    let t = context.queries.lookup_type(id);

    if let WalkAction::Stop = walker.visit(state, context, id, &t)? {
        return Ok(());
    }

    match t {
        Type::Application(function, argument) | Type::KindApplication(function, argument) => {
            walk_type(state, context, function, walker)?;
            walk_type(state, context, argument, walker)?;
        }
        Type::OperatorApplication(_, _, left, right) => {
            walk_type(state, context, left, walker)?;
            walk_type(state, context, right, walker)?;
        }
        Type::SynonymApplication(synonym_id) => {
            let synonym = context.queries.lookup_synonym(synonym_id);
            for &argument in synonym.arguments.iter() {
                walk_type(state, context, argument, walker)?;
            }
        }
        Type::Forall(binder_id, inner) => {
            let binder = context.queries.lookup_forall_binder(binder_id);
            walker.visit_binder(&binder);
            walk_type(state, context, binder.kind, walker)?;
            walk_type(state, context, inner, walker)?;
        }
        Type::Constrained(constraint, inner) => {
            walk_type(state, context, constraint, walker)?;
            walk_type(state, context, inner, walker)?;
        }
        Type::Function(argument, result) => {
            walk_type(state, context, argument, walker)?;
            walk_type(state, context, result, walker)?;
        }
        Type::Kinded(inner, kind) => {
            walk_type(state, context, inner, walker)?;
            walk_type(state, context, kind, walker)?;
        }
        Type::Constructor(_, _) | Type::OperatorConstructor(_, _) => {}
        Type::Integer(_) | Type::String(_, _) => {}
        Type::Row(row_id) => {
            let row = context.queries.lookup_row_type(row_id);
            for field in row.fields.iter() {
                walk_type(state, context, field.id, walker)?;
            }
            if let Some(tail) = row.tail {
                walk_type(state, context, tail, walker)?;
            }
        }
        Type::Rigid(_, _, kind) => {
            walk_type(state, context, kind, walker)?;
        }
        Type::Unification(_) | Type::Free(_) | Type::Unknown(_) => {}
    }

    Ok(())
}
