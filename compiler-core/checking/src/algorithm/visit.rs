use files::FileId;
use rustc_hash::FxHashSet;

use crate::algorithm::state::CheckState;
use crate::core::{ForallBinder, RowType, Type, TypeId, Variable};

/// Controls behavior during type visiting.
pub enum VisitAction {
    Stop,
    Continue,
}

/// Trait for implementing read-only type traversal.
pub trait TypeVisitor {
    /// Called before recursing into a type. Return `Stop` to short-circuit.
    fn visit(&mut self, state: &mut CheckState, id: TypeId, t: &Type) -> VisitAction;

    /// Called when visiting a Forall binder.
    fn visit_binder(&mut self, _binder: &ForallBinder) {}
}

/// Collects all files referenced through [`Type::Constructor`].
pub struct CollectFileReferences<'a> {
    pub files: &'a mut FxHashSet<FileId>,
}

impl CollectFileReferences<'_> {
    pub fn on(state: &mut CheckState, id: TypeId, files: &mut FxHashSet<FileId>) {
        visit_type(state, id, &mut CollectFileReferences { files });
    }
}

impl TypeVisitor for CollectFileReferences<'_> {
    fn visit(&mut self, _state: &mut CheckState, _id: TypeId, t: &Type) -> VisitAction {
        if let Type::Constructor(file_id, _) = t {
            self.files.insert(*file_id);
        }
        VisitAction::Continue
    }
}

/// Checks if a type contains any rows with labels.
pub struct HasLabeledRole {
    contains: bool,
}

impl HasLabeledRole {
    pub fn on(state: &mut CheckState, id: TypeId) -> bool {
        let mut visitor = HasLabeledRole { contains: false };
        visit_type(state, id, &mut visitor);
        visitor.contains
    }
}

impl TypeVisitor for HasLabeledRole {
    fn visit(&mut self, _state: &mut CheckState, _id: TypeId, t: &Type) -> VisitAction {
        if let Type::Row(RowType { fields, .. }) = t
            && !fields.is_empty()
        {
            self.contains = true;
            return VisitAction::Stop;
        }
        VisitAction::Continue
    }
}

/// Recursively visit a type without transforming it.
pub fn visit_type<V: TypeVisitor>(state: &mut CheckState, id: TypeId, visitor: &mut V) {
    let id = state.normalize_type(id);
    let ty = state.storage[id].clone();

    if let VisitAction::Stop = visitor.visit(state, id, &ty) {
        return;
    }

    match ty {
        Type::Application(function, argument) => {
            visit_type(state, function, visitor);
            visit_type(state, argument, visitor);
        }
        Type::Constrained(constraint, inner) => {
            visit_type(state, constraint, visitor);
            visit_type(state, inner, visitor);
        }
        Type::Constructor(_, _) => {}
        Type::Forall(binder, inner) => {
            visitor.visit_binder(&binder);
            visit_type(state, binder.kind, visitor);
            visit_type(state, inner, visitor);
        }
        Type::Function(argument, result) => {
            visit_type(state, argument, visitor);
            visit_type(state, result, visitor);
        }
        Type::Integer(_) => {}
        Type::KindApplication(function, argument) => {
            visit_type(state, function, visitor);
            visit_type(state, argument, visitor);
        }
        Type::Kinded(inner, kind) => {
            visit_type(state, inner, visitor);
            visit_type(state, kind, visitor);
        }
        Type::Operator(_, _) => {}
        Type::OperatorApplication(_, _, left, right) => {
            visit_type(state, left, visitor);
            visit_type(state, right, visitor);
        }
        Type::Row(RowType { fields, tail }) => {
            for field in fields.iter() {
                visit_type(state, field.id, visitor);
            }
            if let Some(tail) = tail {
                visit_type(state, tail, visitor);
            }
        }
        Type::String(_, _) => {}
        Type::SynonymApplication(_, _, _, arguments) => {
            for &argument in arguments.iter() {
                visit_type(state, argument, visitor);
            }
        }
        Type::Unification(_) => {}
        Type::Variable(variable) => match variable {
            Variable::Bound(_, kind) | Variable::Skolem(_, kind) => {
                visit_type(state, kind, visitor);
            }
            Variable::Free(_) => {}
        },
        Type::Unknown => {}
    }
}
