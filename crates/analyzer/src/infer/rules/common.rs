use std::sync::Arc;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    id::InFile,
    infer::{Constraint, CoreTypeVariable, Hint, InferError, InferErrorKind},
    scope::TypeConstructorKind,
    surface::tree::*,
    InferenceDatabase,
};

use super::{CoreType, CoreTypeId, InferContext};

impl InferContext<'_> {
    pub(super) fn fresh_unification(&mut self, db: &dyn InferenceDatabase) -> CoreTypeId {
        let file_id = self.file_id;
        let value = self.state.count;
        self.state.count += 1;
        db.intern_type(CoreType::Unification(InFile { file_id, value }))
    }

    pub(super) fn lower_type(&mut self, db: &dyn InferenceDatabase, type_id: TypeId) -> CoreTypeId {
        match &self.arena[type_id] {
            Type::Arrow(arguments, result) => {
                let result = self.lower_type(db, *result);
                let arguments = arguments.iter().map(|argument| self.lower_type(db, *argument));
                arguments.rev().fold(result, |result, argument| {
                    db.intern_type(CoreType::Function(argument, result))
                })
            }
            Type::Application(function, arguments) => {
                let function = self.lower_type(db, *function);
                let arguments = arguments.iter().map(|argument| self.lower_type(db, *argument));
                arguments.fold(function, |function, argument| {
                    db.intern_type(CoreType::Application(function, argument))
                })
            }
            Type::Constrained(_, _) => {
                todo!("lower_type(Constrained)");
            }
            Type::Constructor(name) => {
                // FIXME: actually resolve primitives
                if matches!(name.value.as_ref(), "Int" | "Number" | "Eq") {
                    return db.intern_type(CoreType::Primitive(Name::clone(&name.value)));
                }

                let resolution = self.resolve.per_type_type.get(&type_id);
                db.intern_type(resolution.map_or(CoreType::NotImplemented, |resolution| {
                    let file_id = resolution.file_id;
                    match resolution.kind {
                        TypeConstructorKind::Data(data_id) => {
                            CoreType::Constructor(InFile { file_id, value: data_id })
                        }
                    }
                }))
            }
            Type::Forall(variables, inner) => {
                let inner = self.lower_type(db, *inner);
                variables.iter().rev().fold(inner, |inner, variable| {
                    let variable = match variable {
                        TypeVariable::Kinded(name, kind) => {
                            let name = Name::clone(name);
                            let kind = self.lower_type(db, *kind);
                            CoreTypeVariable::Kinded(name, kind)
                        }
                        TypeVariable::Name(name) => {
                            let name = Name::clone(name);
                            CoreTypeVariable::Name(name)
                        }
                    };
                    db.intern_type(CoreType::Forall(variable, inner))
                })
            }
            Type::Parenthesized(parenthesized) => self.lower_type(db, *parenthesized),
            Type::Variable(name) => {
                let name = Name::clone(name);
                db.intern_type(CoreType::Variable(name))
            }
            Type::NotImplemented => db.intern_type(CoreType::NotImplemented),
        }
    }

    pub(super) fn peel_arguments(
        &mut self,
        db: &dyn InferenceDatabase,
        type_id: CoreTypeId,
    ) -> (Vec<CoreTypeId>, CoreTypeId) {
        let mut arguments = vec![];
        let mut current = type_id;
        while let CoreType::Function(argument, result) = db.lookup_intern_type(current) {
            arguments.push(argument);
            current = result;
        }
        (arguments, current)
    }

    pub(super) fn subsume_types(
        &mut self,
        db: &dyn InferenceDatabase,
        x_id: CoreTypeId,
        y_id: CoreTypeId,
    ) {
        let hints = self.current_hints();
        self.subsume_types_with_hints(db, hints, x_id, y_id);
    }

    pub(super) fn subsume_types_with_hints(
        &mut self,
        db: &dyn InferenceDatabase,
        hints: Arc<[Hint]>,
        x_id: CoreTypeId,
        y_id: CoreTypeId,
    ) {
        let x_ty = db.lookup_intern_type(x_id);
        let y_ty = db.lookup_intern_type(y_id);

        match (x_ty, y_ty) {
            (CoreType::Forall(variable, inner_ty), _) => {
                let name = Name::clone(variable.name());
                let fresh_ty = self.fresh_unification(db);

                let mut replacements = FxHashMap::default();
                replacements.insert(name, fresh_ty);

                let inner_ty = replace_type(db, &replacements, inner_ty);
                self.subsume_types_with_hints(db, hints, inner_ty, y_id);
            }
            (_, CoreType::Forall(_, _)) => {
                todo!("Skolemize");
            }
            (
                CoreType::Function(argument_x_ty, result_x_ty),
                CoreType::Function(argument_y_ty, result_y_ty),
            ) => {
                self.subsume_types_with_hints(db, Arc::clone(&hints), argument_y_ty, argument_x_ty);
                self.subsume_types_with_hints(db, hints, result_x_ty, result_y_ty);
            }
            _ => {
                self.unify_types_with_hints(db, hints, x_id, y_id);
            }
        }
    }

    pub(super) fn unify_types(
        &mut self,
        db: &dyn InferenceDatabase,
        x_id: CoreTypeId,
        y_id: CoreTypeId,
    ) {
        let hints = self.current_hints();
        self.unify_types_with_hints(db, hints, x_id, y_id);
    }

    pub(super) fn unify_types_with_hints(
        &mut self,
        db: &dyn InferenceDatabase,
        hints: Arc<[Hint]>,
        x_id: CoreTypeId,
        y_id: CoreTypeId,
    ) {
        let x_ty = db.lookup_intern_type(x_id);
        let y_ty = db.lookup_intern_type(y_id);

        match (x_ty, y_ty) {
            (
                CoreType::Application(x_function, x_argument),
                CoreType::Application(y_function, y_argument),
            ) => {
                self.unify_types_with_hints(db, Arc::clone(&hints), x_function, y_function);
                self.unify_types_with_hints(db, hints, x_argument, y_argument);
            }
            (
                CoreType::Function(x_argument, x_result),
                CoreType::Function(y_argument, y_result),
            ) => {
                self.unify_types_with_hints(db, Arc::clone(&hints), x_argument, y_argument);
                self.unify_types_with_hints(db, hints, x_result, y_result);
            }
            (CoreType::Unification(x_u), CoreType::Unification(y_u)) => {
                if x_u != y_u {
                    self.add_constraint(Constraint::UnifyDeep(hints, x_u, y_u));
                }
            }
            (CoreType::Unification(x_u), _) => {
                self.add_constraint(Constraint::UnifySolve(hints, x_u, y_id));
            }
            (_, CoreType::Unification(y_u)) => {
                self.add_constraint(Constraint::UnifySolve(hints, y_u, x_id));
            }
            (CoreType::Constructor(x_c), CoreType::Constructor(y_c)) => {
                if x_c != y_c {
                    self.add_error(InferError {
                        hints,
                        kind: InferErrorKind::CannotUnify(x_id, y_id),
                    })
                }
            }
            (CoreType::Primitive(x_p), CoreType::Primitive(y_p)) => {
                if x_p != y_p {
                    self.add_error(InferError {
                        hints,
                        kind: InferErrorKind::CannotUnify(x_id, y_id),
                    });
                }
            }
            (x_ty, y_ty) => {
                unimplemented!("Oh No! {:?} ~ {:?}", x_ty, y_ty);
            }
        }
    }

    pub(super) fn instantiate_type(
        &mut self,
        db: &dyn InferenceDatabase,
        type_id: CoreTypeId,
    ) -> CoreTypeId {
        if let CoreType::Forall(initial_variable, initial_body) = db.lookup_intern_type(type_id) {
            let mut replacements = FxHashMap::default();

            let initial_name = Name::clone(initial_variable.name());
            let initial_unification = self.fresh_unification(db);
            replacements.insert(initial_name, initial_unification);
            let mut current_body = initial_body;

            while let CoreType::Forall(variable, body) = db.lookup_intern_type(current_body) {
                let name = Name::clone(variable.name());
                let unification = self.fresh_unification(db);
                replacements.insert(name, unification);
                current_body = body;
            }

            replace_type(db, &replacements, current_body)
        } else {
            type_id
        }
    }
}

fn replace_type(
    db: &dyn InferenceDatabase,
    replacements: &FxHashMap<Name, CoreTypeId>,
    type_id: CoreTypeId,
) -> CoreTypeId {
    fn aux(
        db: &dyn InferenceDatabase,
        in_scope: &mut FxHashSet<Name>,
        replacements: &FxHashMap<Name, CoreTypeId>,
        type_id: CoreTypeId,
    ) -> CoreTypeId {
        match db.lookup_intern_type(type_id) {
            CoreType::Application(function, argument) => {
                let function = aux(db, in_scope, replacements, function);
                let argument = aux(db, in_scope, replacements, argument);
                db.intern_type(CoreType::Application(function, argument))
            }
            CoreType::Constructor(_) => type_id,
            CoreType::Forall(variable, body) => {
                in_scope.insert(Name::clone(variable.name()));
                let body = aux(db, in_scope, replacements, body);
                in_scope.remove(variable.name());
                db.intern_type(CoreType::Forall(variable, body))
            }
            CoreType::Function(argument, result) => {
                let argument = aux(db, in_scope, replacements, argument);
                let result = aux(db, in_scope, replacements, result);
                db.intern_type(CoreType::Function(argument, result))
            }
            CoreType::Primitive(_) => type_id,
            CoreType::Unification(_) => type_id,
            CoreType::Variable(name) => {
                if in_scope.contains(&name) {
                    type_id
                } else {
                    replacements
                        .get(&name)
                        .copied()
                        .unwrap_or_else(|| db.intern_type(CoreType::NotImplemented))
                }
            }
            CoreType::NotImplemented => type_id,
        }
    }

    let mut in_scope = FxHashSet::default();
    aux(db, &mut in_scope, replacements, type_id)
}
