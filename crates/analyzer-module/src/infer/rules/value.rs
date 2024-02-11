use itertools::Itertools;

use crate::{
    id::InFile, index::nominal::ValueGroupId, scope::VariableResolution, surface::tree::*,
    InferenceDatabase,
};

use super::{recursive_let_names, CoreType, CoreTypeId, InferContext};

impl InferContext<'_> {
    pub(super) fn infer_value_scc(
        &mut self,
        db: &dyn InferenceDatabase,
        value_declarations: &[(ValueGroupId, &ValueDeclaration)],
    ) {
        for (value_group_id, _) in value_declarations {
            let fresh_ty = self.fresh_unification(db);
            self.result.of_value_group.insert(*value_group_id, fresh_ty);
        }
        for (value_group_id, value_declaration) in value_declarations {
            self.infer_value_declaration(db, *value_group_id, value_declaration);
        }
    }

    fn infer_value_declaration(
        &mut self,
        db: &dyn InferenceDatabase,
        value_group_id: ValueGroupId,
        value_declaration: &ValueDeclaration,
    ) {
        let Some(fresh_ty) = self.result.of_value_group.get(&value_group_id).copied() else {
            unreachable!("impossible:");
        };
        let value_ty = if let Some(annotation) = value_declaration.annotation {
            let annotation_ty = self.lower_type(db, annotation);
            self.unify_types(db, fresh_ty, annotation_ty);
            annotation_ty
        } else {
            fresh_ty
        };
        for equation in &value_declaration.equations {
            for binder in &equation.binders {
                self.infer_binder(db, *binder);
            }
            let equation_ty = self.infer_binding(db, &equation.binding);
            self.unify_types(db, value_ty, equation_ty);
        }
    }

    fn infer_binder(&mut self, db: &dyn InferenceDatabase, binder_id: BinderId) -> CoreTypeId {
        let binder_ty = match &self.arena[binder_id] {
            Binder::Constructor { fields, .. } => {
                if let Some(constructor_resolution) =
                    self.resolve.per_constructor_binder.get(&binder_id)
                {
                    if let Some(constructor_ty) =
                        self.result.of_constructor.get(&constructor_resolution.constructor_id)
                    {
                        let constructor_ty = self.instantiate_type(db, *constructor_ty);
                        let arguments_ty = self.peel_arguments(db, constructor_ty);

                        for (field, argument_ty) in fields.iter().zip(arguments_ty) {
                            let field_ty = self.infer_binder(db, *field);
                            self.unify_types(db, field_ty, argument_ty);
                        }

                        constructor_ty
                    } else {
                        db.intern_type(CoreType::NotImplemented)
                    }
                } else {
                    db.intern_type(CoreType::NotImplemented)
                }
            }
            Binder::Literal(literal) => match literal {
                Literal::Array(_) => db.intern_type(CoreType::NotImplemented),
                Literal::Record(_) => db.intern_type(CoreType::NotImplemented),
                Literal::Int(_) => {
                    let name = db.interner().intern("Int");
                    db.intern_type(CoreType::Primitive(Name::from_raw(name)))
                }
                Literal::Number(_) => {
                    let name = db.interner().intern("Number");
                    db.intern_type(CoreType::Primitive(Name::from_raw(name)))
                }
                Literal::String(_) => {
                    let name = db.interner().intern("String");
                    db.intern_type(CoreType::Primitive(Name::from_raw(name)))
                }
                Literal::Char(_) => {
                    let name = db.interner().intern("Char");
                    db.intern_type(CoreType::Primitive(Name::from_raw(name)))
                }
                Literal::Boolean(_) => {
                    let name = db.interner().intern("Boolean");
                    db.intern_type(CoreType::Primitive(Name::from_raw(name)))
                }
            },
            Binder::Negative(negative) => {
                let name = match negative {
                    IntOrNumber::Int(_) => db.interner().intern("Int"),
                    IntOrNumber::Number(_) => db.interner().intern("Number"),
                };
                db.intern_type(CoreType::Primitive(Name::from_raw(name)))
            }
            Binder::Parenthesized(parenthesized) => self.infer_binder(db, *parenthesized),
            Binder::Variable(_) => self.fresh_unification(db),
            Binder::Wildcard => self.fresh_unification(db),
            Binder::NotImplemented => db.intern_type(CoreType::NotImplemented),
        };
        self.result.of_binder.insert(binder_id, binder_ty);
        binder_ty
    }

    fn infer_binding(&mut self, db: &dyn InferenceDatabase, binding: &Binding) -> CoreTypeId {
        match binding {
            Binding::Unconditional { where_expr } => {
                self.infer_let_bindings(db, &where_expr.let_bindings);
                self.infer_expr(db, where_expr.expr_id)
            }
        }
    }

    fn infer_let_bindings(&mut self, db: &dyn InferenceDatabase, let_bindings: &[LetBinding]) {
        // The compiler delimits let-names by let-patterns, as such, we
        // perform this grouping operation before we can analyze which
        // groups of let-names are mutually recursive. In the future,
        // let bindings _may_ be topologically sorted instead.

        #[derive(Debug, PartialEq, Eq)]
        enum GroupKey {
            Name,
            Pattern(BinderId),
        }

        let groups = let_bindings.iter().group_by(|let_binding| match let_binding {
            LetBinding::Name { .. } => GroupKey::Name,
            LetBinding::Pattern { binder, .. } => GroupKey::Pattern(*binder),
        });

        for (key, mut group) in groups.into_iter() {
            match key {
                GroupKey::Name => {
                    let let_names = group.map(|let_binding| {
                        if let LetBinding::Name { id } = let_binding {
                            id
                        } else {
                            unreachable!("impossible:");
                        }
                    });
                    let let_name_components =
                        recursive_let_names(self.arena, self.resolve, let_names);
                    for let_name_component in let_name_components {
                        for let_name_id in &let_name_component {
                            let fresh_ty = self.fresh_unification(db);
                            self.result.of_let_name.insert(*let_name_id, fresh_ty);
                        }
                        for let_name_id in let_name_component {
                            self.infer_let_name(db, let_name_id);
                        }
                    }
                }
                GroupKey::Pattern(_) => {
                    let Some(LetBinding::Pattern { binder, where_expr }) = group.next() else {
                        unreachable!("impossible:");
                    };
                    self.infer_binder(db, *binder);
                    self.infer_let_bindings(db, &where_expr.let_bindings);
                    self.infer_expr(db, where_expr.expr_id);
                }
            }
        }
    }

    fn infer_let_name(&mut self, db: &dyn InferenceDatabase, let_name_id: LetNameId) {
        let Some(fresh_ty) = self.result.of_let_name.get(&let_name_id).copied() else {
            unreachable!("impossible:");
        };
        let let_name = &self.arena[let_name_id];
        let let_name_ty = if let Some(annotation) = let_name.annotation {
            let annotation_ty = self.lower_type(db, annotation);
            self.unify_types(db, fresh_ty, annotation_ty);
            annotation_ty
        } else {
            fresh_ty
        };
        for equation in &let_name.equations {
            for binder in &equation.binders {
                self.infer_binder(db, *binder);
            }
            let equation_ty = self.infer_binding(db, &equation.binding);
            self.unify_types(db, let_name_ty, equation_ty);
        }
    }

    fn infer_expr(&mut self, db: &dyn InferenceDatabase, expr_id: ExprId) -> CoreTypeId {
        let expr_ty = match &self.arena[expr_id] {
            Expr::Application(_, _) => db.intern_type(CoreType::NotImplemented),
            Expr::Constructor(_) => {
                if let Some(constructor) = self.resolve.per_constructor_expr.get(&expr_id) {
                    if let Some(constructor_ty) =
                        self.result.of_constructor.get(&constructor.constructor_id)
                    {
                        *constructor_ty
                    } else {
                        db.intern_type(CoreType::NotImplemented)
                    }
                } else {
                    db.intern_type(CoreType::NotImplemented)
                }
            }
            Expr::Lambda(_, _) => db.intern_type(CoreType::NotImplemented),
            Expr::LetIn(_, _) => db.intern_type(CoreType::NotImplemented),
            Expr::Literal(literal) => match literal {
                Literal::Array(_) => db.intern_type(CoreType::NotImplemented),
                Literal::Record(_) => db.intern_type(CoreType::NotImplemented),
                Literal::Int(_) => {
                    let name = db.interner().intern("Int");
                    db.intern_type(CoreType::Primitive(Name::from_raw(name)))
                }
                Literal::Number(_) => {
                    let name = db.interner().intern("Number");
                    db.intern_type(CoreType::Primitive(Name::from_raw(name)))
                }
                Literal::String(_) => {
                    let name = db.interner().intern("String");
                    db.intern_type(CoreType::Primitive(Name::from_raw(name)))
                }
                Literal::Char(_) => {
                    let name = db.interner().intern("Char");
                    db.intern_type(CoreType::Primitive(Name::from_raw(name)))
                }
                Literal::Boolean(_) => {
                    let name = db.interner().intern("Boolean");
                    db.intern_type(CoreType::Primitive(Name::from_raw(name)))
                }
            },
            Expr::Variable(_) => {
                if let Some(variable) = self.resolve.per_variable_expr.get(&expr_id) {
                    let resolved_ty = match variable {
                        VariableResolution::Binder(binder_id) => {
                            self.result.of_binder.get(binder_id).copied()
                        }
                        VariableResolution::Imported(InFile { file_id, value }) => {
                            if let Some(result) = self.imported.get(file_id) {
                                result.of_value_group.get(value).copied()
                            } else {
                                None
                            }
                        }
                        VariableResolution::LetName(let_id) => {
                            self.result.of_let_name.get(let_id).copied()
                        }
                        VariableResolution::Local(value_id) => {
                            self.result.of_value_group.get(value_id).copied()
                        }
                    };
                    resolved_ty.unwrap_or_else(|| db.intern_type(CoreType::NotImplemented))
                } else {
                    db.intern_type(CoreType::NotImplemented)
                }
            }
            Expr::NotImplemented => db.intern_type(CoreType::NotImplemented),
        };
        self.result.of_expr.insert(expr_id, expr_ty);
        expr_ty
    }
}
