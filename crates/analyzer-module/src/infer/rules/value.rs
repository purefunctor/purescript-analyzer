use itertools::Itertools;

use crate::{
    id::InFile, index::nominal::ValueGroupId, infer::Hint, scope::VariableResolution,
    surface::tree::*, InferenceDatabase,
};

use super::{recursive_let_names, CoreType, CoreTypeId, InferContext};

impl InferContext<'_> {
    pub(super) fn infer_value_scc(
        &mut self,
        db: &dyn InferenceDatabase,
        value_declarations: &[(ValueGroupId, &ValueDeclaration)],
    ) {
        for (value_group_id, value_declaration) in value_declarations {
            let value_ty = if let Some(value_annotation) = value_declaration.annotation {
                self.lower_type(db, value_annotation)
            } else {
                self.fresh_unification(db)
            };
            self.state.infer_map.of_value_group.insert(*value_group_id, value_ty);
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
        self.add_hint(Hint::ValueGroup(value_group_id));
        let Some(value_ty) = self.state.infer_map.of_value_group.get(&value_group_id).copied()
        else {
            unreachable!("impossible: caller must insert a type!");
        };

        let checking = value_declaration.annotation.is_some();

        if checking {
            let value_ty = self.instantiate_type(db, value_ty);
            for equation in &value_declaration.equations {
                self.check_equation(db, equation, value_ty);
            }
        } else {
            for equation in &value_declaration.equations {
                self.infer_equation(db, equation, value_ty);
            }
        }
        self.pop_hint();
    }

    fn check_equation(
        &mut self,
        db: &dyn InferenceDatabase,
        equation: &ValueEquation,
        value_ty: CoreTypeId,
    ) {
        let (arguments_ty, result_ty) = self.peel_arguments(db, value_ty);
        for (binder_id, argument_ty) in equation.binders.iter().zip(arguments_ty) {
            self.check_binder(db, *binder_id, argument_ty)
        }
        self.check_binding(db, &equation.binding, result_ty);
    }

    fn infer_equation(
        &mut self,
        db: &dyn InferenceDatabase,
        equation: &ValueEquation,
        value_ty: CoreTypeId,
    ) {
        let binders_ty =
            equation.binders.iter().map(|binder| self.infer_binder(db, *binder)).collect_vec();
        let binding_ty = self.infer_binding(db, &equation.binding);
        let equation_ty = binders_ty.into_iter().rev().fold(binding_ty, |binding_ty, binder_ty| {
            db.intern_type(CoreType::Function(binder_ty, binding_ty))
        });
        self.unify_types(db, value_ty, equation_ty);
    }

    fn infer_binder(&mut self, db: &dyn InferenceDatabase, binder_id: BinderId) -> CoreTypeId {
        let binder_ty = match &self.arena[binder_id] {
            Binder::Constructor { fields, .. } => {
                if let Some(constructor_resolution) =
                    self.resolve.per_constructor_binder.get(&binder_id)
                {
                    if let Some(constructor_ty) = self
                        .state
                        .infer_map
                        .of_constructor
                        .get(&constructor_resolution.constructor_id)
                    {
                        let constructor_ty = self.instantiate_type(db, *constructor_ty);
                        let (arguments_ty, _) = self.peel_arguments(db, constructor_ty);

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
        self.state.infer_map.of_binder.insert(binder_id, binder_ty);
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
                            self.state.infer_map.of_let_name.insert(*let_name_id, fresh_ty);
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
        let Some(fresh_ty) = self.state.infer_map.of_let_name.get(&let_name_id).copied() else {
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
        self.add_hint(Hint::Expression(expr_id));
        let expr_ty = self.infer_expr_core(db, expr_id);
        self.pop_hint();
        expr_ty
    }

    fn infer_expr_core(&mut self, db: &dyn InferenceDatabase, expr_id: ExprId) -> CoreTypeId {
        let expr_ty = match &self.arena[expr_id] {
            Expr::Application(function, arguments) => {
                let function_ty = self.infer_expr_core(db, *function);
                let function_ty = self.instantiate_type(db, function_ty);

                let arguments_ty = arguments
                    .iter()
                    .map(|argument| self.infer_expr_core(db, *argument))
                    .collect_vec();

                let result_ty = self.fresh_unification(db);
                let auxiliary_ty =
                    arguments_ty.into_iter().rev().fold(result_ty, |result_ty, argument_ty| {
                        db.intern_type(CoreType::Function(argument_ty, result_ty))
                    });

                self.unify_types(db, function_ty, auxiliary_ty);

                result_ty
            }
            Expr::Constructor(_) => {
                if let Some(constructor) = self.resolve.per_constructor_expr.get(&expr_id) {
                    if let Some(constructor_ty) =
                        self.state.infer_map.of_constructor.get(&constructor.constructor_id)
                    {
                        *constructor_ty
                    } else {
                        db.intern_type(CoreType::NotImplemented)
                    }
                } else {
                    db.intern_type(CoreType::NotImplemented)
                }
            }
            Expr::Lambda(binders, body) => {
                let binders_ty =
                    binders.iter().map(|binder| self.infer_binder(db, *binder)).collect_vec();
                let body_ty = self.infer_expr_core(db, *body);
                binders_ty.into_iter().rev().fold(body_ty, |body_ty, binder_ty| {
                    db.intern_type(CoreType::Function(binder_ty, body_ty))
                })
            }
            Expr::LetIn(bindings, body) => {
                self.infer_let_bindings(db, bindings);
                self.infer_expr_core(db, *body)
            }
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
                            self.state.infer_map.of_binder.get(binder_id).copied()
                        }
                        VariableResolution::Imported(InFile { file_id, value }) => {
                            if let Some(result) = self.imported.get(file_id) {
                                result.of_value_group.get(value).copied()
                            } else {
                                None
                            }
                        }
                        VariableResolution::LetName(let_id) => {
                            self.state.infer_map.of_let_name.get(let_id).copied()
                        }
                        VariableResolution::Local(value_id) => {
                            self.state.infer_map.of_value_group.get(value_id).copied()
                        }
                    };
                    resolved_ty.unwrap_or_else(|| db.intern_type(CoreType::NotImplemented))
                } else {
                    db.intern_type(CoreType::NotImplemented)
                }
            }
            Expr::NotImplemented => db.intern_type(CoreType::NotImplemented),
        };
        self.state.infer_map.of_expr.insert(expr_id, expr_ty);
        expr_ty
    }
}

impl InferContext<'_> {
    fn check_binder(
        &mut self,
        db: &dyn InferenceDatabase,
        binder_id: BinderId,
        expected_ty: CoreTypeId,
    ) {
        let assign_expected = |this: &mut Self| {
            this.state.infer_map.of_binder.insert(binder_id, expected_ty);
        };

        let assign_error = |this: &mut Self| {
            this.state
                .infer_map
                .of_binder
                .insert(binder_id, db.intern_type(CoreType::NotImplemented));
        };

        let check_literal = |this: &mut Self, name: &str| {
            let name = Name::from_raw(db.interner().intern(name));
            if let CoreType::Primitive(primitive) = db.lookup_intern_type(expected_ty) {
                if primitive == name {
                    assign_expected(this);
                } else {
                    assign_error(this);
                }
            }
        };

        match &self.arena[binder_id] {
            Binder::Constructor { fields, .. } => {
                if let Some(constructor) = self.resolve.per_constructor_binder.get(&binder_id) {
                    if let CoreType::Constructor(expected_data_id) =
                        db.lookup_intern_type(expected_ty)
                    {
                        if constructor.file_id != expected_data_id.file_id
                            || constructor.data_id != expected_data_id.value
                        {
                            return assign_error(self);
                        }
                    }

                    if let Some(constructor_ty) =
                        self.state.infer_map.of_constructor.get(&constructor.constructor_id)
                    {
                        let constructor_ty = self.instantiate_type(db, *constructor_ty);
                        let (arguments_ty, result_ty) = self.peel_arguments(db, constructor_ty);

                        for (field, argument_ty) in fields.iter().zip(arguments_ty) {
                            self.check_binder(db, *field, argument_ty);
                        }

                        self.unify_types(db, result_ty, expected_ty);
                        self.state.infer_map.of_binder.insert(binder_id, result_ty);
                    } else {
                        assign_error(self);
                    }
                } else {
                    assign_error(self);
                }
            }
            Binder::Literal(literal) => match literal {
                Literal::Array(_) => todo!("check_binder(Array)"),
                Literal::Record(_) => todo!("check_binder(Record)"),
                Literal::Int(_) => check_literal(self, "Int"),
                Literal::Number(_) => check_literal(self, "Number"),
                Literal::String(_) => check_literal(self, "String"),
                Literal::Char(_) => check_literal(self, "Char"),
                Literal::Boolean(_) => check_literal(self, "Boolean"),
            },
            Binder::Negative(negative) => match negative {
                IntOrNumber::Int(_) => check_literal(self, "Int"),
                IntOrNumber::Number(_) => check_literal(self, "Number"),
            },
            Binder::Parenthesized(parenthesized) => {
                self.check_binder(db, *parenthesized, expected_ty);
            }
            Binder::Variable(_) => assign_expected(self),
            Binder::Wildcard => assign_expected(self),
            Binder::NotImplemented => assign_expected(self),
        }
    }

    fn check_binding(
        &mut self,
        db: &dyn InferenceDatabase,
        binding: &Binding,
        expected_ty: CoreTypeId,
    ) {
        match binding {
            Binding::Unconditional { where_expr } => {
                self.infer_let_bindings(db, &where_expr.let_bindings);
                self.check_expr(db, where_expr.expr_id, expected_ty);
            }
        }
    }

    fn check_expr(&mut self, db: &dyn InferenceDatabase, expr_id: ExprId, expected_ty: CoreTypeId) {
        self.add_hint(Hint::Expression(expr_id));
        self.check_expr_core(db, expr_id, expected_ty);
        self.pop_hint();
    }

    fn check_expr_core(
        &mut self,
        db: &dyn InferenceDatabase,
        expr_id: ExprId,
        expected_ty: CoreTypeId,
    ) {
        let assign_error = |this: &mut Self| {
            this.state.infer_map.of_expr.insert(expr_id, db.intern_type(CoreType::NotImplemented));
        };

        let check_literal = |this: &mut Self, name: &str| {
            let name = Name::from_raw(db.interner().intern(name));
            if let CoreType::Primitive(primitive) = db.lookup_intern_type(expected_ty) {
                if primitive == name {
                    this.state.infer_map.of_expr.insert(expr_id, expected_ty);
                } else {
                    assign_error(this);
                }
            }
        };

        match &self.arena[expr_id] {
            Expr::Application(_, _) => todo!("check_expr(Application)"),
            Expr::Constructor(_) => {
                if let Some(constructor) = self.resolve.per_constructor_expr.get(&expr_id) {
                    if let Some(constructor_ty) = self
                        .state
                        .infer_map
                        .of_constructor
                        .get(&constructor.constructor_id)
                        .copied()
                    {
                        self.subsume_types(db, constructor_ty, expected_ty);
                        self.state.infer_map.of_expr.insert(expr_id, expected_ty);
                    } else {
                        assign_error(self);
                    }
                } else {
                    assign_error(self);
                }
            }
            Expr::Lambda(_, _) => todo!("check_expr(Lambda)"),
            Expr::LetIn(bindings, body) => {
                self.infer_let_bindings(db, bindings);
                self.check_expr_core(db, *body, expected_ty);
            }
            Expr::Literal(literal) => match literal {
                Literal::Array(_) => todo!("check_expr(Array)"),
                Literal::Record(_) => todo!("check_expr(Record"),
                Literal::Int(_) => check_literal(self, "Int"),
                Literal::Number(_) => check_literal(self, "Number"),
                Literal::String(_) => check_literal(self, "String"),
                Literal::Char(_) => check_literal(self, "Char"),
                Literal::Boolean(_) => check_literal(self, "Boolean"),
            },
            Expr::Variable(_) => {
                if let Some(variable) = self.resolve.per_variable_expr.get(&expr_id) {
                    let variable_ty = match variable {
                        VariableResolution::Binder(binder_id) => {
                            self.state.infer_map.of_binder.get(binder_id)
                        }
                        VariableResolution::Imported(InFile { file_id, value }) => {
                            if let Some(result) = self.imported.get(file_id) {
                                result.of_value_group.get(value)
                            } else {
                                None
                            }
                        }
                        VariableResolution::LetName(let_id) => {
                            self.state.infer_map.of_let_name.get(let_id)
                        }
                        VariableResolution::Local(local_id) => {
                            self.state.infer_map.of_value_group.get(local_id)
                        }
                    };
                    let variable_ty = variable_ty
                        .copied()
                        .unwrap_or_else(|| db.intern_type(CoreType::NotImplemented));
                    self.subsume_types(db, variable_ty, expected_ty);
                    self.state.infer_map.of_expr.insert(expr_id, expected_ty);
                } else {
                    assign_error(self);
                }
            }
            Expr::NotImplemented => {
                self.state.infer_map.of_expr.insert(expr_id, expected_ty);
            }
        }
    }
}
