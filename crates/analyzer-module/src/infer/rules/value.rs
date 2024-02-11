use crate::{surface::tree::*, InferenceDatabase};

use super::{CoreType, CoreTypeId, InferContext};

impl InferContext<'_> {
    pub(super) fn infer_value_declaration(
        &mut self,
        db: &dyn InferenceDatabase,
        value_declaration: &ValueDeclaration,
    ) {
        value_declaration.equations.iter().for_each(|value_equation| {
            value_equation.binders.iter().for_each(|binder| {
                self.infer_binder(db, *binder);
            });
            self.infer_binding(db, &value_equation.binding);
        });
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

    fn infer_binding(&mut self, db: &dyn InferenceDatabase, _binding: &Binding) {
        match _binding {
            Binding::Unconditional { where_expr } => {
                self.infer_let_bindings(db, &where_expr.let_bindings);
                self.infer_expr(db, where_expr.expr_id);
            }
        }
    }

    fn infer_let_bindings(&mut self, db: &dyn InferenceDatabase, let_bindings: &[LetBinding]) {}

    fn infer_expr(&mut self, db: &dyn InferenceDatabase, expr_id: ExprId) {}
}
