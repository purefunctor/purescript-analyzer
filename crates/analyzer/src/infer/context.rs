use std::sync::Arc;

use itertools::Itertools;
use la_arena::Arena;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    resolver::ValueGroupId,
    scope::{ResolutionKind, ValueGroupResolutions},
    sugar::{BindingGroup, BindingGroupId},
    surface, InferDatabase,
};

use super::{
    constraint::Constraint, lower::LowerContext, InferBindingGroup, InferValueGroup, Primitive,
    Provenance, Type, TypeId, Unification,
};

// TYPES //

#[derive(Debug, Default)]
struct InferState {
    fresh_index: usize,
    constraints: Vec<Constraint>,
}

struct InferBindingGroupContext<'env, 'state> {
    db: &'env dyn InferDatabase,

    id: InFile<BindingGroupId>,
    infer_state: &'state mut InferState,
    of_value_group: FxHashMap<ValueGroupId, InferValueGroup>,
}

struct InferValueGroupContext<'env, 'state> {
    db: &'env dyn InferDatabase,

    expr_arena: &'env Arena<surface::Expr>,
    let_name_arena: &'env Arena<surface::LetName>,
    binder_arena: &'env Arena<surface::Binder>,
    type_arena: &'env Arena<surface::Type>,

    id: InFile<ValueGroupId>,
    resolutions: &'env ValueGroupResolutions,

    infer_state: &'state mut InferState,
    of_expr: FxHashMap<surface::ExprId, TypeId>,
    of_let_name: FxHashMap<surface::LetNameId, TypeId>,
    of_binder: FxHashMap<surface::BinderId, TypeId>,
}

// CONSTRUCTORS //

impl<'env, 'state> InferBindingGroupContext<'env, 'state> {
    fn new(
        db: &'env dyn InferDatabase,
        id: InFile<BindingGroupId>,
        infer_state: &'state mut InferState,
    ) -> InferBindingGroupContext<'env, 'state> {
        let of_value_group = FxHashMap::default();
        InferBindingGroupContext { db, id, infer_state, of_value_group }
    }
}

impl<'env, 'state> InferValueGroupContext<'env, 'state> {
    fn new(
        db: &'env dyn InferDatabase,
        id: InFile<ValueGroupId>,
        expr_arena: &'env Arena<surface::Expr>,
        let_name_arena: &'env Arena<surface::LetName>,
        binder_arena: &'env Arena<surface::Binder>,
        type_arena: &'env Arena<surface::Type>,
        resolutions: &'env ValueGroupResolutions,
        infer_state: &'state mut InferState,
    ) -> InferValueGroupContext<'env, 'state> {
        let of_expr = FxHashMap::default();
        let of_let_name = FxHashMap::default();
        let of_binder = FxHashMap::default();

        InferValueGroupContext {
            db,
            id,
            expr_arena,
            let_name_arena,
            binder_arena,
            type_arena,
            resolutions,
            of_expr,
            of_let_name,
            of_binder,
            infer_state,
        }
    }
}

// RULES //

impl<'env, 'state> InferBindingGroupContext<'env, 'state> {
    fn infer(&mut self, binding_group: &BindingGroup) {
        match &binding_group {
            BindingGroup::Singular(s) => {
                let id = InFile { file_id: self.id.file_id, value: *s };
                self.infer_value_group(id);
            }
            BindingGroup::Recursive(r) => {
                let id = InFile { file_id: self.id.file_id, value: *r };
                self.infer_value_group(id);
            }
            BindingGroup::MutuallyRecursive(m) => {
                m.iter().for_each(|m| {
                    let id = InFile { file_id: self.id.file_id, value: *m };
                    self.infer_value_group(id);
                });
            }
        }
    }

    fn infer_value_group(&mut self, id: InFile<ValueGroupId>) {
        let value_surface = self.db.value_surface(id);
        let value_resolutions = self.db.value_resolved(id);

        let mut context = InferValueGroupContext::new(
            self.db,
            id,
            &value_surface.expr_arena,
            &value_surface.let_name_arena,
            &value_surface.binder_arena,
            &value_surface.type_arena,
            &value_resolutions,
            self.infer_state,
        );

        let value_group_ty = context.infer(&value_surface.value);
        let infer_value_group = InferValueGroup::new(
            value_group_ty,
            context.of_expr,
            context.of_let_name,
            context.of_binder,
        );
        self.of_value_group.insert(id.value, infer_value_group);
    }
}

impl<'env, 'state> InferValueGroupContext<'env, 'state> {
    fn infer(&mut self, group: &surface::ValueGroup) -> TypeId {
        let annotation_ty =
            group.annotation.as_ref().map(|annotation| self.infer_annotation(annotation));

        let mut equations = group.equations.iter();

        let expected_ty = if let Some(annotation_ty) = annotation_ty {
            annotation_ty
        } else if let Some((id, equation)) = equations.next() {
            self.infer_equation(*id, equation)
        } else {
            unreachable!("invariant violated: group is empty");
        };

        equations.for_each(|(id, equation)| {
            self.check_equation(*id, equation, expected_ty);
        });

        expected_ty
    }

    fn infer_annotation(&mut self, annotation: &surface::ValueAnnotation) -> TypeId {
        LowerContext::new(self.db, self.type_arena).lower_type(annotation.ty)
    }

    fn infer_equation(
        &mut self,
        _: AstId<ast::ValueEquationDeclaration>,
        equation: &surface::ValueEquation,
    ) -> TypeId {
        self.infer_binders_binding(&equation.binders, &equation.binding)
    }

    fn check_equation(
        &mut self,
        _: AstId<ast::ValueEquationDeclaration>,
        _: &surface::ValueEquation,
        _: TypeId,
    ) {
    }
}

impl<'env, 'state> InferValueGroupContext<'env, 'state> {
    fn infer_binders_binding(
        &mut self,
        binders: &[surface::BinderId],
        binding: &surface::Binding,
    ) -> TypeId {
        let binders_ty =
            binders.iter().map(|binder_id| self.infer_binder(*binder_id)).collect_vec();
        let binding_ty = self.infer_binding(binding);
        binders_ty.into_iter().rev().fold(binding_ty, |result_ty, binder_ty| {
            self.db.intern_type(Type::Function(binder_ty, result_ty))
        })
    }

    fn infer_let_bindings(&mut self, let_bindings: &[surface::LetBinding]) {
        for let_binding in let_bindings.iter() {
            match let_binding {
                surface::LetBinding::Name { id } => {
                    let let_binding_groups = self.db.let_binding_groups(self.id);
                    if let_binding_groups.is_normal(*id) {
                        let let_name = &self.let_name_arena[*id];

                        let annotation_ty = let_name.annotation.as_ref().map(|annotation| {
                            LowerContext::new(self.db, self.type_arena).lower_type(annotation.ty)
                        });

                        let mut equations = let_name.equations.iter();

                        let expected_ty = if let Some(annotation_ty) = annotation_ty {
                            annotation_ty
                        } else if let Some(equation) = equations.next() {
                            self.infer_binders_binding(&equation.binders, &equation.binding)
                        } else {
                            unreachable!("invariant violated: let group is empty");
                        };

                        for _ in equations {
                            unimplemented!(
                                "Implement once checking and exhaustiveness is available."
                            );
                        }

                        self.of_let_name.insert(*id, expected_ty);
                    } else if let_binding_groups.is_recursive(*id) {
                        unimplemented!("Implement recursive type checking.");
                    } else {
                        unimplemented!("Implement mutually recursive type checking.");
                    }
                }
                surface::LetBinding::Pattern { .. } => {
                    unimplemented!("I don't know how to check patterns yet.")
                }
            }
        }
    }
}

impl<'env, 'state> InferValueGroupContext<'env, 'state> {
    fn fresh_unification(&mut self) -> TypeId {
        let index = self.infer_state.fresh_index as u32;
        self.infer_state.fresh_index += 1;
        self.db.intern_type(Type::Unification(Unification {
            index,
            provenance: Provenance::ValueGroup(self.id),
        }))
    }

    fn infer_binder(&mut self, binder_id: surface::BinderId) -> TypeId {
        let binder_ty = match &self.binder_arena[binder_id] {
            surface::Binder::Constructor { .. } => self.db.intern_type(Type::NotImplemented),
            surface::Binder::Literal(_) => self.db.intern_type(Type::NotImplemented),
            surface::Binder::Negative(_) => self.db.intern_type(Type::NotImplemented),
            surface::Binder::Parenthesized(p) => self.infer_binder(*p),
            surface::Binder::Variable(_) => self.fresh_unification(),
            surface::Binder::Wildcard => self.fresh_unification(),
        };
        self.of_binder.insert(binder_id, binder_ty);
        binder_ty
    }

    fn infer_binding(&mut self, binding: &surface::Binding) -> TypeId {
        match binding {
            surface::Binding::Unconditional { where_expr } => self.infer_where_expr(where_expr),
        }
    }

    fn infer_where_expr(&mut self, where_expr: &surface::WhereExpr) -> TypeId {
        self.infer_let_bindings(&where_expr.let_bindings);
        self.infer_expr(where_expr.expr_id)
    }

    fn infer_expr(&mut self, expr_id: surface::ExprId) -> TypeId {
        let expr_ty = match &self.expr_arena[expr_id] {
            surface::Expr::Application(_, _) => self.db.intern_type(Type::NotImplemented),
            surface::Expr::Constructor(_) => self.db.intern_type(Type::NotImplemented),
            surface::Expr::Lambda(_, _) => self.db.intern_type(Type::NotImplemented),
            surface::Expr::LetIn(let_bindings, let_body) => {
                self.infer_let_bindings(let_bindings);
                self.infer_expr(*let_body)
            }
            surface::Expr::Literal(literal) => self.infer_expr_literal(literal),
            surface::Expr::Variable(_) => self.infer_expr_variable(expr_id),
        };
        self.of_expr.insert(expr_id, expr_ty);
        expr_ty
    }

    fn infer_expr_literal(&mut self, literal: &surface::Literal<surface::ExprId>) -> TypeId {
        match literal {
            surface::Literal::Array(_) => self.db.intern_type(Type::NotImplemented),
            surface::Literal::Record(_) => self.db.intern_type(Type::NotImplemented),
            surface::Literal::Int(_) => self.db.intern_type(Type::Primitive(Primitive::Int)),
            surface::Literal::Number(_) => self.db.intern_type(Type::Primitive(Primitive::Number)),
            surface::Literal::String(_) => self.db.intern_type(Type::Primitive(Primitive::String)),
            surface::Literal::Char(_) => self.db.intern_type(Type::Primitive(Primitive::Char)),
            surface::Literal::Boolean(_) => {
                self.db.intern_type(Type::Primitive(Primitive::Boolean))
            }
        }
    }

    fn infer_expr_variable(&mut self, expr_id: surface::ExprId) -> TypeId {
        self.resolutions
            .get(expr_id)
            .and_then(|resolution| match resolution.kind {
                ResolutionKind::Binder(b) => self.of_binder.get(&b).copied(),
                ResolutionKind::LetName(l) => self.of_let_name.get(&l).copied(),
                ResolutionKind::Global(reference) => {
                    let current = self.id;

                    // If we're in a local resolution...
                    if current.file_id == reference.file_id {
                        let binding_groups = self.db.binding_groups(current.file_id);
                        let current_binding_group = binding_groups.binding_group_id(current.value);
                        let reference_binding_group =
                            binding_groups.binding_group_id(reference.value);
                        if current_binding_group == reference_binding_group {
                            // Ideally, this unification variable should remain
                            // the same for all uses of `g`. We should probably
                            // float binding_groups up so we don't call into the
                            // db multiple times.
                            Some(self.fresh_unification())
                        } else {
                            let infer_binding_group =
                                self.db.infer_binding_group(reference_binding_group);
                            Some(
                                infer_binding_group
                                    .of_value_group
                                    .get(&reference.value)
                                    .unwrap()
                                    .ty,
                            )
                        }
                    } else {
                        None
                    }
                }
            })
            .unwrap_or_else(|| self.db.intern_type(Type::NotImplemented))
    }
}

// QUERIES //

pub(crate) fn infer_binding_group_query(
    db: &dyn InferDatabase,
    id: InFile<BindingGroupId>,
) -> Arc<InferBindingGroup> {
    let mut infer_state = InferState::default();
    let mut context = InferBindingGroupContext::new(db, id, &mut infer_state);

    let binding_groups = db.binding_groups(id.file_id);
    context.infer(&binding_groups[id.value]);

    Arc::new(InferBindingGroup::new(context.of_value_group, infer_state.constraints))
}
