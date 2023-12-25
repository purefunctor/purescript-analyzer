//! Implements inference for value declarations.

use std::{iter, sync::Arc};

use itertools::Itertools;
use la_arena::Arena;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    infer::{BindingGroupTypes, Primitive, Provenance, Type, TypeId, ValueGroupTypes},
    resolver::ValueGroupId,
    scope::{ConstructorResolution, Resolutions, VariableResolutionKind},
    sugar::{BindingGroup, BindingGroupId, BindingGroups, LetBindingGroups},
    surface, InferDatabase,
};

use super::{
    instantiate::instantiate_type, lower::lower_type, solve::SolveContext,
    substitute::ApplySubstitution, unify::unify_types, InferState,
};

struct ValueGroupArenas<'env> {
    expr_arena: &'env Arena<surface::Expr>,
    let_name_arena: &'env Arena<surface::LetName>,
    binder_arena: &'env Arena<surface::Binder>,
    type_arena: &'env Arena<surface::Type>,
}

struct ValueGroupEnv<'env> {
    binding_groups: &'env BindingGroups,
    let_binding_groups: &'env LetBindingGroups,
    of_sibling: &'env FxHashMap<ValueGroupId, TypeId>,
    resolutions: &'env Resolutions,
}

struct InferBindingGroupContext<'env, 'state> {
    db: &'env dyn InferDatabase,
    id: InFile<BindingGroupId>,

    infer_state: &'state mut InferState,
    of_value_group: FxHashMap<ValueGroupId, ValueGroupTypes>,
}

struct InferValueGroupContext<'env, 'state> {
    db: &'env dyn InferDatabase,
    id: InFile<ValueGroupId>,

    value_arenas: ValueGroupArenas<'env>,
    value_env: ValueGroupEnv<'env>,

    infer_state: &'state mut InferState,
    of_expr: FxHashMap<surface::ExprId, TypeId>,
    of_let_name: FxHashMap<surface::LetNameId, TypeId>,
    of_binder: FxHashMap<surface::BinderId, TypeId>,
}

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
        value_arenas: ValueGroupArenas<'env>,
        value_env: ValueGroupEnv<'env>,
        infer_state: &'state mut InferState,
    ) -> InferValueGroupContext<'env, 'state> {
        let of_expr = FxHashMap::default();
        let of_let_name = FxHashMap::default();
        let of_binder = FxHashMap::default();

        InferValueGroupContext {
            db,
            id,

            value_arenas,
            value_env,

            infer_state,
            of_expr,
            of_let_name,
            of_binder,
        }
    }
}

impl<'env, 'state> InferBindingGroupContext<'env, 'state> {
    fn fresh_unification(&mut self, id: InFile<ValueGroupId>) -> TypeId {
        self.infer_state.fresh_unification(self.db, Provenance::ValueGroup(id))
    }

    fn infer(&mut self, binding_group: &BindingGroup) {
        match &binding_group {
            BindingGroup::Singular(s) => {
                let id = InFile { file_id: self.id.file_id, value: *s };
                let of_sibling = FxHashMap::default();
                self.infer_value_group(id, &of_sibling);
            }
            BindingGroup::Recursive(r) => {
                let id = InFile { file_id: self.id.file_id, value: *r };
                let of_r = self.fresh_unification(id);
                let of_sibling = iter::once((*r, of_r)).collect();
                self.infer_value_group(id, &of_sibling);
            }
            BindingGroup::MutuallyRecursive(m) => {
                let of_sibling = m
                    .iter()
                    .map(|m| {
                        let id = InFile { file_id: self.id.file_id, value: *m };
                        let of_m = self.fresh_unification(id);
                        (*m, of_m)
                    })
                    .collect();
                m.iter().for_each(|m| {
                    let id = InFile { file_id: self.id.file_id, value: *m };
                    self.infer_value_group(id, &of_sibling);
                });
            }
        }
    }

    fn infer_value_group(
        &mut self,
        id: InFile<ValueGroupId>,
        of_sibling: &FxHashMap<ValueGroupId, TypeId>,
    ) {
        let value_surface = self.db.value_surface(id);
        let value_resolutions = self.db.value_resolutions(id);
        let binding_groups = self.db.binding_groups(id.file_id);
        let let_binding_groups = self.db.let_binding_groups(id);

        let value_arenas = ValueGroupArenas {
            expr_arena: &value_surface.expr_arena,
            let_name_arena: &value_surface.let_name_arena,
            binder_arena: &value_surface.binder_arena,
            type_arena: &value_surface.type_arena,
        };

        let value_env = ValueGroupEnv {
            binding_groups: &binding_groups,
            let_binding_groups: &let_binding_groups,
            of_sibling,
            resolutions: &value_resolutions,
        };

        let mut context =
            InferValueGroupContext::new(self.db, id, value_arenas, value_env, self.infer_state);

        let value_group_ty = context.infer(&value_surface.value);
        let infer_value_group = ValueGroupTypes::new(
            value_group_ty,
            context.of_expr,
            context.of_let_name,
            context.of_binder,
        );
        self.of_value_group.insert(id.value, infer_value_group);
    }
}

impl<'env, 'state> InferValueGroupContext<'env, 'state> {
    fn fresh_unification(&mut self) -> TypeId {
        self.infer_state.fresh_unification(self.db, Provenance::ValueGroup(self.id))
    }

    fn instantiate_type(&mut self, type_id: TypeId) -> TypeId {
        instantiate_type(self.db, self.infer_state, Provenance::ValueGroup(self.id), type_id)
    }

    fn unify_types(&mut self, x_id: TypeId, y_id: TypeId) {
        unify_types(self.db, self.infer_state, x_id, y_id)
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
        lower_type(self.db, self.value_arenas.type_arena, self.value_env.resolutions, annotation.ty)
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
                    let let_binding_groups = self.value_env.let_binding_groups;
                    if let_binding_groups.is_normal(*id) {
                        let let_name = &self.value_arenas.let_name_arena[*id];

                        let annotation_ty = let_name.annotation.as_ref().map(|annotation| {
                            lower_type(
                                self.db,
                                self.value_arenas.type_arena,
                                self.value_env.resolutions,
                                annotation.ty,
                            )
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
    fn infer_binder(&mut self, binder_id: surface::BinderId) -> TypeId {
        let binder_ty = match &self.value_arenas.binder_arena[binder_id] {
            surface::Binder::Constructor { .. } => self.db.intern_type(Type::NotImplemented),
            surface::Binder::Literal(l) => self.infer_binder_literal(l),
            surface::Binder::Negative(n) => self.infer_binder_negative(n),
            surface::Binder::Parenthesized(p) => self.infer_binder(*p),
            surface::Binder::Variable(_) => self.fresh_unification(),
            surface::Binder::Wildcard => self.fresh_unification(),
        };
        self.of_binder.insert(binder_id, binder_ty);
        binder_ty
    }

    fn infer_binder_literal(&mut self, literal: &surface::Literal<surface::BinderId>) -> TypeId {
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

    fn infer_binder_negative(&mut self, negative: &surface::IntOrNumber) -> TypeId {
        match negative {
            surface::IntOrNumber::Int(_) => self.db.intern_type(Type::Primitive(Primitive::Int)),
            surface::IntOrNumber::Number(_) => {
                self.db.intern_type(Type::Primitive(Primitive::Number))
            }
        }
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
        let expr_ty = match &self.value_arenas.expr_arena[expr_id] {
            surface::Expr::Application(function, arguments) => {
                self.infer_expr_application(*function, arguments)
            }
            surface::Expr::Constructor(_) => self.infer_expr_constructor(expr_id),
            surface::Expr::Lambda(_, _) => self.db.intern_type(Type::NotImplemented),
            surface::Expr::LetIn(let_bindings, let_body) => {
                self.infer_expr_let_in(let_bindings, *let_body)
            }
            surface::Expr::Literal(literal) => self.infer_expr_literal(literal),
            surface::Expr::Variable(_) => self.infer_expr_variable(expr_id),
        };
        self.of_expr.insert(expr_id, expr_ty);
        expr_ty
    }

    fn infer_expr_application(
        &mut self,
        function: surface::ExprId,
        arguments: &[surface::ExprId],
    ) -> TypeId {
        let function_ty = self.infer_expr(function);
        let function_ty = self.instantiate_type(function_ty);

        let arguments_ty = arguments
            .iter()
            .map(|argument| {
                let argument_ty = self.infer_expr(*argument);
                self.instantiate_type(argument_ty)
            })
            .collect_vec();

        let result_ty = self.fresh_unification();
        let auxiliary_ty =
            arguments_ty.into_iter().rev().fold(result_ty, |result_ty, argument_ty| {
                self.db.intern_type(Type::Function(argument_ty, result_ty))
            });

        self.unify_types(function_ty, auxiliary_ty);

        result_ty
    }

    fn infer_expr_constructor(&mut self, expr_id: surface::ExprId) -> TypeId {
        self.value_env
            .resolutions
            .get_constructor_expr(expr_id)
            .map(|ConstructorResolution { data_id, constructor_id }| {
                let data_types = self.db.infer_data_group(data_id);
                data_types.get_constructor(constructor_id)
            })
            .unwrap_or_else(|| self.db.intern_type(Type::NotImplemented))
    }

    fn infer_expr_let_in(
        &mut self,
        let_bindings: &[surface::LetBinding],
        let_body: surface::ExprId,
    ) -> TypeId {
        self.infer_let_bindings(let_bindings);
        self.infer_expr(let_body)
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
        self.value_env
            .resolutions
            .get_variable(expr_id)
            .and_then(|resolution| match resolution.kind {
                VariableResolutionKind::Binder(b) => self.of_binder.get(&b).copied(),
                VariableResolutionKind::LetName(l) => self.of_let_name.get(&l).copied(),
                VariableResolutionKind::Local(l) => {
                    if let Some(t) = self.value_env.of_sibling.get(&l) {
                        Some(*t)
                    } else {
                        let binding_group = self.value_env.binding_groups.binding_group_id(l);
                        let binding_group_ty = self.db.infer_binding_group(binding_group);
                        Some(binding_group_ty.get(l).of_value_group)
                    }
                }
            })
            .unwrap_or_else(|| self.db.intern_type(Type::NotImplemented))
    }
}

pub(crate) fn infer_binding_group_query(
    db: &dyn InferDatabase,
    id: InFile<BindingGroupId>,
) -> Arc<BindingGroupTypes> {
    let mut infer_state = InferState::default();
    let mut context = InferBindingGroupContext::new(db, id, &mut infer_state);

    let binding_groups = db.binding_groups(id.file_id);
    context.infer(&binding_groups[id.value]);

    let mut solver = SolveContext::new(db, context.infer_state);
    solver.solve();

    let of_value_group = context.of_value_group;
    let unification_solved = solver.unification_solved;
    let constraints = infer_state.constraints;

    let mut substitutions = unification_solved.clone();
    substitutions.apply_substitution(db, &unification_solved);

    let mut binding_group_types = BindingGroupTypes::new(of_value_group, constraints);
    binding_group_types.apply_substitution(db, &substitutions);

    Arc::new(binding_group_types)
}
