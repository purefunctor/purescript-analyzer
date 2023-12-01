use std::sync::Arc;

use files::FileId;
use itertools::Itertools;
use la_arena::Arena;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    resolver::ValueGroupId,
    scope::{ResolutionKind, ValueGroupRecursiveLets, ValueGroupResolutions},
    surface, InferDatabase,
};

use super::{
    constraint::Constraint, lower::LowerContext, Primitive, Provenance, Type, TypeId, Unification,
};

// TYPES //

struct Context<'db, T> {
    db: &'db dyn InferDatabase,
    inner: T,
}

struct ValueGroupCtx<'ctx> {
    expr_arena: &'ctx Arena<surface::Expr>,
    let_name_arena: &'ctx Arena<surface::LetName>,
    binder_arena: &'ctx Arena<surface::Binder>,
    type_arena: &'ctx Arena<surface::Type>,

    file_id: FileId,
    id: ValueGroupId,
    resolutions: &'ctx ValueGroupResolutions,
    recursive_lets: &'ctx ValueGroupRecursiveLets,

    fresh_index: usize,
    constraints: Vec<Constraint>,
    of_expr: FxHashMap<surface::ExprId, TypeId>,
    of_let_name: FxHashMap<surface::LetNameId, TypeId>,
    of_binder: FxHashMap<surface::BinderId, TypeId>,
}

// CONSTRUCTORS //

impl<'db, T> Context<'db, T> {
    fn new(db: &'db dyn InferDatabase, inner: T) -> Context<'db, T> {
        Context { db, inner }
    }
}

impl<'ctx> ValueGroupCtx<'ctx> {
    fn new(
        id: InFile<ValueGroupId>,
        group_data: &'ctx surface::WithArena<surface::ValueGroup>,
        resolutions: &'ctx ValueGroupResolutions,
        recursive_lets: &'ctx ValueGroupRecursiveLets,
    ) -> ValueGroupCtx<'ctx> {
        let fresh_index = 0;
        let constraints = vec![];
        let of_expr = FxHashMap::default();
        let of_let_name = FxHashMap::default();
        let of_binder = FxHashMap::default();

        ValueGroupCtx {
            expr_arena: &group_data.expr_arena,
            let_name_arena: &group_data.let_name_arena,
            binder_arena: &group_data.binder_arena,
            type_arena: &group_data.type_arena,
            file_id: id.file_id,
            id: id.value,
            resolutions,
            recursive_lets,
            fresh_index,
            constraints,
            of_expr,
            of_let_name,
            of_binder,
        }
    }
}

// RULES //

impl Context<'_, ValueGroupCtx<'_>> {
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
                    if self.inner.recursive_lets.is_normal(*id) {
                        let let_name = &self.inner.let_name_arena[*id];

                        let annotation_ty = let_name.annotation.as_ref().map(|annotation| {
                            LowerContext::new(self.db, self.inner.type_arena)
                                .lower_type(annotation.ty)
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

                        self.inner.of_let_name.insert(*id, expected_ty);
                    } else if self.inner.recursive_lets.is_recursive(*id) {
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

impl Context<'_, ValueGroupCtx<'_>> {
    fn infer_value(&mut self, group: &surface::ValueGroup) -> TypeId {
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
        LowerContext::new(self.db, self.inner.type_arena).lower_type(annotation.ty)
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

impl Context<'_, ValueGroupCtx<'_>> {
    fn fresh_unification(&mut self) -> TypeId {
        let index = self.inner.fresh_index as u32;
        self.inner.fresh_index += 1;
        self.db.intern_type(Type::Unification(Unification {
            index,
            provenance: Provenance::ValueGroup(InFile {
                file_id: self.inner.file_id,
                value: self.inner.id,
            }),
        }))
    }

    fn infer_binder(&mut self, binder_id: surface::BinderId) -> TypeId {
        let binder_ty = match &self.inner.binder_arena[binder_id] {
            surface::Binder::Constructor { .. } => self.db.intern_type(Type::NotImplemented),
            surface::Binder::Literal(_) => self.db.intern_type(Type::NotImplemented),
            surface::Binder::Negative(_) => self.db.intern_type(Type::NotImplemented),
            surface::Binder::Parenthesized(p) => self.infer_binder(*p),
            surface::Binder::Variable(_) => self.fresh_unification(),
            surface::Binder::Wildcard => self.fresh_unification(),
        };
        self.inner.of_binder.insert(binder_id, binder_ty);
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
        let expr_ty = match &self.inner.expr_arena[expr_id] {
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
        self.inner.of_expr.insert(expr_id, expr_ty);
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
        self.inner
            .resolutions
            .get(expr_id)
            .and_then(|resolution| match resolution.kind {
                ResolutionKind::Binder(b) => self.inner.of_binder.get(&b).copied(),
                ResolutionKind::LetName(l) => self.inner.of_let_name.get(&l).copied(),
                ResolutionKind::Global(g) => Some(self.db.intern_type(Type::Reference(g))),
            })
            .unwrap_or_else(|| self.db.intern_type(Type::NotImplemented))
    }
}

// QUERIES //

pub(crate) fn infer_value_query(
    db: &dyn InferDatabase,
    id: InFile<ValueGroupId>,
) -> (TypeId, Arc<Vec<Constraint>>) {
    let group_data = db.value_surface(id);
    let resolutions = db.value_resolved(id);
    let recursive_lets = db.value_recursive_lets(id);
    let mut context =
        Context::new(db, ValueGroupCtx::new(id, &group_data, &resolutions, &recursive_lets));

    let value_ty = context.infer_value(&group_data.value);
    let constraints = context.inner.constraints;

    (value_ty, Arc::new(constraints))
}
