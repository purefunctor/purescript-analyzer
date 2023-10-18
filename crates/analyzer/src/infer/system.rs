//! Implements the PureScript type system.
use la_arena::Arena;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    surface::{
        self,
        visitor::{default_visit_expr, Visitor},
        Binder, BinderId, Expr, ExprId,
    },
};

use super::{constraint::Constraint, trees, InferDatabase, Type, TypeId, Unification};

pub(crate) struct InferValueDeclarationContext<'a> {
    db: &'a dyn InferDatabase,
    id: InFile<AstId<ast::ValueDeclaration>>,

    expr_arena: &'a Arena<Expr>,
    binder_arena: &'a Arena<Binder>,

    constraints: Vec<Constraint>,
    fresh_index: u32,
    type_per_expr: FxHashMap<ExprId, TypeId>,
    type_per_binder: FxHashMap<BinderId, TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferValueDeclarationResult {
    constraints: Vec<Constraint>,
    type_per_expr: FxHashMap<ExprId, TypeId>,
    type_per_binder: FxHashMap<BinderId, TypeId>,
}

impl<'a> InferValueDeclarationContext<'a> {
    pub(crate) fn new(
        db: &'a dyn InferDatabase,
        id: InFile<AstId<ast::ValueDeclaration>>,
        expr_arena: &'a Arena<Expr>,
        binder_arena: &'a Arena<Binder>,
    ) -> InferValueDeclarationContext<'a> {
        let constraints = vec![];
        let fresh_index = 0;
        let type_per_expr = FxHashMap::default();
        let type_per_binder = FxHashMap::default();
        InferValueDeclarationContext {
            db,
            id,
            expr_arena,
            binder_arena,
            constraints,
            fresh_index,
            type_per_expr,
            type_per_binder,
        }
    }

    fn fresh_unification(&mut self) -> TypeId {
        let index = self.fresh_index;
        self.fresh_index += 1;
        self.db.intern_type(Type::Unification(Unification::Local(index, self.id)))
    }

    pub(crate) fn infer_expr(&mut self, expr_id: ExprId) -> TypeId {
        let expr_ty = match &self.expr_arena[expr_id] {
            Expr::Application(function, arguments) => {
                let function_ty = self.infer_expr(*function);
                let arguments_ty =
                    arguments.iter().map(|argument| self.infer_expr(*argument)).collect();

                let result_ty = self.fresh_unification();
                let synthesized_ty = self.db.intern_type(Type::Function(arguments_ty, result_ty));

                self.unify(function_ty, synthesized_ty);

                self.db.intern_type(Type::NotImplemented)
            }
            Expr::Constructor(constructor) => {
                // FIXME: Resolve against other types of declarations...
                if let Some(id) =
                    self.db.nominal_map(self.id.file_id).get_foreign_data(&constructor.value)
                {
                    self.db.infer_foreign_data_query(id)
                } else {
                    self.db.intern_type(Type::NotImplemented)
                }
            }
            Expr::LetIn(_, _) => self.db.intern_type(Type::NotImplemented),
            Expr::Literal(literal) => {
                let literal = match literal {
                    surface::Literal::Array(_) => {
                        default_visit_expr(self, expr_id);
                        Type::NotImplemented
                    }
                    surface::Literal::Record(_) => {
                        default_visit_expr(self, expr_id);
                        Type::NotImplemented
                    }
                    surface::Literal::Int(_) => Type::Literal(trees::Literal::Int),
                    surface::Literal::Number(_) => Type::Literal(trees::Literal::Number),
                    surface::Literal::String(_) => Type::Literal(trees::Literal::String),
                    surface::Literal::Char(_) => Type::Literal(trees::Literal::Char),
                    surface::Literal::Boolean(_) => Type::Literal(trees::Literal::Boolean),
                };
                self.db.intern_type(literal)
            }
            Expr::Variable(_) => self.db.intern_type(Type::NotImplemented),
        };
        self.type_per_expr.insert(expr_id, expr_ty);
        expr_ty
    }

    fn unify(&mut self, t: TypeId, u: TypeId) {
        UnifyContext { db: self.db, constraints: &mut self.constraints }.unify(t, u);
    }

    pub(crate) fn into_result(self) -> InferValueDeclarationResult {
        InferValueDeclarationResult {
            constraints: self.constraints,
            type_per_expr: self.type_per_expr,
            type_per_binder: self.type_per_binder,
        }
    }
}

impl<'a> Visitor<'a> for InferValueDeclarationContext<'a> {
    fn expr_arena(&self) -> &'a Arena<Expr> {
        self.expr_arena
    }

    fn binder_arena(&self) -> &'a Arena<Binder> {
        self.binder_arena
    }

    fn visit_expr(&mut self, expr_id: ExprId) {
        self.infer_expr(expr_id);
    }
}

struct UnifyContext<'a> {
    db: &'a dyn InferDatabase,
    constraints: &'a mut Vec<Constraint>,
}

impl<'a> UnifyContext<'a> {
    fn unify(&mut self, t: TypeId, u: TypeId) {
        match dbg!(self.db.lookup_intern_type(t), self.db.lookup_intern_type(u)) {
            // Deep
            (Type::Unification(t_unification), Type::Unification(u_unification)) => {
                if t_unification != u_unification {
                    self.constraints.push(Constraint::UnifyDeep(t, u));
                }
            }
            // Solve
            (_, Type::Unification(u_unification)) => {
                self.constraints.push(Constraint::UnifySolve(u_unification, t));
            }
            (Type::Unification(t_unification), _) => {
                self.constraints.push(Constraint::UnifySolve(t_unification, u));
            }
            _ => {}
        }
    }
}
