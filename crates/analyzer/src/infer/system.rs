//! Implements the PureScript type system.

/*

What we learned:
1. We deal with cyclic inference in value declarations by using unification
   variables as stand-ins for type information about other value declarations.
   This is done whether or not the declarations themselves are recursive, since
   the promise is that the constraint solver algorithm would be able to work on
   them just fine.
2. We created a smaller representation for types used in the inference algorithm,
   as we've learned that the approach of using different arenas per query wouldn't
   quite work with an algorithm as global as type inference. Instead, we make use
   of salsa's interning mechanism to still have ID-based ASTs and make them global
   across the inference queries.

What we should know:
1. Unification variables cannot unify with polymorphic (forall) types as that
   introduces impredicativity into the type system. While PureScript allows
   impredicativity to some extent, it only does so because there are no
   safeguards against it (to my knowledge)
2. We should introduce a new `Type` variant called `Deferred`, which acts similar
   to a unification variable but for obtaining type information on-demand.

What we should answer:
1. Does our approach of deferring inference for types cause duplication of work
   in the constraint solver? Traditionally, inference for mutually recursive
   declarations are done through binding groups, which effectively means that
   operations are batched.

   The deferred approach in the analyzer may mean that constraints generated
   between mutually recursive bindings would have to be processed twice.
   Fortunately, the idea behind constraint solving is built around the idea
   of progressing as more information becomes available.

   With this in mind, inference queries can be made to do "best-effort"
   constraint-solving to eliminate any constraints that are immediately
   solvable, while retaining any residual ones. Meanwhile, constraints
   which require global dependencies, like demanding type information
   through `Deferred` can be solved in a separate step.

   Residual constraints that require global dependencies can be cached
   by putting them in an inference query that works at the module level.
   As mentioned, inference queries such as `infer_value_declaration` are
   able to solve fully local constraints to completion; this affords us
   caching at the local level.

   Meanwhile, an inference query for an entire module allows us to partially
   or completely solve constraints, caching the work for the editor to then
   consume and render for the user.
*/

use la_arena::Arena;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    surface::{Binder, BinderId, Expr, ExprId},
    InferDatabase,
};

use super::{constraint::Constraint, Type, TypeId};

/// Mutable internal state used during inference.
#[derive(Debug, Default)]
struct InferState {
    constraints: Vec<Constraint>,
    index: u32,
    type_per_expr: FxHashMap<ExprId, TypeId>,
    type_per_binder: FxHashMap<BinderId, TypeId>,
}

/// Inference context for value declarations.
pub(crate) struct InferValueDeclarationContext<'a> {
    db: &'a dyn InferDatabase,
    id: InFile<AstId<ast::ValueDeclaration>>,
    expr_arena: &'a Arena<Expr>,
    binder_arena: &'a Arena<Binder>,
    infer_state: InferState,
}

impl<'a> InferValueDeclarationContext<'a> {
    pub(crate) fn infer_value_declaration_query(
        db: &'a dyn InferDatabase,
        id: InFile<AstId<ast::ValueDeclaration>>,
    ) -> () {
        let value_declaration_data = db.surface_value_declaration(id);
        let mut context = InferValueDeclarationContext {
            db,
            id,
            expr_arena: &value_declaration_data.expr_arena,
            binder_arena: &value_declaration_data.binder_arena,
            infer_state: InferState::default(),
        };
        match &value_declaration_data.binding {
            crate::surface::Binding::Unconditional { where_expr } => {
                dbg!(db.lookup_intern_type(context.infer_expr(where_expr.expr_id)));
            },
        }
    }

    fn fresh_unification(&mut self) -> TypeId {
        let index = self.infer_state.index;
        self.infer_state.index += 1;
        self.db.intern_type(Type::Unification(index, self.id))
    }

    fn infer_expr(&mut self, expr_id: ExprId) -> TypeId {
        match &self.expr_arena[expr_id] {
            Expr::Application(_, _) => self.db.intern_type(Type::NotImplemented),
            Expr::Constructor(_) => self.db.intern_type(Type::NotImplemented),
            Expr::LetIn(_, _) => self.db.intern_type(Type::NotImplemented),
            Expr::Literal(_) => self.db.intern_type(Type::NotImplemented),
            Expr::Variable(_) => self.db.intern_type(Type::NotImplemented),
        }
    }
}

// use la_arena::Arena;
// use rustc_hash::FxHashMap;
// use syntax::ast;

// use crate::{
//     id::{AstId, InFile},
//     surface::{
//         self,
//         visitor::{default_visit_expr, Visitor},
//         Binder, BinderId, Expr, ExprId,
//     },
// };

// use super::{constraint::Constraint, trees, InferDatabase, Type, TypeId, Unification};

// pub(crate) struct InferValueDeclarationContext<'a> {
//     db: &'a dyn InferDatabase,
//     id: InFile<AstId<ast::ValueDeclaration>>,

//     expr_arena: &'a Arena<Expr>,
//     binder_arena: &'a Arena<Binder>,

//     constraints: Vec<Constraint>,
//     fresh_index: u32,
//     type_per_expr: FxHashMap<ExprId, TypeId>,
//     type_per_binder: FxHashMap<BinderId, TypeId>,
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct InferValueDeclarationResult {
//     constraints: Vec<Constraint>,
//     type_per_expr: FxHashMap<ExprId, TypeId>,
//     type_per_binder: FxHashMap<BinderId, TypeId>,
// }

// impl<'a> InferValueDeclarationContext<'a> {
//     pub(crate) fn new(
//         db: &'a dyn InferDatabase,
//         id: InFile<AstId<ast::ValueDeclaration>>,
//         expr_arena: &'a Arena<Expr>,
//         binder_arena: &'a Arena<Binder>,
//     ) -> InferValueDeclarationContext<'a> {
//         let constraints = vec![];
//         let fresh_index = 0;
//         let type_per_expr = FxHashMap::default();
//         let type_per_binder = FxHashMap::default();
//         InferValueDeclarationContext {
//             db,
//             id,
//             expr_arena,
//             binder_arena,
//             constraints,
//             fresh_index,
//             type_per_expr,
//             type_per_binder,
//         }
//     }

//     fn fresh_unification(&mut self) -> TypeId {
//         let index = self.fresh_index;
//         self.fresh_index += 1;
//         self.db.intern_type(Type::Unification(Unification::Local(index, self.id)))
//     }

//     pub(crate) fn infer_expr(&mut self, expr_id: ExprId) -> TypeId {
//         let expr_ty = match &self.expr_arena[expr_id] {
//             Expr::Application(function, arguments) => {
//                 let function_ty = self.infer_expr(*function);
//                 let arguments_ty =
//                     arguments.iter().map(|argument| self.infer_expr(*argument)).collect();

//                 let result_ty = self.fresh_unification();
//                 let synthesized_ty = self.db.intern_type(Type::Function(arguments_ty, result_ty));

//                 self.unify(function_ty, synthesized_ty);

//                 self.db.intern_type(Type::NotImplemented)
//             }
//             Expr::Constructor(constructor) => {
//                 // FIXME: Resolve against other types of declarations...
//                 if let Some(id) =
//                     self.db.nominal_map(self.id.file_id).get_foreign_data(&constructor.value)
//                 {
//                     self.db.infer_foreign_data_query(id)
//                 } else {
//                     self.db.intern_type(Type::NotImplemented)
//                 }
//             }
//             Expr::LetIn(_, _) => self.db.intern_type(Type::NotImplemented),
//             Expr::Literal(literal) => {
//                 let literal = match literal {
//                     surface::Literal::Array(_) => {
//                         default_visit_expr(self, expr_id);
//                         Type::NotImplemented
//                     }
//                     surface::Literal::Record(_) => {
//                         default_visit_expr(self, expr_id);
//                         Type::NotImplemented
//                     }
//                     surface::Literal::Int(_) => Type::Literal(trees::Literal::Int),
//                     surface::Literal::Number(_) => Type::Literal(trees::Literal::Number),
//                     surface::Literal::String(_) => Type::Literal(trees::Literal::String),
//                     surface::Literal::Char(_) => Type::Literal(trees::Literal::Char),
//                     surface::Literal::Boolean(_) => Type::Literal(trees::Literal::Boolean),
//                 };
//                 self.db.intern_type(literal)
//             }
//             Expr::Variable(_) => self.db.intern_type(Type::NotImplemented),
//         };
//         self.type_per_expr.insert(expr_id, expr_ty);
//         expr_ty
//     }

//     fn unify(&mut self, t: TypeId, u: TypeId) {
//         UnifyContext { db: self.db, constraints: &mut self.constraints }.unify(t, u);
//     }

//     pub(crate) fn into_result(self) -> InferValueDeclarationResult {
//         InferValueDeclarationResult {
//             constraints: self.constraints,
//             type_per_expr: self.type_per_expr,
//             type_per_binder: self.type_per_binder,
//         }
//     }
// }

// impl<'a> Visitor<'a> for InferValueDeclarationContext<'a> {
//     fn expr_arena(&self) -> &'a Arena<Expr> {
//         self.expr_arena
//     }

//     fn binder_arena(&self) -> &'a Arena<Binder> {
//         self.binder_arena
//     }

//     fn visit_expr(&mut self, expr_id: ExprId) {
//         self.infer_expr(expr_id);
//     }
// }

// struct UnifyContext<'a> {
//     db: &'a dyn InferDatabase,
//     constraints: &'a mut Vec<Constraint>,
// }

// impl<'a> UnifyContext<'a> {
//     fn unify(&mut self, t: TypeId, u: TypeId) {
//         match dbg!(self.db.lookup_intern_type(t), self.db.lookup_intern_type(u)) {
//             // Deep
//             (Type::Unification(t_unification), Type::Unification(u_unification)) => {
//                 if t_unification != u_unification {
//                     self.constraints.push(Constraint::UnifyDeep(t, u));
//                 }
//             }
//             // Solve
//             (_, Type::Unification(u_unification)) => {
//                 self.constraints.push(Constraint::UnifySolve(u_unification, t));
//             }
//             (Type::Unification(t_unification), _) => {
//                 self.constraints.push(Constraint::UnifySolve(t_unification, u));
//             }
//             _ => {}
//         }
//     }
// }
