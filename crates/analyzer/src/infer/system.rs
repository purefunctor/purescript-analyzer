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
    infer::lower::LowerContext,
    names::{NameRef, Qualified},
    surface::{Binder, BinderId, Expr, ExprId, Literal},
    InferDatabase,
};

use super::{constraint::Constraint, Primitive, Type, TypeId, Unification};

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
            }
        }
    }

    pub(crate) fn infer_value_annotation_declaration_query(
        db: &'a dyn InferDatabase,
        id: InFile<AstId<ast::ValueAnnotationDeclaration>>,
    ) -> TypeId {
        let value_annotation_declaration_data = db.surface_value_annotation_declaration(id);
        let lower_context = LowerContext::new(db, &value_annotation_declaration_data.type_arena);
        lower_context.lower_type(value_annotation_declaration_data.ty)
    }

    fn fresh_unification(&mut self) -> TypeId {
        let index = self.infer_state.index;
        self.infer_state.index += 1;
        self.db.intern_type(Type::Unification(Unification(index, self.id)))
    }

    fn infer_expr(&mut self, expr_id: ExprId) -> TypeId {
        match &self.expr_arena[expr_id] {
            Expr::Application(_, _) => self.db.intern_type(Type::NotImplemented),
            Expr::Constructor(_) => self.db.intern_type(Type::NotImplemented),
            Expr::Lambda(_, _) => self.db.intern_type(Type::NotImplemented),
            Expr::LetIn(_, _) => self.db.intern_type(Type::NotImplemented),
            Expr::Literal(literal) => self.infer_expr_literal(literal),
            Expr::Variable(variable) => self.infer_expr_variable(variable),
        }
    }

    fn infer_expr_literal(&mut self, literal: &Literal<la_arena::Idx<Expr>>) -> TypeId {
        match literal {
            Literal::Array(_) => self.db.intern_type(Type::NotImplemented),
            Literal::Record(_) => self.db.intern_type(Type::NotImplemented),
            Literal::Int(_) => self.db.intern_type(Type::Primitive(Primitive::Int)),
            Literal::Number(_) => self.db.intern_type(Type::Primitive(Primitive::Number)),
            Literal::String(_) => self.db.intern_type(Type::Primitive(Primitive::String)),
            Literal::Char(_) => self.db.intern_type(Type::Primitive(Primitive::Char)),
            Literal::Boolean(_) => self.db.intern_type(Type::Primitive(Primitive::Boolean)),
        }
    }

    fn infer_expr_variable(&self, variable: &Qualified<NameRef>) -> TypeId {
        // FIXME: the following code is here to make it easier to implement the type checker
        let nominal_map = self.db.nominal_map(self.id.file_id);
        if let Some(group_id) = nominal_map.value_group_id(&variable.value) {
            self.db.intern_type(Type::Reference(group_id))
        } else {
            self.db.intern_type(Type::NotImplemented)
        }
    }
}
