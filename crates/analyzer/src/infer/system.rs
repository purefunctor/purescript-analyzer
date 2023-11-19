//! Implements the PureScript type system.
use la_arena::Arena;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    resolver::ValueGroupId,
    scope::{ResolutionKind, ValueGroupResolutions},
    surface, InferDatabase,
};

use super::{
    constraint::Constraint, lower::LowerContext, Primitive, Provenance, Type, TypeId, Unification,
};

#[derive(Debug, Default)]
struct InferState {
    constraints: Vec<Constraint>,
    index: u32,
}

pub(crate) struct InferContext<'a> {
    // Environment
    db: &'a dyn InferDatabase,
    expr_arena: &'a Arena<surface::Expr>,
    binder_arena: &'a Arena<surface::Binder>,
    type_arena: &'a Arena<surface::Type>,
    resolutions: &'a ValueGroupResolutions,
    // Accumulators
    of_expr: FxHashMap<surface::ExprId, TypeId>,
    of_binder: FxHashMap<surface::BinderId, TypeId>,
    of_let_name: FxHashMap<surface::LetNameId, TypeId>,
    // State
    infer_state: InferState,
    provenance: Provenance,
    on_equation_id: Option<AstId<ast::ValueEquationDeclaration>>,
}

impl<'a> InferContext<'a> {
    fn new(
        db: &'a dyn InferDatabase,
        expr_arena: &'a Arena<surface::Expr>,
        binder_arena: &'a Arena<surface::Binder>,
        type_arena: &'a Arena<surface::Type>,
        resolutions: &'a ValueGroupResolutions,
        provenance: Provenance,
    ) -> InferContext<'a> {
        let infer_state = InferState::default();
        let of_expr = FxHashMap::default();
        let of_binder = FxHashMap::default();
        let of_let_name = FxHashMap::default();
        let on_equation_id = None;
        InferContext {
            db,
            expr_arena,
            binder_arena,
            type_arena,
            resolutions,
            of_expr,
            of_binder,
            of_let_name,
            infer_state,
            provenance,
            on_equation_id,
        }
    }

    pub(crate) fn infer_value_query(db: &'a dyn InferDatabase, id: InFile<ValueGroupId>) -> TypeId {
        let group_data = db.value_surface(id);
        let provenance = Provenance::ValueGroup(id);
        let resolutions = db.value_resolved(id);
        let mut infer_context = InferContext::new(
            db,
            &group_data.expr_arena,
            &group_data.binder_arena,
            &group_data.type_arena,
            &resolutions,
            provenance,
        );

        let annotation_ty = group_data
            .value
            .annotation
            .as_ref()
            .map(|annotation| infer_context.infer_value_annotation(annotation));

        let mut equations = group_data.value.equations.iter();

        let expected_ty = if let Some(annotation_ty) = annotation_ty {
            annotation_ty
        } else if let Some((equation_id, equation_ast)) = equations.next() {
            infer_context.on_equation_id = Some(*equation_id);
            infer_context.infer_value_equation(equation_ast)
        } else {
            unreachable!("invariant violated: no annotation and empty equations")
        };

        equations.for_each(|(_, equation_ast)| {
            infer_context.check_value_equation(equation_ast, expected_ty)
        });

        dbg!(infer_context.of_expr);
        dbg!(infer_context.of_binder);
        dbg!(infer_context.of_let_name);

        expected_ty
    }
}

impl<'a> InferContext<'a> {
    fn fresh_unification(&mut self) -> TypeId {
        let index = self.infer_state.index;
        let provenance = self.provenance;
        self.infer_state.index += 1;
        self.db.intern_type(Type::Unification(Unification { index, provenance }))
    }

    fn infer_value_annotation(&mut self, annotation: &surface::ValueAnnotation) -> TypeId {
        LowerContext::new(self.db, self.type_arena).lower_type(annotation.ty)
    }

    fn infer_value_equation(&mut self, equation: &surface::ValueEquation) -> TypeId {
        let binders_ty: Vec<_> =
            equation.binders.iter().map(|binder_id| self.infer_binder(*binder_id)).collect();
        let binding_ty = self.infer_binding(&equation.binding);

        binders_ty.into_iter().rev().fold(binding_ty, |result_ty, binder_ty| {
            self.db.intern_type(Type::Function(binder_ty, result_ty))
        })
    }

    fn infer_binding(&mut self, binding: &surface::Binding) -> TypeId {
        match binding {
            surface::Binding::Unconditional { where_expr } => self.infer_where_expr(where_expr),
        }
    }

    fn infer_where_expr(&mut self, where_expr: &surface::WhereExpr) -> TypeId {
        self.infer_expr(where_expr.expr_id)
    }

    fn infer_expr(&mut self, expr_id: surface::ExprId) -> TypeId {
        let expr_ty = match &self.expr_arena[expr_id] {
            surface::Expr::Application(_, _) => self.db.intern_type(Type::NotImplemented),
            surface::Expr::Constructor(_) => self.db.intern_type(Type::NotImplemented),
            surface::Expr::Lambda(_, _) => self.db.intern_type(Type::NotImplemented),
            surface::Expr::LetIn(_, _) => self.db.intern_type(Type::NotImplemented),
            surface::Expr::Literal(literal) => self.infer_expr_literal(literal),
            surface::Expr::Variable(_) => self.infer_expr_variable(expr_id),
        };
        self.of_expr.insert(expr_id, expr_ty);
        expr_ty
    }

    fn infer_expr_literal(&self, literal: &surface::Literal<surface::ExprId>) -> TypeId {
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

    fn infer_expr_variable(&self, expr_id: surface::ExprId) -> TypeId {
        let equation_id = self
            .on_equation_id
            .unwrap_or_else(|| unreachable!("invariant violated: caller must set this"));

        if let Some(resolution) = &self.resolutions.get(equation_id, expr_id) {
            let variable_ty = match resolution {
                ResolutionKind::Binder(i) => self.of_binder.get(i).copied(),
                ResolutionKind::LetName(i) => self.of_let_name.get(i).copied(),
                ResolutionKind::Global(i) => Some(self.db.intern_type(Type::Reference(*i))),
            };
            variable_ty.unwrap_or(self.db.intern_type(Type::NotImplemented))
        } else {
            self.db.intern_type(Type::NotImplemented)
        }
    }

    fn infer_binder(&mut self, binder_id: surface::BinderId) -> TypeId {
        let binder_ty = match &self.binder_arena[binder_id] {
            surface::Binder::Constructor { .. } => self.db.intern_type(Type::NotImplemented),
            surface::Binder::Literal(literal) => self.infer_binder_literal(literal),
            surface::Binder::Negative(negative) => self.infer_binder_negative(negative),
            surface::Binder::Parenthesized(binder_id) => self.infer_binder(*binder_id),
            surface::Binder::Variable(_) => self.infer_binder_variable(),
            surface::Binder::Wildcard => self.infer_binder_wildcard(),
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

    fn infer_binder_variable(&mut self) -> TypeId {
        self.fresh_unification()
    }

    fn infer_binder_wildcard(&mut self) -> TypeId {
        self.fresh_unification()
    }
}

impl<'a> InferContext<'a> {
    fn check_value_equation(&mut self, equation: &surface::ValueEquation, expected_ty: TypeId) {
        let mut arguments_expected_ty = vec![];
        let mut binding_expected_ty = expected_ty;

        while let Type::Function(argument_ty, result_ty) =
            self.db.lookup_intern_type(binding_expected_ty)
        {
            arguments_expected_ty.push(argument_ty);
            binding_expected_ty = result_ty;
        }

        if equation.binders.len() != arguments_expected_ty.len() {
            panic!("Arity mismatch! Make sure we emit an error for this.");
        }

        equation.binders.iter().zip(arguments_expected_ty.iter()).for_each(
            |(binder, argument_expected_ty)| {
                self.check_binder(*binder, *argument_expected_ty);
            },
        );

        self.check_binding(&equation.binding, binding_expected_ty);
    }

    fn check_binding(&mut self, _: &surface::Binding, _: TypeId) {}

    fn check_binder(&mut self, binder_id: surface::BinderId, expected_ty: TypeId) {
        let binder_ty = self.infer_binder(binder_id);
        self.unify(binder_ty, expected_ty);
        // FIXME: This should introduce variables to the local
        // environment, but only after all binders are introduced.
        // Ultimately, what this means is that this rule should
        // return a pair of name to type id which should be
        // introduced by the upstream rule calling it.
        // Alternatively, we can also implement this rule
        // in terms of "binders" as a whole, rather than
        // checking an individual binder.
    }
}

impl<'a> InferContext<'a> {
    fn unify(&mut self, x: TypeId, y: TypeId) {
        let x_t = self.db.lookup_intern_type(x);
        let y_t = self.db.lookup_intern_type(y);
        match (x_t, y_t) {
            (Type::Unification(x_u), Type::Unification(y_u)) => {
                if x_u != y_u {
                    self.infer_state.constraints.push(Constraint::UnifyDeep(x, y));
                }
            }
            (Type::Unification(x_u), _) => {
                self.infer_state.constraints.push(Constraint::UnifySolve(x_u, y));
            }
            (_, Type::Unification(y_u)) => {
                self.infer_state.constraints.push(Constraint::UnifySolve(y_u, x));
            }
            (x_t, y_t) => {
                dbg!(x_t, y_t);
            }
        }
    }
}

// The idea with the `infer` and `check` rules is that they bind
// types to IDs like `ExprId = TypeId` or `BinderId = TypeId`.
// What we want is a transaction going from local variables to
// types; a transition from `SmolStr` -> `BinderId` -> `TypeId`.
// Currently, our mechanism for scopes only takes note of which
// variables are introduced by which syntactic constructs. We
// do not have a way for the desired transaction yet.
//
// The question then, is:
//
// How should we accomodate introducing local variables while
// type checking? Should we implement scoping in the type system,
// by maintaining a vector of hashmaps of names to type ids, or,
// should we rely on the scope to be able to map names to IDs
// which they ultimately came from.
//
// To begin, we can change the scope data to associate names
// to IDs rather than just store names. However, for constructs
// like let bindings, we would have to also change their surface
// representation to make use of IDs as well. The idea being that
// we want to assign them IDs which can be used to relate to other
// data. One reason that we use arena allocation for expressions
// or binders or types is that it makes their recursive definition
// much easier to work with, which is not necessarily the case for
// let bindings, but it affords us the ability to query their types.
