//! Implements the PureScript type system.
use la_arena::Arena;

use crate::{id::InFile, resolver::ValueGroupId, surface, InferDatabase};

use super::{lower::LowerContext, Provenance, Type, TypeId, Unification};

#[derive(Debug, Default)]
struct InferState {
    index: u32,
}

pub(crate) struct InferContext<'a> {
    db: &'a dyn InferDatabase,
    expr_arena: &'a Arena<surface::Expr>,
    binder_arena: &'a Arena<surface::Binder>,
    type_arena: &'a Arena<surface::Type>,
    infer_state: InferState,
}

impl<'a> InferContext<'a> {
    fn new(
        db: &'a dyn InferDatabase,
        expr_arena: &'a Arena<surface::Expr>,
        binder_arena: &'a Arena<surface::Binder>,
        type_arena: &'a Arena<surface::Type>,
    ) -> InferContext<'a> {
        let infer_state = InferState::default();
        InferContext { db, expr_arena, binder_arena, type_arena, infer_state }
    }

    pub(crate) fn infer_value_query(db: &'a dyn InferDatabase, id: InFile<ValueGroupId>) -> TypeId {
        let group_data = db.value_surface(id);
        let mut infer_context = InferContext::new(
            db,
            &group_data.expr_arena,
            &group_data.binder_arena,
            &group_data.type_arena,
        );

        let annotation_ty = group_data
            .value
            .annotation
            .as_ref()
            .map(|annotation| infer_context.infer_value_annotation(annotation));

        let mut equations = group_data.value.equations.values();

        let expected_ty = if let Some(annotation_ty) = annotation_ty {
            annotation_ty
        } else if let Some(equation) = equations.next() {
            infer_context.infer_value_equation(equation)
        } else {
            unreachable!("Empty value group!");
        };

        equations.for_each(|equation| infer_context.check_value_equation(equation, expected_ty));

        expected_ty
    }
}

impl<'a> InferContext<'a> {
    fn fresh_unification(&mut self, provenance: Provenance) -> TypeId {
        let index = self.infer_state.index;
        self.infer_state.index += 1;
        self.db.intern_type(Type::Unification(Unification { index, provenance }))
    }

    fn infer_value_annotation(&mut self, annotation: &surface::ValueAnnotation) -> TypeId {
        LowerContext::new(self.db, self.type_arena).lower_type(annotation.ty)
    }

    fn infer_value_equation(&mut self, equation: &surface::ValueEquation) -> TypeId {
        self.infer_binding(&equation.binding)
    }

    fn check_value_equation(&mut self, equation: &surface::ValueEquation, expected_ty: TypeId) {}

    fn infer_binding(&mut self, binding: &surface::Binding) -> TypeId {
        match binding {
            surface::Binding::Unconditional { where_expr } => self.infer_where_expr(where_expr),
        }
    }

    fn infer_where_expr(&mut self, where_expr: &surface::WhereExpr) -> TypeId {
        self.infer_expr(where_expr.expr_id)
    }

    fn infer_expr(&mut self, expr_id: surface::ExprId) -> TypeId {
        self.db.intern_type(Type::NotImplemented)
    }
}
