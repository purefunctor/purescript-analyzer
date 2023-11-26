use files::FileId;
use itertools::Itertools;
use la_arena::Arena;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    resolver::ValueGroupId,
    scope::ValueGroupResolutions,
    surface, InferDatabase,
};

use super::{constraint::Constraint, lower::LowerContext, Type, TypeId};

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

    fresh_index: usize,
    constraints: Vec<Constraint>,
    of_expr: FxHashMap<surface::ExprId, TypeId>,
    of_let_name: FxHashMap<surface::LetNameId, TypeId>,
    of_binder: FxHashMap<surface::BinderId, TypeId>,
}

struct ValueEquationCtx<'parent, 'ctx> {
    parent: &'parent mut ValueGroupCtx<'ctx>,
    id: AstId<ast::ValueEquationDeclaration>,
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
            fresh_index,
            constraints,
            of_expr,
            of_let_name,
            of_binder,
        }
    }
}

impl<'parent, 'ctx> ValueEquationCtx<'parent, 'ctx> {
    fn new(
        parent: &'parent mut ValueGroupCtx<'ctx>,
        id: AstId<ast::ValueEquationDeclaration>,
    ) -> ValueEquationCtx<'parent, 'ctx> {
        ValueEquationCtx { parent, id }
    }
}

// RULES //

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
        id: AstId<ast::ValueEquationDeclaration>,
        equation: &surface::ValueEquation,
    ) -> TypeId {
        Context::new(self.db, ValueEquationCtx::new(&mut self.inner, id)).infer(equation)
    }

    fn check_equation(
        &mut self,
        id: AstId<ast::ValueEquationDeclaration>,
        equation: &surface::ValueEquation,
        expected: TypeId,
    ) {
        Context::new(self.db, ValueEquationCtx::new(&mut self.inner, id)).check(equation, expected)
    }
}

impl Context<'_, ValueEquationCtx<'_, '_>> {
    fn infer(&mut self, equation: &surface::ValueEquation) -> TypeId {
        let binders_ty =
            equation.binders.iter().map(|binder_id| self.infer_binder(*binder_id)).collect_vec();
        let binding_ty = self.infer_binding(&equation.binding);
        binders_ty.into_iter().rev().fold(binding_ty, |result_ty, binder_ty| {
            self.db.intern_type(Type::Function(binder_ty, result_ty))
        })
    }

    fn infer_binder(&mut self, binder_id: surface::BinderId) -> TypeId {
        let binder_ty = match &self.inner.parent.binder_arena[binder_id] {
            surface::Binder::Constructor { .. } => self.db.intern_type(Type::NotImplemented),
            surface::Binder::Literal(_) => self.db.intern_type(Type::NotImplemented),
            surface::Binder::Negative(_) => self.db.intern_type(Type::NotImplemented),
            surface::Binder::Parenthesized(_) => self.db.intern_type(Type::NotImplemented),
            surface::Binder::Variable(_) => self.db.intern_type(Type::NotImplemented),
            surface::Binder::Wildcard => self.db.intern_type(Type::NotImplemented),
        };
        self.inner.parent.of_binder.insert(binder_id, binder_ty);
        binder_ty
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
        let expr_ty = match &self.inner.parent.expr_arena[expr_id] {
            surface::Expr::Application(_, _) => self.db.intern_type(Type::NotImplemented),
            surface::Expr::Constructor(_) => self.db.intern_type(Type::NotImplemented),
            surface::Expr::Lambda(_, _) => self.db.intern_type(Type::NotImplemented),
            surface::Expr::LetIn(_, _) => self.db.intern_type(Type::NotImplemented),
            surface::Expr::Literal(_) => self.db.intern_type(Type::NotImplemented),
            surface::Expr::Variable(_) => self.db.intern_type(Type::NotImplemented),
        };
        self.inner.parent.of_expr.insert(expr_id, expr_ty);
        expr_ty
    }

    fn check(&mut self, _: &surface::ValueEquation, _: TypeId) {}
}

// QUERIES //

pub(crate) fn infer_value_query(db: &dyn InferDatabase, id: InFile<ValueGroupId>) -> TypeId {
    let group_data = db.value_surface(id);
    let resolutions = db.value_resolved(id);
    let mut context = Context::new(db, ValueGroupCtx::new(id, &group_data, &resolutions));
    context.infer_value(&group_data.value)
}
