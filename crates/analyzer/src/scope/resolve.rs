//! Implements the name resolution algorithm.

use la_arena::Arena;
use std::sync::Arc;

use rustc_hash::FxHashMap;

use crate::{
    id::InFile,
    resolver::{DataGroupId, NominalMap, ValueGroupId},
    scope::{BinderKind, ScopeKind, VariableResolution},
    surface::{
        visitor::{default_visit_binder, default_visit_expr, default_visit_type, Visitor},
        Binder, BinderId, Expr, ExprId, LetName, Type, TypeId,
    },
    ScopeDatabase,
};

use super::{
    ConstructorResolution, Resolutions, TypeResolution, ValueGroupScope, VariableResolutionKind,
    WithScope,
};

pub(crate) struct ResolveContext<'a> {
    expr_arena: &'a Arena<Expr>,
    let_name_arena: &'a Arena<LetName>,
    binder_arena: &'a Arena<Binder>,
    type_arena: &'a Arena<Type>,
    value_scope: &'a WithScope<ValueGroupScope>,
    nominal_map: &'a NominalMap,
    per_constructor_expr: FxHashMap<ExprId, ConstructorResolution>,
    per_constructor_binder: FxHashMap<BinderId, ConstructorResolution>,
    per_type_type: FxHashMap<TypeId, TypeResolution>,
    per_variable: FxHashMap<ExprId, VariableResolution>,
}

impl<'a> ResolveContext<'a> {
    pub(crate) fn new(
        expr_arena: &'a Arena<Expr>,
        let_name_arena: &'a Arena<LetName>,
        binder_arena: &'a Arena<Binder>,
        type_arena: &'a Arena<Type>,
        value_scope: &'a WithScope<ValueGroupScope>,
        nominal_map: &'a NominalMap,
    ) -> ResolveContext<'a> {
        let per_constructor_expr = FxHashMap::default();
        let per_constructor_binder = FxHashMap::default();
        let per_type_type = FxHashMap::default();
        let per_variable = FxHashMap::default();
        ResolveContext {
            expr_arena,
            let_name_arena,
            binder_arena,
            type_arena,
            value_scope,
            nominal_map,
            per_constructor_expr,
            per_constructor_binder,
            per_type_type,
            per_variable,
        }
    }

    pub(crate) fn data_resolutions_query(
        db: &dyn ScopeDatabase,
        id: InFile<DataGroupId>,
    ) -> Arc<Resolutions> {
        let data_surface = db.data_surface(id);
        let value_scope = Default::default();
        let nominal_map = db.nominal_map(id.file_id);

        let mut resolve_context = ResolveContext::new(
            &data_surface.expr_arena,
            &data_surface.let_name_arena,
            &data_surface.binder_arena,
            &data_surface.type_arena,
            &value_scope,
            &nominal_map,
        );

        data_surface.value.declaration.constructors.iter().for_each(|(_, constructor)| {
            constructor.fields.iter().for_each(|field| {
                resolve_context.visit_type(*field);
            });
        });

        Arc::new(Resolutions::new(
            resolve_context.per_constructor_expr,
            resolve_context.per_constructor_binder,
            resolve_context.per_type_type,
            resolve_context.per_variable,
        ))
    }

    pub(crate) fn value_resolutions_query(
        db: &dyn ScopeDatabase,
        id: InFile<ValueGroupId>,
    ) -> Arc<Resolutions> {
        let value_surface = db.value_surface(id);
        let value_scope = db.value_scope(id);
        let nominal_map = db.nominal_map(id.file_id);

        let mut resolve_context = ResolveContext::new(
            &value_surface.expr_arena,
            &value_surface.let_name_arena,
            &value_surface.binder_arena,
            &value_surface.type_arena,
            &value_scope,
            &nominal_map,
        );

        value_surface.value.equations.iter().for_each(|(_, value_equation)| {
            resolve_context.visit_value_equation(value_equation);
        });

        Arc::new(Resolutions::new(
            resolve_context.per_constructor_expr,
            resolve_context.per_constructor_binder,
            resolve_context.per_type_type,
            resolve_context.per_variable,
        ))
    }

    fn resolve_constructor_expr(&mut self, expr_id: ExprId, name: impl AsRef<str>) {
        let name = name.as_ref();
        self.nominal_map.constructor_id(name).map(|(data_id, constructor_id)| {
            self.per_constructor_expr.insert(
                expr_id,
                ConstructorResolution { data_id, constructor_id: constructor_id.value },
            )
        });
    }

    fn resolve_constructor_binder(&mut self, binder_id: BinderId, name: impl AsRef<str>) {
        let name = name.as_ref();
        self.nominal_map.constructor_id(name).map(|(data_id, constructor_id)| {
            self.per_constructor_binder.insert(
                binder_id,
                ConstructorResolution { data_id, constructor_id: constructor_id.value },
            )
        });
    }

    fn resolve_constructor_type(&mut self, type_id: TypeId, name: impl AsRef<str>) {
        let name = name.as_ref();
        self.nominal_map
            .data_id(name)
            .map(|data_id| self.per_type_type.insert(type_id, TypeResolution::Data(data_id)));
    }

    fn resolve_variable_expr(&mut self, expr_id: ExprId, name: impl AsRef<str>) {
        let name = name.as_ref();
        let expr_scope_id = self.value_scope.expr_scope(expr_id);

        let mut thunked = false;
        let kind =
            self.value_scope.ancestors(expr_scope_id).find_map(|scope_data| {
                match &scope_data.kind {
                    ScopeKind::Root => None,
                    ScopeKind::Binders(names, kind) => {
                        if let BinderKind::Thunk = kind {
                            thunked = true;
                        }
                        Some(VariableResolutionKind::Binder(*names.get(name)?))
                    }
                    ScopeKind::LetBound(names, _) => {
                        Some(VariableResolutionKind::LetName(*names.get(name)?))
                    }
                }
            });

        let kind = kind.or_else(|| {
            let id = self.nominal_map.value_group_id(name)?;
            Some(VariableResolutionKind::Local(id.value))
        });

        if let Some(kind) = kind {
            self.per_variable.insert(expr_id, VariableResolution { thunked, kind });
        }
    }
}

impl<'a> Visitor<'a> for ResolveContext<'a> {
    fn expr_arena(&self) -> &'a Arena<Expr> {
        self.expr_arena
    }

    fn let_name_arena(&self) -> &'a Arena<LetName> {
        self.let_name_arena
    }

    fn binder_arena(&self) -> &'a Arena<Binder> {
        self.binder_arena
    }

    fn type_arena(&self) -> &'a Arena<Type> {
        self.type_arena
    }

    fn visit_expr(&mut self, expr_id: ExprId) {
        match &self.expr_arena[expr_id] {
            Expr::Constructor(constructor) => {
                self.resolve_constructor_expr(expr_id, &constructor.value);
            }
            Expr::Variable(variable) => {
                self.resolve_variable_expr(expr_id, &variable.value);
            }
            _ => default_visit_expr(self, expr_id),
        }
    }

    fn visit_binder(&mut self, binder_id: BinderId) {
        match &self.binder_arena[binder_id] {
            Binder::Constructor { name, fields } => {
                self.resolve_constructor_binder(binder_id, &name.value);
                for field in fields.iter() {
                    default_visit_binder(self, *field);
                }
            }
            _ => default_visit_binder(self, binder_id),
        }
    }

    fn visit_type(&mut self, type_id: TypeId) {
        match &self.type_arena[type_id] {
            Type::Constructor(constructor) => {
                self.resolve_constructor_type(type_id, &constructor.value);
            }
            _ => default_visit_type(self, type_id),
        }
    }
}
