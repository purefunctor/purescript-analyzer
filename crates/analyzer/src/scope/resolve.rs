//! Implements the name resolution algorithm.

use la_arena::Arena;
use std::sync::Arc;

use rustc_hash::FxHashMap;

use crate::{
    id::InFile,
    names::Qualified,
    resolver::{DataGroupId, NominalMap, ValueGroupId},
    scope::{BinderKind, ScopeKind, VariableResolution},
    surface::{
        visitor::{default_visit_binder, default_visit_expr, default_visit_type, Visitor},
        Binder, BinderId, Expr, ExprId, LetName, ModuleImports, Type, TypeId,
    },
    ScopeDatabase,
};

use super::{
    ConstructorResolution, Resolutions, TypeResolution, ValueGroupScope, VariableResolutionKind,
    WithScope,
};

struct ResolveArenas<'a> {
    expr_arena: &'a Arena<Expr>,
    let_name_arena: &'a Arena<LetName>,
    binder_arena: &'a Arena<Binder>,
    type_arena: &'a Arena<Type>,
}

struct ResolveEnv<'a> {
    value_scope: &'a WithScope<ValueGroupScope>,
    nominal_map: &'a NominalMap,
    module_imports: &'a ModuleImports,
}

pub(crate) struct ResolveContext<'a> {
    db: &'a dyn ScopeDatabase,
    resolve_arenas: ResolveArenas<'a>,
    resolve_env: ResolveEnv<'a>,
    per_constructor_expr: FxHashMap<ExprId, ConstructorResolution>,
    per_constructor_binder: FxHashMap<BinderId, ConstructorResolution>,
    per_type_type: FxHashMap<TypeId, TypeResolution>,
    per_variable: FxHashMap<ExprId, VariableResolution>,
}

impl<'a> ResolveContext<'a> {
    fn new(
        db: &'a dyn ScopeDatabase,
        resolve_arenas: ResolveArenas<'a>,
        resolve_env: ResolveEnv<'a>,
    ) -> ResolveContext<'a> {
        let per_constructor_expr = FxHashMap::default();
        let per_constructor_binder = FxHashMap::default();
        let per_type_type = FxHashMap::default();
        let per_variable = FxHashMap::default();
        ResolveContext {
            db,
            resolve_arenas,
            resolve_env,
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
        let module_imports = db.module_imports(id.file_id);

        let resolve_arenas = ResolveArenas {
            expr_arena: &data_surface.expr_arena,
            let_name_arena: &data_surface.let_name_arena,
            binder_arena: &data_surface.binder_arena,
            type_arena: &data_surface.type_arena,
        };

        let resolve_env = ResolveEnv {
            value_scope: &value_scope,
            nominal_map: &nominal_map,
            module_imports: &module_imports,
        };

        let mut resolve_context = ResolveContext::new(db, resolve_arenas, resolve_env);

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
        let module_imports = db.module_imports(id.file_id);

        let resolve_arenas = ResolveArenas {
            expr_arena: &value_surface.expr_arena,
            let_name_arena: &value_surface.let_name_arena,
            binder_arena: &value_surface.binder_arena,
            type_arena: &value_surface.type_arena,
        };

        let resolve_env = ResolveEnv {
            value_scope: &value_scope,
            nominal_map: &nominal_map,
            module_imports: &module_imports,
        };

        let mut resolve_context = ResolveContext::new(db, resolve_arenas, resolve_env);

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
        self.resolve_env.nominal_map.constructor_id(name).map(|(data_id, constructor_id)| {
            self.per_constructor_expr.insert(
                expr_id,
                ConstructorResolution { data_id, constructor_id: constructor_id.value },
            )
        });
    }

    fn resolve_constructor_binder(&mut self, binder_id: BinderId, name: impl AsRef<str>) {
        let name = name.as_ref();
        self.resolve_env.nominal_map.constructor_id(name).map(|(data_id, constructor_id)| {
            self.per_constructor_binder.insert(
                binder_id,
                ConstructorResolution { data_id, constructor_id: constructor_id.value },
            )
        });
    }

    fn resolve_constructor_type(&mut self, type_id: TypeId, name: impl AsRef<str>) {
        let name = name.as_ref();
        self.resolve_env
            .nominal_map
            .data_id(name)
            .map(|data_id| self.per_type_type.insert(type_id, TypeResolution::Data(data_id)));
    }

    fn resolve_variable_expr(&mut self, expr_id: ExprId, name: &Qualified<impl AsRef<str>>) {
        if let Some(prefix) = &name.prefix {
            // FIXME: validate exports for the module being imported, somewhere, somehow.
            if let Some(qualified_import) = self.resolve_env.module_imports.find_qualified(prefix) {
                let file_id = qualified_import.file_id;

                if !qualified_import.is_value_imported(&name.value) {
                    unimplemented!("Is not imported!");
                }

                let export_items = self.db.module_exports(file_id);
                if !export_items.is_value_exported(&name.value) {
                    unimplemented!("Is not exported!");
                }

                let nominal_map = self.db.nominal_map(file_id);
                if let Some(value_group_id) = nominal_map.value_group_id(&name.value) {
                    self.per_variable.insert(
                        expr_id,
                        VariableResolution {
                            thunked: false,
                            kind: VariableResolutionKind::Imported(value_group_id),
                        },
                    );
                }
            } else {
                unimplemented!("Could not resolve...");
            }
        } else {
            let name = name.value.as_ref();
            let expr_scope_id = self.resolve_env.value_scope.expr_scope(expr_id);

            let mut thunked = false;
            let kind =
                self.resolve_env.value_scope.ancestors(expr_scope_id).find_map(|scope_data| {
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
                let id = self.resolve_env.nominal_map.value_group_id(name)?;
                Some(VariableResolutionKind::Local(id.value))
            });

            if let Some(kind) = kind {
                self.per_variable.insert(expr_id, VariableResolution { thunked, kind });
            }
        }
    }
}

impl<'a> Visitor<'a> for ResolveContext<'a> {
    fn expr_arena(&self) -> &'a Arena<Expr> {
        self.resolve_arenas.expr_arena
    }

    fn let_name_arena(&self) -> &'a Arena<LetName> {
        self.resolve_arenas.let_name_arena
    }

    fn binder_arena(&self) -> &'a Arena<Binder> {
        self.resolve_arenas.binder_arena
    }

    fn type_arena(&self) -> &'a Arena<Type> {
        self.resolve_arenas.type_arena
    }

    fn visit_expr(&mut self, expr_id: ExprId) {
        match &self.resolve_arenas.expr_arena[expr_id] {
            Expr::Constructor(constructor) => {
                self.resolve_constructor_expr(expr_id, &constructor.value);
            }
            Expr::Variable(variable) => {
                self.resolve_variable_expr(expr_id, variable);
            }
            _ => default_visit_expr(self, expr_id),
        }
    }

    fn visit_binder(&mut self, binder_id: BinderId) {
        match &self.resolve_arenas.binder_arena[binder_id] {
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
        match &self.resolve_arenas.type_arena[type_id] {
            Type::Constructor(constructor) => {
                self.resolve_constructor_type(type_id, &constructor.value);
            }
            _ => default_visit_type(self, type_id),
        }
    }
}
