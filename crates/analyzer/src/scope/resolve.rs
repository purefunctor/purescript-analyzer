use std::sync::Arc;

use files::FileId;
use rustc_hash::{FxHashMap, FxHashSet};
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    index::nominal::{ClassGroupId, DataGroupId, ValueGroupId},
    surface::tree::*,
    ScopeDatabase,
};

use super::{
    ConstructorResolution, ResolveInfo, ScopeInfo, ScopeKind, TypeConstructorKind,
    TypeConstructorResolution, TypeVariableResolution, VariableResolution,
};

struct Ctx<'a> {
    file_id: FileId,

    arena: &'a SurfaceArena,
    imports: &'a Imports,
    local: &'a UsableItems,

    scope_info: &'a ScopeInfo,
    resolve_info: ResolveInfo,
}

impl<'a> Ctx<'a> {
    fn new(
        file_id: FileId,
        arena: &'a SurfaceArena,
        imports: &'a Imports,
        local: &'a UsableItems,
        scope_info: &'a ScopeInfo,
    ) -> Ctx<'a> {
        let resolve_info =
            ResolveInfo::new(imports.values().map(|(file_id, _)| *file_id).collect());
        Ctx { file_id, arena, imports, local, scope_info, resolve_info }
    }
}

type Imports = FxHashMap<ModuleName, (FileId, UsableItems)>;

#[derive(Debug, Default)]
struct UsableItems {
    data: FxHashMap<Name, DataGroupId>,
    data_constructor: FxHashMap<Name, (DataGroupId, AstId<ast::DataConstructor>)>,
    class: FxHashMap<Name, ClassGroupId>,
    class_member: FxHashMap<Name, (ClassGroupId, AstId<ast::ClassMember>)>,
    value: FxHashMap<Name, ValueGroupId>,
}

impl UsableItems {
    fn find_type_constructor(&self, name: &Name) -> Option<TypeConstructorKind> {
        let class =
            self.class.iter().map(|(name, class_id)| (name, TypeConstructorKind::Class(*class_id)));
        let data =
            self.data.iter().map(|(name, data_id)| (name, TypeConstructorKind::Data(*data_id)));

        class.chain(data).find_map(|(type_constructor_name, type_constructor_id)| {
            if name == type_constructor_name {
                Some(type_constructor_id)
            } else {
                None
            }
        })
    }

    fn find_data_constructor(
        &self,
        name: &Name,
    ) -> Option<(DataGroupId, AstId<ast::DataConstructor>)> {
        self.data_constructor.iter().find_map(|(constructor_name, data_constructor_id)| {
            if name == constructor_name {
                Some(*data_constructor_id)
            } else {
                None
            }
        })
    }

    fn find_value(&self, name: &Name) -> Option<ValueGroupId> {
        self.value.iter().find_map(
            |(value_name, value_id)| {
                if name == value_name {
                    Some(*value_id)
                } else {
                    None
                }
            },
        )
    }
}

// region: Common

fn resolve_constructor(ctx: &Ctx, name: &Qualified<Name>) -> Option<ConstructorResolution> {
    if let Some(prefix) = &name.prefix {
        let name = &name.value;
        let (file_id, usable_items) = ctx.imports.get(prefix)?;
        usable_items.data_constructor.get(name).map(|(data_id, constructor_id)| {
            ConstructorResolution {
                file_id: *file_id,
                data_id: *data_id,
                constructor_id: *constructor_id,
            }
        })
    } else {
        let name = &name.value;

        let local_resolution =
            ctx.local.find_data_constructor(name).map(|(data_id, constructor_id)| {
                let file_id = ctx.file_id;
                ConstructorResolution { file_id, data_id, constructor_id }
            });

        local_resolution.or_else(|| {
            ctx.imports.values().find_map(|(file_id, usable_items)| {
                let file_id = *file_id;
                let (data_id, constructor_id) = usable_items.find_data_constructor(name)?;
                Some(ConstructorResolution { file_id, data_id, constructor_id })
            })
        })
    }
}

fn resolve_type_constructor(
    ctx: &mut Ctx,
    name: &Qualified<Name>,
) -> Option<TypeConstructorResolution> {
    if let Some(prefix) = &name.prefix {
        let name = &name.value;
        let (file_id, usable_items) = ctx.imports.get(prefix)?;
        usable_items.data.get(name).map(|&data_id| {
            let file_id = *file_id;
            let kind = TypeConstructorKind::Data(data_id);
            TypeConstructorResolution { file_id, kind }
        })
    } else {
        let name = &name.value;

        let local_resolution = ctx.local.find_type_constructor(name).map(|kind| {
            let file_id = ctx.file_id;
            TypeConstructorResolution { file_id, kind }
        });

        local_resolution.or_else(|| {
            ctx.imports.values().find_map(|(file_id, usable_items)| {
                let file_id = *file_id;
                let kind = usable_items.find_type_constructor(name)?;
                Some(TypeConstructorResolution { file_id, kind })
            })
        })
    }
}

fn resolve_type_variable(
    ctx: &mut Ctx,
    type_id: TypeId,
    name: &Name,
) -> Option<TypeVariableResolution> {
    let scope_id = ctx.scope_info.type_scope(type_id);
    ctx.scope_info.ancestors(scope_id).find_map(|scope_data| match &scope_data.kind {
        ScopeKind::Root => None,
        ScopeKind::Binders(_) => None,
        ScopeKind::LetBound(_) => None,
        ScopeKind::TypeVariable(names, kind) => {
            if names.contains(name) {
                Some(TypeVariableResolution { file_id: ctx.file_id, kind: *kind })
            } else {
                None
            }
        }
    })
}

fn resolve_variable(
    ctx: &mut Ctx,
    expr_id: ExprId,
    name: &Qualified<Name>,
) -> Option<VariableResolution> {
    if let Some(prefix) = &name.prefix {
        let name = &name.value;
        let (file_id, usable_items) = ctx.imports.get(prefix)?;
        usable_items.value.get(name).map(|value_id| {
            VariableResolution::Imported(InFile { file_id: *file_id, value: *value_id })
        })
    } else {
        let name = &name.value;
        let scope_id = ctx.scope_info.expr_scope(expr_id);

        let scope_resolution =
            ctx.scope_info.ancestors(scope_id).find_map(|scope_data| match &scope_data.kind {
                ScopeKind::Root => None,
                ScopeKind::Binders(names) => {
                    if let Some(names) = names {
                        Some(VariableResolution::Binder(*names.get(name)?))
                    } else {
                        None
                    }
                }
                ScopeKind::LetBound(names) => Some(VariableResolution::LetName(*names.get(name)?)),
                ScopeKind::TypeVariable(_, _) => None,
            });

        let local_resolution = scope_resolution.or_else(|| {
            let value_id = ctx.local.find_value(name)?;
            Some(VariableResolution::Local(value_id))
        });

        local_resolution.or_else(|| {
            ctx.imports.values().find_map(|(file_id, usable_items)| {
                let file_id = *file_id;
                let value = usable_items.find_value(name)?;
                Some(VariableResolution::Imported(InFile { file_id, value }))
            })
        })
    }
}

// endregion

// region: Traversals

fn resolve_class_declaration(ctx: &mut Ctx, class_declaration: &ClassDeclaration) {
    if let Some(signature) = class_declaration.signature {
        resolve_type(ctx, signature);
    }
    class_declaration.members.iter().for_each(|(_, class_member)| {
        resolve_type(ctx, class_member.ty);
    });
}

fn resolve_data_declaration(ctx: &mut Ctx, data_declaration: &DataDeclaration) {
    if let Some(annotation) = data_declaration.annotation {
        resolve_type(ctx, annotation);
    }
    data_declaration.constructors.iter().for_each(|(_, constructor)| {
        constructor.fields.iter().for_each(|field| {
            resolve_type(ctx, *field);
        })
    });
}

fn resolve_value_declaration(ctx: &mut Ctx, value_declaration: &ValueDeclaration) {
    if let Some(annotation) = value_declaration.annotation {
        resolve_type(ctx, annotation);
    }
    value_declaration.equations.iter().for_each(|value_equation| {
        resolve_value_equation(ctx, value_equation);
    });
}

fn resolve_value_equation(ctx: &mut Ctx, value_equation: &ValueEquation) {
    value_equation.binders.iter().for_each(|binder| {
        resolve_binder(ctx, *binder);
    });
    resolve_binding(ctx, &value_equation.binding);
}

fn resolve_binding(ctx: &mut Ctx, binding: &Binding) {
    match binding {
        Binding::Unconditional { where_expr } => resolve_where_expr(ctx, where_expr),
    }
}

fn resolve_where_expr(ctx: &mut Ctx, where_expr: &WhereExpr) {
    resolve_let_bindings(ctx, &where_expr.let_bindings);
    resolve_expr(ctx, where_expr.expr_id);
}

fn resolve_let_bindings(ctx: &mut Ctx, let_bindings: &[LetBinding]) {
    for let_binding in let_bindings {
        match let_binding {
            LetBinding::Name { id } => {
                let let_name = &ctx.arena[*id];
                if let Some(annotation) = let_name.annotation {
                    resolve_type(ctx, annotation);
                }
                resolve_let_name_equations(ctx, &let_name.equations);
            }
            LetBinding::Pattern { binder, where_expr } => {
                resolve_binder(ctx, *binder);
                resolve_where_expr(ctx, where_expr);
            }
        }
    }
}

fn resolve_let_name_equations(ctx: &mut Ctx, equations: &[LetNameEquation]) {
    for equation in equations {
        for binder_id in &equation.binders {
            resolve_binder(ctx, *binder_id);
        }
        resolve_binding(ctx, &equation.binding);
    }
}

fn resolve_binder(ctx: &mut Ctx, binder_id: BinderId) {
    match &ctx.arena[binder_id] {
        Binder::Constructor { name, fields } => {
            if let Some(constructor) = resolve_constructor(ctx, name) {
                ctx.resolve_info.per_constructor_binder.insert(binder_id, constructor);
            }
            for field in fields {
                resolve_binder(ctx, *field);
            }
        }
        Binder::Literal(literal) => resolve_literal(ctx, literal, resolve_binder),
        Binder::Negative(_) => (),
        Binder::Parenthesized(parenthesized) => resolve_binder(ctx, *parenthesized),
        Binder::Variable(_) => (),
        Binder::Wildcard => (),
        Binder::NotImplemented => (),
    }
}

fn resolve_expr(ctx: &mut Ctx, expr_id: ExprId) {
    match &ctx.arena[expr_id] {
        Expr::Application(head, spine) => {
            resolve_expr(ctx, *head);
            for argument in spine {
                resolve_expr(ctx, *argument);
            }
        }
        Expr::Constructor(name) => {
            if let Some(constructor) = resolve_constructor(ctx, name) {
                ctx.resolve_info.per_constructor_expr.insert(expr_id, constructor);
            }
        }
        Expr::Lambda(binders, body) => {
            for binder in binders {
                resolve_binder(ctx, *binder);
            }
            resolve_expr(ctx, *body)
        }
        Expr::LetIn(let_bindings, body) => {
            resolve_let_bindings(ctx, let_bindings);
            resolve_expr(ctx, *body);
        }
        Expr::Literal(literal) => resolve_literal(ctx, literal, resolve_expr),
        Expr::Variable(name) => {
            if let Some(variable) = resolve_variable(ctx, expr_id, name) {
                ctx.resolve_info.per_variable_expr.insert(expr_id, variable);
            }
        }
        Expr::NotImplemented => (),
    }
}

fn resolve_type(ctx: &mut Ctx, type_id: TypeId) {
    match &ctx.arena[type_id] {
        Type::Arrow(arguments, result) => {
            for argument in arguments {
                resolve_type(ctx, *argument);
            }
            resolve_type(ctx, *result);
        }
        Type::Application(function, arguments) => {
            resolve_type(ctx, *function);
            for argument in arguments {
                resolve_type(ctx, *argument);
            }
        }
        Type::Constrained(constraint, constrained) => {
            resolve_type(ctx, *constraint);
            resolve_type(ctx, *constrained);
        }
        Type::Constructor(name) => {
            if let Some(type_constructor) = resolve_type_constructor(ctx, name) {
                ctx.resolve_info.per_type_type.insert(type_id, type_constructor);
            }
        }
        Type::Forall(_, inner) => {
            resolve_type(ctx, *inner);
        }
        Type::Parenthesized(parenthesized) => resolve_type(ctx, *parenthesized),
        Type::Variable(name) => {
            if let Some(type_variable) = resolve_type_variable(ctx, type_id, name) {
                ctx.resolve_info.per_variable_type.insert(type_id, type_variable);
            }
        }
        Type::NotImplemented => (),
    }
}

fn resolve_literal<T>(
    ctx: &mut Ctx,
    literal: &Literal<T>,
    mut resolve_inner: impl FnMut(&mut Ctx, T),
) where
    T: Copy,
{
    match literal {
        Literal::Array(items) => {
            for item in items {
                resolve_inner(ctx, *item);
            }
        }
        Literal::Record(items) => {
            for item in items {
                match item {
                    RecordItem::RecordPun(_) => (),
                    RecordItem::RecordField(_, item) => {
                        resolve_inner(ctx, *item);
                    }
                }
            }
        }
        Literal::Int(_) => (),
        Literal::Number(_) => (),
        Literal::String(_) => (),
        Literal::Char(_) => (),
        Literal::Boolean(_) => (),
    }
}

// endregion

// region: Usable Items

enum AllowListMembers {
    All,
    Enumerated(FxHashSet<Name>),
}

#[derive(Default)]
struct ExportedItems {
    data: FxHashMap<Name, ExportedData>,
    class: FxHashMap<Name, ClassGroupId>,
    value: FxHashMap<Name, ValueGroupId>,
}

struct ExportedData {
    id: DataGroupId,
    constructors: FxHashMap<Name, AstId<ast::DataConstructor>>,
}

fn exported_items(surface: &Module) -> ExportedItems {
    let export_all = surface.header.export_list.is_none();
    let mut allowlist_data = FxHashMap::default();
    let mut allowlist_class = FxHashSet::default();
    let mut allowlist_value = FxHashSet::default();

    if let Some(export_list) = &surface.header.export_list {
        for export_item in &export_list.items {
            match export_item {
                ExportItem::ExportClass(name) => {
                    allowlist_class.insert(Name::clone(name));
                }
                ExportItem::ExportType(name, members) => {
                    let members = if let Some(members) = members {
                        match members {
                            DataMembers::DataAll => AllowListMembers::All,
                            DataMembers::DataEnumerated(names) => {
                                AllowListMembers::Enumerated(names.iter().cloned().collect())
                            }
                        }
                    } else {
                        AllowListMembers::Enumerated(FxHashSet::default())
                    };
                    allowlist_data.insert(Name::clone(name), members);
                }
                ExportItem::ExportValue(name) => {
                    allowlist_value.insert(Name::clone(name));
                }
            }
        }
    }

    let is_constructor_exported = |data_name: &Name, constructor_name: &Name| {
        export_all
            || allowlist_data.get(data_name).is_some_and(|member| match member {
                AllowListMembers::All => true,
                AllowListMembers::Enumerated(constructors) => {
                    constructors.contains(constructor_name)
                }
            })
    };

    let mut exported_items = ExportedItems::default();

    for declaration in &surface.body.declarations {
        match declaration {
            Declaration::ClassDeclaration(class_declaration) => {
                if export_all || allowlist_class.contains(&class_declaration.name) {
                    exported_items
                        .class
                        .insert(Name::clone(&class_declaration.name), class_declaration.id);
                }
            }
            Declaration::DataDeclaration(data_declaration) => {
                let id = data_declaration.id;
                let constructors = data_declaration
                    .constructors
                    .iter()
                    .filter_map(|(constructor_id, constructor)| {
                        if is_constructor_exported(&data_declaration.name, &constructor.name) {
                            Some((Name::clone(&constructor.name), *constructor_id))
                        } else {
                            None
                        }
                    })
                    .collect();
                if export_all || allowlist_data.contains_key(&data_declaration.name) {
                    exported_items.data.insert(
                        Name::clone(&data_declaration.name),
                        ExportedData { id, constructors },
                    );
                }
            }
            Declaration::ValueDeclaration(value_declaration) => {
                if export_all || allowlist_value.contains(&value_declaration.name) {
                    exported_items
                        .value
                        .insert(Name::clone(&value_declaration.name), value_declaration.id);
                }
            }
        }
    }

    exported_items
}

fn usable_items(surface: &Module, import: &ImportDeclaration) -> UsableItems {
    let exported_items = exported_items(surface);

    let import_all;
    let import_hiding;
    let mut allowlist_data = FxHashMap::default();
    let mut allowlist_value = FxHashSet::default();

    if let Some(import_list) = &import.import_list {
        import_all = false;
        import_hiding = import_list.hiding;
        for item in &import_list.items {
            match item {
                ImportItem::ImportClass(name) => {
                    allowlist_value.insert(Name::clone(name));
                }
                ImportItem::ImportType(name, members) => {
                    let members = if let Some(members) = members {
                        match members {
                            DataMembers::DataAll => AllowListMembers::All,
                            DataMembers::DataEnumerated(names) => {
                                AllowListMembers::Enumerated(names.iter().cloned().collect())
                            }
                        }
                    } else {
                        AllowListMembers::Enumerated(FxHashSet::default())
                    };
                    allowlist_data.insert(Name::clone(name), members);
                }
                ImportItem::ImportValue(name) => {
                    allowlist_value.insert(Name::clone(name));
                }
            }
        }
    } else {
        import_all = true;
        import_hiding = false;
    }

    let is_constructor_imported = |data_name: &Name, constructor_name: &Name| {
        import_all
            || (import_hiding
                ^ allowlist_data.get(data_name).is_some_and(|member| match member {
                    AllowListMembers::All => true,
                    AllowListMembers::Enumerated(constructors) => {
                        constructors.contains(constructor_name)
                    }
                }))
    };

    let mut usable_items = UsableItems::default();

    for (data_name, data) in exported_items.data {
        if import_all || (import_hiding ^ allowlist_data.contains_key(&data_name)) {
            usable_items.data.insert(Name::clone(&data_name), data.id);
        }
        for (constructor_name, id) in data.constructors {
            if is_constructor_imported(&data_name, &constructor_name) {
                usable_items.data_constructor.insert(constructor_name, (data.id, id));
            }
        }
    }

    for (name, id) in exported_items.value {
        if import_all || (import_hiding ^ allowlist_value.contains(&name)) {
            usable_items.value.insert(name, id);
        }
    }

    usable_items
}

fn local_items(surface: &Module) -> UsableItems {
    let mut usable_items = UsableItems::default();
    surface.body.declarations.iter().for_each(|declaration| match declaration {
        Declaration::ClassDeclaration(class_declaration) => {
            let class_name = Name::clone(&class_declaration.name);
            let class_id = class_declaration.id;
            usable_items.class.insert(class_name, class_id);

            class_declaration.members.iter().for_each(|(member_id, class_member)| {
                let member_name = Name::clone(&class_member.name);
                usable_items.class_member.insert(member_name, (class_id, *member_id));
            });
        }
        Declaration::DataDeclaration(data_declaration) => {
            let data_name = Name::clone(&data_declaration.name);
            let data_id = data_declaration.id;
            usable_items.data.insert(data_name, data_id);

            data_declaration.constructors.iter().for_each(|(constructor_id, data_constructor)| {
                let constructor_name = Name::clone(&data_constructor.name);
                usable_items.data_constructor.insert(constructor_name, (data_id, *constructor_id));
            });
        }
        Declaration::ValueDeclaration(value_declaration) => {
            let name = Name::clone(&value_declaration.name);
            let id = value_declaration.id;
            usable_items.value.insert(name, id);
        }
    });
    usable_items
}

// endregion

pub(super) fn file_resolve_query(db: &dyn ScopeDatabase, file_id: FileId) -> Arc<ResolveInfo> {
    let (surface, arena) = db.file_surface(file_id);
    let module_map = db.module_map();
    let scope_info = db.file_scope(file_id);

    let mut imports = FxHashMap::default();
    for import in &surface.imports.declarations {
        let Some(file_id) = module_map.file_id(&import.name) else {
            continue;
        };

        let (imported_surface, _) = db.file_surface(file_id);
        let usable_items = usable_items(&imported_surface, import);

        let module_name = ModuleName::clone(import.qualified_as.as_ref().unwrap_or(&import.name));
        imports.insert(module_name, (file_id, usable_items));
    }

    let local = local_items(&surface);

    let mut ctx = Ctx::new(file_id, &arena, &imports, &local, &scope_info);
    surface.body.declarations.iter().for_each(|declaration| match declaration {
        Declaration::ClassDeclaration(class_declaration) => {
            resolve_class_declaration(&mut ctx, class_declaration);
        }
        Declaration::DataDeclaration(data_declaration) => {
            resolve_data_declaration(&mut ctx, data_declaration);
        }
        Declaration::ValueDeclaration(value_declaration) => {
            resolve_value_declaration(&mut ctx, value_declaration);
        }
    });

    Arc::new(ctx.resolve_info)
}
