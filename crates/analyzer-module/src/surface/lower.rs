//! Conversion from the CST to AST

use std::sync::Arc;

use files::FileId;
use rowan::ast::{AstChildren, AstNode};
use syntax::{ast, PureScript, SyntaxNode, SyntaxNodePtr};

use crate::{
    id::AstId,
    index::{nominal::DataGroup, NominalMap, PositionalMap},
    interner::InDb,
    SurfaceDatabase,
};

use super::{
    DataConstructor, DataDeclaration, DataMembers, Declaration, ExportItem, ExportList,
    ImportDeclaration, ImportItem, ImportList, Module, ModuleBody, ModuleHeader, ModuleImports,
    ModuleName, Name, Qualified, SourceMap, SurfaceArena, Type, TypeId, TypeVariable,
};

struct Ctx {
    arena: SurfaceArena,
    source_map: SourceMap,

    syntax_node: SyntaxNode,
    nominal_map: Arc<NominalMap>,
    positional_map: Arc<PositionalMap>,
}

impl Ctx {
    fn ast_of<N: AstNode<Language = PureScript>>(&self, id: AstId<N>) -> N {
        self.positional_map.ast_ptr(id).to_node(&self.syntax_node)
    }

    fn alloc_type(&mut self, lowered: Type, ast: Option<&ast::Type>) -> TypeId {
        let type_id = self.arena.alloc_ty(lowered);
        if let Some(ast) = ast {
            self.source_map.type_to_cst.insert(type_id, SyntaxNodePtr::new(ast.syntax()));
            self.source_map.cst_to_type.insert(SyntaxNodePtr::new(ast.syntax()), type_id);
        }
        type_id
    }

    fn nil_type(&mut self) -> TypeId {
        self.alloc_type(Type::NotImplemented, None)
    }
}

fn lower_qualified_name(
    db: &dyn SurfaceDatabase,
    qualified: ast::QualifiedName,
) -> Qualified<Name> {
    let prefix = qualified
        .prefix()
        .and_then(|prefix| prefix.in_db(db))
        .map(|prefix| ModuleName::from_raw(prefix));
    let value = Name::from_raw(
        qualified
            .name_ref()
            .and_then(|name| name.in_db(db))
            .unwrap_or_else(|| db.interner().intern("$Invalid$")),
    );
    Qualified { prefix, value }
}

fn lower_module(ctx: &mut Ctx, db: &dyn SurfaceDatabase, module: ast::Module) -> Module {
    let header = module.header().map(|header| lower_header(ctx, db, header)).unwrap_or_else(|| {
        let name = ModuleName::from_raw(db.interner().intern("$Invalid$"));
        let export_list = ExportList { items: vec![], explicit: false };
        ModuleHeader { name, export_list }
    });
    let imports = module.imports().map(|imports| lower_imports(db, imports)).unwrap_or_else(|| {
        let declarations = vec![];
        ModuleImports { declarations }
    });
    let body = module.body().map(|body| lower_module_body(ctx, db, body)).unwrap_or_else(|| {
        let declarations = vec![];
        ModuleBody { declarations }
    });
    Module { header, imports, body }
}

fn lower_header(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    header: ast::ModuleHeader,
) -> ModuleHeader {
    let name = ModuleName::from_raw(
        header
            .name()
            .and_then(|name| name.in_db(db))
            .unwrap_or_else(|| db.interner().intern("$Invalid$")),
    );
    let export_list = lower_export_list(ctx, db, header.export_list());

    ModuleHeader { name, export_list }
}

fn lower_export_list(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    export_list: Option<ast::ExportList>,
) -> ExportList {
    match export_list {
        Some(export_list) => {
            let export_items =
                export_list.child().and_then(|export_list| Some(export_list.child()?.children()));

            let items = export_items
                .map(|export_items| lower_export_items(db, export_items))
                .unwrap_or_else(|| vec![]);
            let explicit = true;

            ExportList { items, explicit }
        }
        None => {
            let items = ctx
                .nominal_map
                .iter_value_group()
                .map(|(_, value_group)| {
                    let name = Name::from_raw(Arc::clone(&value_group.name));
                    ExportItem::ExportValue(name)
                })
                .collect();
            let explicit = false;

            ExportList { items, explicit }
        }
    }
}

fn lower_export_items(
    db: &dyn SurfaceDatabase,
    export_items: AstChildren<ast::ExportItem>,
) -> Vec<ExportItem> {
    export_items
        .filter_map(|export_item| match export_item {
            ast::ExportItem::ExportType(t) => {
                let name = Name::from_raw(t.name_ref()?.in_db(db)?);
                let data_members =
                    t.data_members().map(|data_members| lower_data_members(db, data_members));
                Some(ExportItem::ExportType(name, data_members))
            }
            ast::ExportItem::ExportValue(v) => {
                let name = Name::from_raw(v.name_ref()?.in_db(db)?);
                Some(ExportItem::ExportValue(name))
            }
        })
        .collect()
}

fn lower_imports(db: &dyn SurfaceDatabase, imports: ast::ModuleImports) -> ModuleImports {
    let declarations = imports
        .imports()
        .map(|imports| {
            imports
                .children()
                .map(|import_declaration| lower_import_declaration(db, import_declaration))
                .collect()
        })
        .unwrap_or_default();
    ModuleImports { declarations }
}

fn lower_import_declaration(
    db: &dyn SurfaceDatabase,
    import_declaration: ast::ImportDeclaration,
) -> ImportDeclaration {
    let name = ModuleName::from_raw(
        import_declaration
            .module_name()
            .and_then(|module_name| module_name.in_db(db))
            .unwrap_or_else(|| db.interner().intern("$Invalid$")),
    );

    let qualified_as = import_declaration
        .import_qualified()
        .and_then(|qualified| Some(ModuleName::from_raw(qualified.module_name()?.in_db(db)?)));

    let import_list =
        import_declaration.import_list().map(|import_list| lower_import_list(db, import_list));

    ImportDeclaration { name, qualified_as, import_list }
}

fn lower_import_list(db: &dyn SurfaceDatabase, import_list: ast::ImportList) -> ImportList {
    let hiding = import_list.hiding_token().is_some();
    let items = import_list
        .import_items()
        .map(|import_items| {
            import_items
                .children()
                .filter_map(|import_item| match import_item {
                    ast::ImportItem::ImportClass(_) => None,
                    ast::ImportItem::ImportOp(_) => None,
                    ast::ImportItem::ImportType(t) => {
                        let name = Name::from_raw(t.name_ref()?.in_db(db)?);
                        let data_members = t
                            .data_members()
                            .map(|data_members| lower_data_members(db, data_members));
                        Some(ImportItem::ImportType(name, data_members))
                    }
                    ast::ImportItem::ImportTypeOp(_) => None,
                    ast::ImportItem::ImportValue(v) => {
                        let name = Name::from_raw(v.name_ref()?.in_db(db)?);
                        Some(ImportItem::ImportValue(name))
                    }
                })
                .collect()
        })
        .unwrap_or_else(|| vec![]);
    ImportList { items, hiding }
}

fn lower_data_members(db: &dyn SurfaceDatabase, data_members: ast::DataMembers) -> DataMembers {
    match data_members {
        ast::DataMembers::DataAll(_) => DataMembers::DataAll,
        ast::DataMembers::DataEnumerated(e) => {
            let names = e
                .constructors()
                .map(|names| {
                    names
                        .children()
                        .filter_map(|name| Some(Name::from_raw(name.in_db(db)?)))
                        .collect()
                })
                .unwrap_or_else(|| vec![]);
            DataMembers::DataEnumerated(names)
        }
    }
}

fn lower_module_body(ctx: &mut Ctx, db: &dyn SurfaceDatabase, body: ast::ModuleBody) -> ModuleBody {
    let declarations = body
        .declarations()
        .map(|declarations| lower_declarations(ctx, db, declarations.children()))
        .unwrap_or_else(|| vec![]);
    ModuleBody { declarations }
}

fn lower_declarations(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    _: AstChildren<ast::Declaration>,
) -> Vec<Declaration> {
    let nominal_map = Arc::clone(&ctx.nominal_map);

    let mut all = vec![];

    // For declarations that exist in groups, we prefer using the nominal map
    // representations during lowering rather than accessing the AST directly
    // simply to avoid redundant work.
    all.extend(nominal_map.iter_data_group().map(|(_, data_group)| {
        Declaration::DataDeclaration(lower_data_group(ctx, db, data_group))
    }));

    all
}

fn lower_data_group(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    data_group: &DataGroup,
) -> DataDeclaration {
    let name = Name::from_raw(Arc::clone(&data_group.name));
    let annotation = data_group.annotation.map(|id| {
        let annotation = ctx.ast_of(id);
        lower_data_annotation(ctx, db, annotation)
    });

    let equation = ctx.ast_of(data_group.equation);
    let variables = equation
        .variables()
        .map(|variables| lower_type_variable_binding(ctx, db, variables.children()))
        .unwrap_or_else(|| vec![]);

    let constructors = data_group
        .constructors
        .values()
        .map(|id| {
            let constructor = ctx.ast_of(*id);
            (*id, lower_data_constructor(ctx, db, constructor))
        })
        .collect();

    DataDeclaration { name, annotation, variables, constructors }
}

fn lower_data_annotation(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    annotation: ast::DataAnnotation,
) -> TypeId {
    annotation.kind().map(|kind| lower_type(ctx, db, &kind)).unwrap_or_else(|| ctx.nil_type())
}

fn lower_data_constructor(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    constructor: ast::DataConstructor,
) -> DataConstructor {
    let name = Name::from_raw(
        constructor
            .name()
            .and_then(|name| name.in_db(db))
            .unwrap_or_else(|| db.interner().intern("$Invalid$")),
    );
    let fields = constructor
        .fields()
        .map(|fields| fields.children().map(|field| lower_type(ctx, db, &field)).collect())
        .unwrap_or_else(|| vec![]);
    DataConstructor { name, fields }
}

fn lower_type(ctx: &mut Ctx, db: &dyn SurfaceDatabase, ty: &ast::Type) -> TypeId {
    let lowered = match ty {
        ast::Type::ApplicationType(application) => lower_type_application(ctx, db, application),
        ast::Type::ArrowType(arrow) => lower_type_arrow(ctx, db, arrow),
        ast::Type::ConstructorType(constructor) => lower_type_constructor(db, constructor),
        ast::Type::IntegerType(_) => Type::NotImplemented,
        ast::Type::KindedType(_) => Type::NotImplemented,
        ast::Type::OperatorNameType(_) => Type::NotImplemented,
        ast::Type::ParenthesizedType(parenthesized) => {
            lower_type_parenthesized(ctx, db, parenthesized)
        }
        ast::Type::RecordType(_) => Type::NotImplemented,
        ast::Type::RowType(_) => Type::NotImplemented,
        ast::Type::StringType(_) => Type::NotImplemented,
        ast::Type::TypeOperatorChain(_) => Type::NotImplemented,
        ast::Type::VariableType(variable) => lower_type_variable(db, variable),
        ast::Type::WildcardType(_) => Type::NotImplemented,
    };
    ctx.alloc_type(lowered, Some(&ty))
}

fn lower_type_application(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    application: &ast::ApplicationType,
) -> Type {
    let head =
        application.head().map(|head| lower_type(ctx, db, &head)).unwrap_or_else(|| ctx.nil_type());
    let spine = application
        .spine()
        .map(|spine| spine.children().map(|argument| lower_type(ctx, db, &argument)).collect())
        .unwrap_or_else(|| vec![]);

    Type::Application(head, spine)
}

fn lower_type_arrow(ctx: &mut Ctx, db: &dyn SurfaceDatabase, arrow: &ast::ArrowType) -> Type {
    let mut arguments = vec![arrow
        .argument()
        .map(|argument| lower_type(ctx, db, &argument))
        .unwrap_or_else(|| ctx.nil_type())];
    let mut result = arrow.result();

    while let Some(ast::Type::ArrowType(arrow)) = &result {
        arguments.push(
            arrow
                .argument()
                .map(|argument| lower_type(ctx, db, &argument))
                .unwrap_or_else(|| ctx.nil_type()),
        );
        result = arrow.result();
    }

    let result =
        result.map(|result| lower_type(ctx, db, &result)).unwrap_or_else(|| ctx.nil_type());

    Type::Arrow(arguments, result)
}

fn lower_type_constructor(db: &dyn SurfaceDatabase, constructor: &ast::ConstructorType) -> Type {
    let name = constructor
        .qualified_name()
        .map(|qualified| lower_qualified_name(db, qualified))
        .unwrap_or_else(|| {
            let prefix = None;
            let value = Name::from_raw(db.interner().intern("$Invalid$"));
            Qualified { prefix, value }
        });

    Type::Constructor(name)
}

fn lower_type_parenthesized(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    parenthesized: &ast::ParenthesizedType,
) -> Type {
    Type::Parenthesized(
        parenthesized.ty().map(|ty| lower_type(ctx, db, &ty)).unwrap_or_else(|| ctx.nil_type()),
    )
}

fn lower_type_variable(db: &dyn SurfaceDatabase, variable: &ast::VariableType) -> Type {
    let name = Name::from_raw(
        variable
            .name_ref()
            .and_then(|name| name.in_db(db))
            .unwrap_or_else(|| db.interner().intern("$Invalid$")),
    );
    Type::Variable(name)
}

fn lower_type_variable_binding(
    ctx: &mut Ctx,
    db: &dyn SurfaceDatabase,
    variable_bindings: AstChildren<ast::TypeVariableBinding>,
) -> Vec<TypeVariable> {
    variable_bindings
        .filter_map(|variable| match variable {
            ast::TypeVariableBinding::TypeVariableKinded(k) => {
                let name = Name::from_raw(k.name()?.in_db(db)?);
                let kind = lower_type(ctx, db, &k.kind()?);
                Some(TypeVariable::Kinded(name, kind))
            }
            ast::TypeVariableBinding::TypeVariableName(n) => {
                let name = Name::from_raw(n.name()?.in_db(db)?);
                Some(TypeVariable::Name(name))
            }
        })
        .collect()
}

pub(super) fn file_surface_query(
    db: &dyn SurfaceDatabase,
    file_id: FileId,
) -> (Arc<Module>, Arc<SurfaceArena>) {
    let node = db.parse_file(file_id);

    ast::Source::<ast::Module>::cast(node)
        .and_then(|source| {
            let mut ctx = Ctx {
                arena: Default::default(),
                source_map: Default::default(),
                syntax_node: db.parse_file(file_id),
                nominal_map: db.nominal_map(file_id),
                positional_map: db.positional_map(file_id),
            };
            let module = lower_module(&mut ctx, db, source.child()?);
            Some((Arc::new(module), Arc::new(ctx.arena)))
        })
        .unwrap_or_else(|| {
            unreachable!("impossible: empty source files have a traversable CST");
        })
}
