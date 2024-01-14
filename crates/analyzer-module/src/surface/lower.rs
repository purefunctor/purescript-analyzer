//! Conversion from the CST to AST

use std::sync::Arc;

use files::FileId;
use rowan::ast::{AstChildren, AstNode};
use syntax::ast;

use crate::{index::NominalMap, interner::InDb, SurfaceDatabase};

use super::{
    DataMembers, ExportItem, ExportList, ImportDeclaration, ImportItem, ImportList, Module,
    ModuleBody, ModuleHeader, ModuleImports, ModuleName, Name, SurfaceArena,
};

struct Ctx {
    arena: SurfaceArena,
    nominal_map: Arc<NominalMap>,
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
    let body = module.body().and_then(|_| None).unwrap_or_else(|| {
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

pub(super) fn file_surface_query(
    db: &dyn SurfaceDatabase,
    file_id: FileId,
) -> (Arc<Module>, Arc<SurfaceArena>) {
    let node = db.parse_file(file_id);

    ast::Source::<ast::Module>::cast(node)
        .and_then(|source| {
            let mut ctx = Ctx { arena: Default::default(), nominal_map: db.nominal_map(file_id) };
            let module = lower_module(&mut ctx, db, source.child()?);
            Some((Arc::new(module), Arc::new(ctx.arena)))
        })
        .unwrap_or_else(|| {
            unreachable!("impossible: empty source files have a traversable CST");
        })
}
