use std::sync::Arc;

use files::FileId;
use rowan::ast::AstNode;
use syntax::ast;

use crate::{
    names::{InDb, NameRef},
    ResolverDatabase,
};

#[derive(Debug, PartialEq, Eq)]
pub enum ExportItem {
    ExportValue(NameRef),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ModuleExports {
    items: Vec<ExportItem>,
    explicit: bool,
}

impl ModuleExports {
    pub fn is_value_exported(&self, v: impl AsRef<str>) -> bool {
        self.items.iter().any(|export_item| match export_item {
            ExportItem::ExportValue(i) => i.as_ref() == v.as_ref(),
        })
    }
}

impl ModuleExports {
    pub(crate) fn module_exports_query(
        db: &dyn ResolverDatabase,
        file_id: FileId,
    ) -> Arc<ModuleExports> {
        let items;
        let explicit;

        let node = db.parse_file(file_id);
        let nominal_map = db.nominal_map(file_id);
        let export_list = ast::Source::<ast::Module>::cast(node).and_then(|source| {
            Some(source.child()?.header()?.export_list()?.child()?.child()?.children())
        });

        if let Some(export_list) = export_list {
            items = export_list
                .filter_map(|export_item| match export_item {
                    ast::ExportItem::ExportValue(v) => {
                        Some(ExportItem::ExportValue(v.name_ref()?.in_db(db)?))
                    }
                })
                .collect();
            explicit = true
        } else {
            items = nominal_map
                .value_groups()
                .map(|(_, value_group)| {
                    // FIXME: use interned names for ValueGroup
                    let name = NameRef::from_raw(db.interner().intern(&value_group.name));
                    ExportItem::ExportValue(name)
                })
                .collect();
            explicit = false;
        }

        Arc::new(ModuleExports { items, explicit })
    }
}
