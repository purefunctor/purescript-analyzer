//! See documentation for [`Exports`].

use std::sync::Arc;

use files::FileId;
use rowan::ast::AstNode;
use rustc_hash::FxHashSet;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    names::NameRef,
    ResolverDatabase,
};

#[derive(Debug, PartialEq, Eq)]
pub struct Exports {
    file_id: FileId,
    items: Option<ExportItems>,
}

#[derive(Debug, Default, PartialEq, Eq)]
struct ExportItems {
    values: FxHashSet<NameRef>,
}

impl ExportItems {
    fn collect_export_item(&mut self, export_item: &ast::ExportItem) -> Option<()> {
        match export_item {
            ast::ExportItem::ExportValue(value) => {
                let name_ref = NameRef::try_from(value.name_ref()?).ok()?;
                self.values.insert(name_ref);
            }
        }
        Some(())
    }
}

impl Exports {
    pub(crate) fn exports_query(db: &dyn ResolverDatabase, file_id: FileId) -> Arc<Exports> {
        let node = db.parse_file(file_id);
        let export_items = ast::Source::<ast::Module>::cast(node).and_then(|source| {
            Some(source.child()?.header()?.export_list()?.child()?.child()?.children())
        });

        let items = export_items.map(|export_items| {
            let mut items = ExportItems::default();
            for export_item in export_items {
                items.collect_export_item(&export_item);
            }
            items
        });

        Arc::new(Exports { file_id, items })
    }

    pub fn lookup_value(
        &self,
        db: &dyn ResolverDatabase,
        name: &NameRef,
    ) -> Option<Arc<[InFile<AstId<ast::ValueDeclaration>>]>> {
        if self.items.as_ref()?.values.contains(name) {
            db.nominal_map(self.file_id).get_value(name)
        } else {
            None
        }
    }
}
