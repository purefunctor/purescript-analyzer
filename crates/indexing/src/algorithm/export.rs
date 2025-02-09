use smol_str::{SmolStr, SmolStrBuilder};
use syntax::cst;

use crate::{ExportItemId, IndexError, TypeItem, TypeItemId};

use super::State;

pub(super) fn index(state: &mut State, cst: &cst::ExportItem) {
    let id = state.source.allocate_export(cst);
    match cst {
        cst::ExportItem::ExportValue(v) => {
            let Some(name) = v.name_token() else { return };
            let name = name.text();
            index_term_export(state, id, name);
        }
        cst::ExportItem::ExportClass(c) => {
            let Some(name) = c.name_token() else { return };
            let name = name.text();
            index_type_export(state, id, name, None);
        }
        cst::ExportItem::ExportType(t) => {
            let Some(name) = t.name_token() else { return };
            let name = name.text();
            index_type_export(state, id, name, t.type_items());
        }
        cst::ExportItem::ExportOperator(o) => {
            let Some(name) = o.name_token() else { return };
            let name = name.text();
            let Some(name) = operator_name(name) else { return };
            index_term_export(state, id, name);
        }
        cst::ExportItem::ExportTypeOperator(o) => {
            let Some(name) = o.name_token() else { return };
            let name = name.text();
            let Some(name) = operator_name(name) else { return };
            index_type_export(state, id, name, None);
        }
        cst::ExportItem::ExportModule(m) => {
            index_module_export(&m);
        }
    }
}

fn operator_name(name: &str) -> Option<&str> {
    let mut chars = name.chars();
    if chars.next() != Some('(') {
        return None;
    }
    if chars.next_back() != Some(')') {
        return None;
    }
    Some(chars.as_str())
}

fn index_term_export(state: &mut State, id: ExportItemId, name: &str) {
    let Some((_, item_id)) = state.index.term_item_mut(name) else {
        return state.error.push(IndexError::InvalidExport { id });
    };
    let Some(existing) = state.index.term_item_export(item_id) else {
        return state.index.export_term_item(item_id, id);
    };

    state.error.push(IndexError::DuplicateExport { id, existing });
}

fn index_type_export(
    state: &mut State,
    id: ExportItemId,
    name: &str,
    items: Option<cst::TypeItems>,
) {
    let Some((item, item_id)) = state.index.type_item_mut(name) else {
        return state.error.push(IndexError::InvalidExport { id });
    };

    if let Some(items) = items {
        if let TypeItem::Data { .. } | TypeItem::Newtype { .. } = item {
            index_item_export(state, id, item_id, &items);
        } else {
            state.error.push(IndexError::InvalidExport { id });
        }
    }

    let Some(existing) = state.index.type_item_export(item_id) else {
        return state.index.export_type_item(item_id, id);
    };

    state.error.push(IndexError::DuplicateExport { id, existing });
}

fn index_item_export(
    state: &mut State,
    id: ExportItemId,
    item_id: TypeItemId,
    items: &cst::TypeItems,
) {
    match items {
        cst::TypeItems::TypeItemsAll(_) => {
            for item_id in state.relational.constructors_of(item_id) {
                if let Some(existing) = state.index.term_item_export(item_id) {
                    state.error.push(IndexError::DuplicateExport { id, existing });
                } else {
                    state.index.export_term_item(item_id, id);
                }
            }
        }
        cst::TypeItems::TypeItemsList(cst) => {
            for token in cst.name_tokens() {
                let name = token.text();
                index_term_export(state, id, name);
            }
        }
    }
}

fn index_module_export(cst: &cst::ExportModule) -> Option<SmolStr> {
    let module_name = cst.module_name()?;

    let mut buffer = SmolStrBuilder::default();
    if let Some(qualifier) = module_name.qualifier() {
        if let Some(token) = qualifier.text() {
            buffer.push_str(token.text());
        }
    }

    let token = module_name.name_token()?;
    buffer.push_str(token.text());

    Some(buffer.finish())
}
