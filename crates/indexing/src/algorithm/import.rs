use rustc_hash::FxHashMap;
use smol_str::{SmolStr, SmolStrBuilder};
use syntax::cst;

use crate::{ImplicitItems, ImportItemId, ImportedItems, ImportedTerms, ImportedTypes, IndexError};

use super::State;

pub(super) fn index(state: &mut State, cst: &cst::ImportStatement) {
    let id = state.source.allocate_import_statement(cst);

    if let Some(imports) = cst.import_list() {
        let mut terms = FxHashMap::default();
        let mut types = FxHashMap::default();
        for import in imports.children() {
            index_import(state, &mut terms, &mut types, &import);
        }
        let imported_items = ImportedItems { terms, types, exported: false };
        state.index.insert_imported_items(id, imported_items);
    }

    if let Some(alias) = extract_alias(cst) {
        state.index.insert_import_alias(alias, id);
    }
}

fn index_import(
    state: &mut State,
    terms: &mut ImportedTerms,
    types: &mut ImportedTypes,
    cst: &cst::ImportItem,
) {
    let id = state.source.allocate_import(cst);
    match cst {
        cst::ImportItem::ImportValue(v) => {
            let Some(token) = v.name_token() else { return };
            let name = token.text();
            index_term_import(state, terms, name, id);
        }
        cst::ImportItem::ImportClass(c) => {
            let Some(token) = c.name_token() else { return };
            let name = token.text();
            index_type_import(state, terms, types, name, id, None);
        }
        cst::ImportItem::ImportType(t) => {
            let Some(token) = t.name_token() else { return };
            let name = token.text();
            index_type_import(state, terms, types, name, id, t.type_items());
        }
        cst::ImportItem::ImportOperator(o) => {
            let Some(token) = o.name_token() else { return };
            let name = token.text();
            index_term_import(state, terms, name, id);
        }
        cst::ImportItem::ImportTypeOperator(o) => {
            let Some(token) = o.name_token() else { return };
            let name = token.text();
            index_type_import(state, terms, types, name, id, None);
        }
    }
}

fn index_term_import(state: &mut State, terms: &mut ImportedTerms, name: &str, id: ImportItemId) {
    if let Some(&existing) = terms.get(name) {
        state.error.push(IndexError::DuplicateImport { id, existing });
    } else {
        let name = SmolStr::from(name);
        terms.insert(name, id);
    }
}

fn index_type_import(
    state: &mut State,
    terms: &mut ImportedTerms,
    types: &mut ImportedTypes,
    name: &str,
    id: ImportItemId,
    items: Option<cst::TypeItems>,
) {
    let items = items.map(|items| index_type_items(state, terms, id, items));
    if let Some((existing, _)) = types.get(name) {
        let existing = *existing;
        state.error.push(IndexError::DuplicateImport { id, existing });
    } else {
        let name = SmolStr::from(name);
        types.insert(name, (id, items));
    }
}

fn index_type_items(
    state: &mut State,
    terms: &mut ImportedTerms,
    id: ImportItemId,
    items: cst::TypeItems,
) -> ImplicitItems {
    match items {
        cst::TypeItems::TypeItemsAll(_) => ImplicitItems::Everything,
        cst::TypeItems::TypeItemsList(cst) => {
            let enumerated = cst.name_tokens().map(|token| {
                let name = token.text();
                index_term_import(state, terms, name, id);
                SmolStr::from(name)
            });
            let enumerated = enumerated.collect();
            ImplicitItems::Enumerated(enumerated)
        }
    }
}

fn extract_alias(cst: &cst::ImportStatement) -> Option<SmolStr> {
    let cst = cst.import_alias()?;
    let cst = cst.module_name()?;

    let mut buffer = SmolStrBuilder::default();
    if let Some(token) = cst.qualifier().and_then(|cst| cst.text()) {
        buffer.push_str(token.text());
    }

    let token = cst.name_token()?;
    buffer.push_str(token.text());

    Some(buffer.finish())
}
