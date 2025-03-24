use smol_str::SmolStr;
use syntax::cst;

use crate::{
    ImplicitItems, ImportExportKind, ImportItemId, ImportItems, ImportedTerms, ImportedTypes,
    IndexError,
};

use super::{common::extract_module_name, State};

pub(super) fn index(state: &mut State, cst: &cst::ImportStatement) {
    let id = state.source.allocate_import_statement(cst);

    let name = extract_name(cst);
    let alias = extract_alias(cst);

    let mut import_items = ImportItems::new(name, alias);

    if let Some(import_list) = cst.import_list() {
        import_items.kind = ImportExportKind::Explicit;
        for import in import_list.children() {
            index_import(state, &mut import_items.terms, &mut import_items.types, &import);
        }
    }

    state.index.insert_import_items(id, import_items);
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

fn extract_name(cst: &cst::ImportStatement) -> Option<SmolStr> {
    let cst = cst.module_name()?;
    extract_module_name(&cst)
}

fn extract_alias(cst: &cst::ImportStatement) -> Option<SmolStr> {
    let cst = cst.import_alias()?;
    let cst = cst.module_name()?;
    extract_module_name(&cst)
}
