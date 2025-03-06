use smol_str::{SmolStr, SmolStrBuilder};
use syntax::cst;

use super::State;

pub(super) fn index(state: &mut State, cst: &cst::ImportStatement) {
    let import_id = state.source.allocate_import_statement(cst);

    if let Some(imports) = cst.import_list() {
        for import in imports.children() {
            index_import(state, &import);
        }
    }

    if let Some(alias) = extract_alias(cst) {
        state.index.insert_import_alias(alias, import_id);
    }
}

fn index_import(state: &mut State, import: &cst::ImportItem) {
    dbg!(import);
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
