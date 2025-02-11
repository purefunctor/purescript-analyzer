mod declaration;
mod export;

use smol_str::SmolStrBuilder;
use syntax::cst;

use crate::{Index, IndexError, Relational, IndexingSource};

#[derive(Debug, Default)]
pub(super) struct State {
    pub(super) index: Index,
    pub(super) relational: Relational,
    pub(super) source: IndexingSource,
    pub(super) error: Vec<IndexError>,
}

pub(super) fn index_module(module: &cst::Module) -> State {
    let mut state = State::default();

    if let Some(imports) = module.imports() {
        for import in imports.children() {
            index_import(&mut state, &import);
        }
    }

    if let Some(statements) = module.statements() {
        for declaration in statements.children() {
            declaration::index(&mut state, &declaration);
        }
    }

    if let Some(header) = module.header() {
        if let Some(exports) = header.exports() {
            for export in exports.children() {
                export::index(&mut state, &export);
            }
        }
    }

    state
}

fn index_import(state: &mut State, import: &cst::ImportStatement) {
    let import_id = state.source.allocate_import(import);

    let Some(import_alias) = import.import_alias() else { return };
    let Some(module_name) = import_alias.module_name() else { return };

    let mut buffer = SmolStrBuilder::default();
    if let Some(qualifier) = module_name.qualifier() {
        if let Some(token) = qualifier.text() {
            buffer.push_str(token.text());
        }
    }

    let Some(token) = module_name.name_token() else { return };
    buffer.push_str(token.text());

    let name = buffer.finish();
    state.index.insert_import_item(name, import_id);
}
