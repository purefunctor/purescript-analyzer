mod declaration;
mod export;
mod import;

use syntax::cst;

use crate::{Index, IndexError, IndexingSource, Relational};

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
            import::index(&mut state, &import);
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
