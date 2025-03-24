mod common;
mod declaration;
mod export;
mod import;

use smol_str::SmolStr;
use syntax::cst;

use crate::{ExportKind, Index, IndexError, IndexingSource, Relational};

#[derive(Debug)]
pub(super) struct State {
    name: Option<SmolStr>,
    pub(super) index: Index,
    pub(super) relational: Relational,
    pub(super) source: IndexingSource,
    pub(super) error: Vec<IndexError>,
}

impl State {
    fn new(name: Option<SmolStr>) -> State {
        let index = Index::default();
        let relational = Relational::default();
        let source = IndexingSource::default();
        let error = vec![];
        State { name, index, relational, source, error }
    }
}

pub(super) fn index_module(module: &cst::Module) -> State {
    let name = module.header().and_then(|cst| {
        let name = cst.name()?;
        common::extract_module_name(&name)
    });

    let mut state = State::new(name);

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
            state.index.export_kind = ExportKind::Explicit;
            for export in exports.children() {
                export::index(&mut state, &export);
            }
        }
    }

    state
}
