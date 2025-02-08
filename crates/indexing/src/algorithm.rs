mod declaration;

use declaration::index_declaration;
use syntax::cst;

use crate::{Index, IndexError, Source};

#[derive(Debug, Default)]
pub(super) struct State {
    pub(super) index: Index,
    pub(super) source: Source,
    pub(super) error: Vec<IndexError>,
}

pub(super) fn index_module(module: &cst::Module) -> State {
    let mut state = State::default();

    if let Some(statements) = module.statements() {
        for declaration in statements.children() {
            index_declaration(&mut state, &declaration);
        }
    }

    state
}
