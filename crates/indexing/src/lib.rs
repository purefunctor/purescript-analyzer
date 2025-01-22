mod algorithm;
mod error;
mod id;
mod indexes;
mod sourcemap;
mod wellformed;

use std::sync::Arc;

pub use error::*;
pub use id::*;
pub use indexes::*;
pub use sourcemap::*;

use syntax::cst;

pub struct IndexingResult {
    pub source_map: SourceMap,
    pub nominal: NominalIndex,
    pub relational: RelationalIndex,
}

impl IndexingResult {
    pub fn find_declaration(&self, name: &str) -> Option<DeclarationPtr> {
        match self.nominal.lookup_expr_item(name) {
            None => None,
            Some((_, item)) => match item { 
                ExprItem::Operator(id) => self.source_map.declaration_ptr(*id),
                _ => None,
            },
        }
    }
}

pub type IndexingErrors = Arc<[IndexingError]>;

pub fn index(module: &cst::Module) -> (IndexingResult, IndexingErrors) {
    let (index, errors) = algorithm::index_module(module);
    let errors = wellformed::check_index(&index, errors);
    (index, errors)
}
