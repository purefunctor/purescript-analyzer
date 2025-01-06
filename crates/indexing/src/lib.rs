mod algorithm;

use fxhash::FxHashMap;
use la_arena::{Arena, ArenaMap, Idx};
use smol_str::SmolStr;
use syntax::{cst, SyntaxNode, SyntaxNodePtr};

pub type DeclarationId = Idx<cst::Declaration>;

#[derive(Debug, PartialEq, Eq)]
pub struct DeclarationGroup {
    pub signature: Option<DeclarationId>,
    pub declarations: Vec<DeclarationId>,
}

#[derive(Debug, Default)]
pub struct FullIndexingResult {
    arena: Arena<cst::Declaration>,
    pub pointer: ArenaMap<DeclarationId, SyntaxNodePtr>,
    pub value: FxHashMap<SmolStr, DeclarationGroup>,
    pub errors: Vec<IndexingError>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum IndexingError {
    SignatureConflict { existing: DeclarationId, duplicate: DeclarationId },
    SignatureIsLate { equation: DeclarationId, signature: DeclarationId },
}

pub fn index(node: SyntaxNode) -> FullIndexingResult {
    let mut module_map = FullIndexingResult::default();
    algorithm::index_module(&mut module_map, node);
    module_map
}
