mod algorithm;

use fxhash::{FxBuildHasher, FxHashMap};
use la_arena::{Arena, ArenaMap, Idx};
use petgraph::{prelude::GraphMap, Directed};
use smol_str::SmolStr;
use syntax::{cst, SyntaxNode, SyntaxNodePtr};

pub type DeclarationId = Idx<cst::Declaration>;

#[derive(Debug, PartialEq, Eq)]
pub struct ClassGroup {
    pub signature: Option<DeclarationId>,
    pub declaration: Option<DeclarationId>,
}

#[derive(Debug, Default)]
pub struct ClassIndex {
    pub by_type: FxHashMap<SmolStr, ClassGroup>,
    pub by_member: FxHashMap<SmolStr, DeclarationId>,
    pub statement_graph: GraphMap<DeclarationId, (), Directed, FxBuildHasher>,
}

#[derive(Debug, Default)]
pub struct InstanceIndex {
    pub by_type: FxHashMap<SmolStr, Vec<DeclarationId>>,
    pub by_name: FxHashMap<SmolStr, DeclarationId>,
    pub instance_graph: GraphMap<DeclarationId, (), Directed, FxBuildHasher>,
    pub statement_graph: GraphMap<DeclarationId, (), Directed, FxBuildHasher>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ValueGroup {
    pub signature: Option<DeclarationId>,
    pub equations: Vec<DeclarationId>,
}

pub type ValueIndex = FxHashMap<SmolStr, ValueGroup>;

#[derive(Debug, Default)]
pub struct FullIndexingResult {
    arena: Arena<cst::Declaration>,
    pub pointer: ArenaMap<DeclarationId, SyntaxNodePtr>,
    pub class: ClassIndex,
    pub instance: InstanceIndex,
    pub value: ValueIndex,
    pub errors: Vec<IndexingError>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum IndexingError {
    SignatureConflict { existing: DeclarationId, duplicate: DeclarationId },
    SignatureIsLate { equation: DeclarationId, signature: DeclarationId },
    DeclarationConflict { existing: DeclarationId, duplicate: DeclarationId },
}

pub fn index(node: SyntaxNode) -> FullIndexingResult {
    let mut module_map = FullIndexingResult::default();
    algorithm::index_module(&mut module_map, node);
    module_map
}
