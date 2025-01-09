mod algorithm;

use fxhash::{FxBuildHasher, FxHashMap};
use hashbrown::HashMap;
use indexmap::IndexSet;
use la_arena::{Arena, Idx, RawIdx};
use petgraph::{prelude::GraphMap, Directed};
use smol_str::SmolStr;
use syntax::{cst, SyntaxNode};

pub type DeclarationId = Idx<cst::Declaration>;
pub type DeclarationPtr = rowan::ast::AstPtr<cst::Declaration>;

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

#[derive(Debug, PartialEq, Eq)]
pub struct InstanceStatementGroup {
    pub signature: Option<DeclarationId>,
    pub equations: Vec<DeclarationId>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct InstanceStatementGroupKey(pub DeclarationId, pub SmolStr);

impl hashbrown::Equivalent<InstanceStatementGroupKey> for (DeclarationId, &str) {
    fn equivalent(&self, key: &InstanceStatementGroupKey) -> bool {
        self.0 == key.0 && self.1 == key.1
    }
}

#[derive(Debug, Default)]
pub struct InstanceIndex {
    pub by_type: FxHashMap<SmolStr, Vec<DeclarationId>>,
    pub by_name: FxHashMap<SmolStr, DeclarationId>,
    pub instance_graph: GraphMap<DeclarationId, (), Directed, FxBuildHasher>,
    pub statement_graph: GraphMap<DeclarationId, (), Directed, FxBuildHasher>,
    pub statement_group: HashMap<InstanceStatementGroupKey, InstanceStatementGroup, FxBuildHasher>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SynonymGroup {
    pub signature: Option<DeclarationId>,
    pub equation: Option<DeclarationId>,
}

pub type SynonymIndex = FxHashMap<SmolStr, SynonymGroup>;

#[derive(Debug, PartialEq, Eq)]
pub struct ValueGroup {
    pub signature: Option<DeclarationId>,
    pub equations: Vec<DeclarationId>,
}

pub type ValueIndex = FxHashMap<SmolStr, ValueGroup>;

#[derive(Debug, Default)]
pub struct FullIndexingResult {
    arena: Arena<cst::Declaration>,
    pointer: IndexSet<DeclarationPtr, FxBuildHasher>,
    pub class: ClassIndex,
    pub instance: InstanceIndex,
    pub synonym: SynonymIndex,
    pub value: ValueIndex,
    pub errors: Vec<IndexingError>,
}

impl FullIndexingResult {
    pub fn pointer(&self, index: DeclarationId) -> Option<DeclarationPtr> {
        let index: usize = index.into_raw().into_u32() as usize;
        self.pointer.get_index(index).cloned()
    }

    pub fn index(&self, pointer: DeclarationPtr) -> Option<DeclarationId> {
        let index = self.pointer.get_full(&pointer)?.0 as u32;
        Some(DeclarationId::from_raw(RawIdx::from_u32(index)))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum IndexingError {
    SignatureConflict { existing: DeclarationId, duplicate: DeclarationId },
    SignatureIsLate { declaration: DeclarationId, signature: DeclarationId },
    DeclarationConflict { existing: DeclarationId, duplicate: DeclarationId },
}

pub fn index(node: SyntaxNode) -> FullIndexingResult {
    let mut module_map = FullIndexingResult::default();
    algorithm::index_module(&mut module_map, node);
    module_map
}
