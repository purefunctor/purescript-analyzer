mod algorithm;

use fxhash::{FxBuildHasher, FxHashMap};
use hashbrown::HashMap;
use indexmap::{IndexMap, IndexSet};
use la_arena::{Arena, Idx, RawIdx};
use petgraph::{prelude::GraphMap, Directed};
use smol_str::SmolStr;
use syntax::{cst, SyntaxNode};

pub type DeclarationId = Idx<cst::Declaration>;
pub type DeclarationPtr = rowan::ast::AstPtr<cst::Declaration>;

pub type ConstructorId = Idx<cst::DataConstructor>;
pub type ConstructorPtr = rowan::ast::AstPtr<cst::DataConstructor>;

#[derive(Debug, PartialEq, Eq)]
pub struct ClassGroup {
    /// [`cst::ClassSignature`]
    pub signature: Option<DeclarationId>,
    /// [`cst::ClassDeclaration`]
    pub declaration: Option<DeclarationId>,
}

#[derive(Debug, Default)]
pub struct ClassIndex {
    /// From class name to class group
    pub by_name: FxHashMap<SmolStr, ClassGroup>,
    /// From member name to [`cst::ClassMemberStatement`]
    pub by_member: FxHashMap<SmolStr, DeclarationId>,
    /// From [`cst::ClassDeclaration`] to [`cst::ClassMemberStatement`]
    pub statement_graph: GraphMap<DeclarationId, (), Directed, FxBuildHasher>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct InstanceStatementGroup {
    /// [`cst::InstanceSignatureStatement`]
    pub signature: Option<DeclarationId>,
    /// [`cst::InstanceEquationStatement`]
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
    /// From class name to instance chains
    pub by_class: FxHashMap<SmolStr, Vec<DeclarationId>>,
    /// From instance name to [`cst::InstanceDeclaration`]
    pub by_name: FxHashMap<SmolStr, DeclarationId>,
    /// From [`cst::InstanceChain`] to [`cst::InstanceDeclaration`]
    pub instance_graph: GraphMap<DeclarationId, (), Directed, FxBuildHasher>,
    /// From [`cst::InstanceDeclaration`] to [`cst::InstanceChain`]
    pub statement_graph: GraphMap<DeclarationId, (), Directed, FxBuildHasher>,
    /// From member name and [`cst::InstanceDeclaration`] to [`InstanceStatementGroup`]
    pub statement_group: HashMap<InstanceStatementGroupKey, InstanceStatementGroup, FxBuildHasher>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct DataTypeGroup {
    /// [`cst::NewtypeSignature`] or [`cst::DataSignature`]
    pub signature: Option<DeclarationId>,
    /// [`cst::NewtypeEquation`] or [`cst::DataEquation`]
    pub equation: Option<DeclarationId>,
}

#[derive(Debug, Default)]
pub struct DataTypeIndex {
    /// From data type name to data type group
    pub by_name: IndexMap<SmolStr, DataTypeGroup, FxBuildHasher>,
    /// From constructor name to [`cst::DataConstructor`]
    pub by_constructor: FxHashMap<SmolStr, ConstructorId>,
    /// From [`cst::DataConstructor`] to [`by_name`] index
    ///
    /// [`by_name`]: DataTypeIndex::by_name
    pub constructor_of: FxHashMap<ConstructorId, usize>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SynonymGroup {
    /// [`cst::TypeSynonymSignature`]
    pub signature: Option<DeclarationId>,
    /// [`cst::TypeSynonymEquation`]
    pub equation: Option<DeclarationId>,
}

pub type SynonymIndex = FxHashMap<SmolStr, SynonymGroup>;

#[derive(Debug, PartialEq, Eq)]
pub struct ValueGroup {
    /// [`cst::ValueSignature`]
    pub signature: Option<DeclarationId>,
    /// [`cst::ValueEquation`]
    pub equations: Vec<DeclarationId>,
}

pub type ValueIndex = FxHashMap<SmolStr, ValueGroup>;

#[derive(Debug, Default)]
pub struct FullIndexingResult {
    declarations: Arena<cst::Declaration>,
    declarations_pointers: IndexSet<DeclarationPtr, FxBuildHasher>,
    constructors: Arena<cst::DataConstructor>,
    constructors_pointers: IndexSet<ConstructorPtr, FxBuildHasher>,
    pub data: DataTypeIndex,
    pub class: ClassIndex,
    pub instance: InstanceIndex,
    pub synonym: SynonymIndex,
    pub value: ValueIndex,
    pub errors: Vec<IndexingError>,
}

impl FullIndexingResult {
    pub fn pointer(&self, index: DeclarationId) -> Option<DeclarationPtr> {
        let index: usize = index.into_raw().into_u32() as usize;
        self.declarations_pointers.get_index(index).cloned()
    }

    pub fn index(&self, pointer: DeclarationPtr) -> Option<DeclarationId> {
        let index = self.declarations_pointers.get_full(&pointer)?.0 as u32;
        Some(DeclarationId::from_raw(RawIdx::from_u32(index)))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum IndexingError {
    SignatureConflict { existing: DeclarationId, duplicate: DeclarationId },
    SignatureIsLate { declaration: DeclarationId, signature: DeclarationId },
    DeclarationConflict { existing: DeclarationId, duplicate: DeclarationId },
    ConstructorConflict { existing: ConstructorId, duplicate: ConstructorId },
}

pub fn index(node: SyntaxNode) -> FullIndexingResult {
    let mut module_map = FullIndexingResult::default();
    algorithm::index_module(&mut module_map, node);
    module_map
}
