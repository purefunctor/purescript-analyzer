//! IDs used as query keys.

use std::{hash::Hash, marker::PhantomData, sync::Arc};

use files::FileId;
use la_arena::Idx;
use rowan::ast::AstNode;
use rustc_hash::FxHashSet;
use syntax::{PureScript, SyntaxNodePtr};

use crate::SurfaceDatabase;

/// See documentation for [`PositionalMap`].
///
/// [`PositionalMap`]: crate::resolver::PositionalMap
#[derive(Debug)]
pub struct AstId<N: AstNode<Language = PureScript>> {
    pub(crate) raw: Idx<SyntaxNodePtr>,
    _marker: PhantomData<fn() -> N>,
}

impl<N: AstNode<Language = PureScript>> Clone for AstId<N> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<N: AstNode<Language = PureScript>> Copy for AstId<N> {}

impl<N: AstNode<Language = PureScript>> PartialEq for AstId<N> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<N: AstNode<Language = PureScript>> Eq for AstId<N> {}

impl<N: AstNode<Language = PureScript>> PartialOrd for AstId<N> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<N: AstNode<Language = PureScript>> Ord for AstId<N> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.raw.cmp(&other.raw)
    }
}

impl<N: AstNode<Language = PureScript>> Hash for AstId<N> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.raw.hash(state);
    }
}

impl<N: AstNode<Language = PureScript>> AstId<N> {
    pub(crate) fn new(raw: Idx<SyntaxNodePtr>) -> AstId<N> {
        AstId { raw, _marker: PhantomData }
    }

    pub fn in_file(self, file_id: FileId) -> InFile<AstId<N>> {
        InFile { file_id, value: self }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InFile<T> {
    pub(crate) file_id: FileId,
    pub(crate) value: T,
}

impl<T> InFile<AstId<T>>
where
    T: AstNode<Language = PureScript>,
{
    pub fn to_ast(self, db: &dyn SurfaceDatabase) -> T {
        let root = db.parse_file(self.file_id);
        let ptr = db.positional_map(self.file_id).ast_ptr(self.value);
        ptr.to_node(&root)
    }
}

#[derive(Debug)]
pub struct GroupAstId<N: AstNode<Language = PureScript>> {
    pub(crate) raw: Idx<FxHashSet<Idx<SyntaxNodePtr>>>,
    _marker: PhantomData<fn() -> N>,
}

impl<N: AstNode<Language = PureScript>> Clone for GroupAstId<N> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<N: AstNode<Language = PureScript>> Copy for GroupAstId<N> {}

impl<N: AstNode<Language = PureScript>> PartialEq for GroupAstId<N> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<N: AstNode<Language = PureScript>> Eq for GroupAstId<N> {}

impl<N: AstNode<Language = PureScript>> PartialOrd for GroupAstId<N> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<N: AstNode<Language = PureScript>> Ord for GroupAstId<N> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.raw.cmp(&other.raw)
    }
}

impl<N: AstNode<Language = PureScript>> Hash for GroupAstId<N> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.raw.hash(state);
    }
}

impl<N: AstNode<Language = PureScript>> GroupAstId<N> {
    pub(crate) fn new(raw: Idx<FxHashSet<Idx<SyntaxNodePtr>>>) -> GroupAstId<N> {
        GroupAstId { raw, _marker: PhantomData }
    }

    pub fn in_file(self, file_id: FileId) -> InFile<GroupAstId<N>> {
        InFile { file_id, value: self }
    }
}
