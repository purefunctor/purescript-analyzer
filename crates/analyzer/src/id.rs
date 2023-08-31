//! IDs used as query keys.

use std::marker::PhantomData;

use files::FileId;
use la_arena::Idx;
use rowan::ast::AstNode;
use syntax::{PureScript, SyntaxNodePtr};

/// See documentation for [`PositionalMap`].
///
/// [`PositionalMap`]: crate::resolver::PositionalMap
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstId<N: AstNode<Language = PureScript>> {
    pub(crate) raw: Idx<SyntaxNodePtr>,
    _marker: PhantomData<fn() -> N>,
}

impl<N: AstNode<Language = PureScript>> AstId<N> {
    pub(crate) fn new(raw: Idx<SyntaxNodePtr>) -> AstId<N> {
        AstId { raw, _marker: PhantomData }
    }

    pub fn in_file(self, file_id: FileId) -> InFile<AstId<N>> {
        InFile { file_id, value: self }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InFile<T> {
    pub(crate) file_id: FileId,
    pub(crate) value: T,
}
