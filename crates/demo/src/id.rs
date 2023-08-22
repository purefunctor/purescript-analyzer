//! IDs are abstractions over pointers.
//!
//! ## Terminlogy
//! * CST - an untyped syntax tree, basically [`rowan`]'s types.
//! * AST - a typed syntax tree, basically types with [`AstNode`].

use std::marker::PhantomData;

use files::FileId;
use la_arena::Idx;
use rowan::ast::AstNode;
use syntax::{PureScript, SyntaxNodePtr};

/// An ID for a CST pointer.
pub type CstId = Idx<SyntaxNodePtr>;

/// An ID for an AST pointer.
#[derive(Debug, PartialEq, Eq)]
pub struct AstId<N: AstNode<Language = PureScript>> {
    pub(crate) raw: CstId,
    _marker: PhantomData<fn() -> N>,
}

impl<N> AstId<N>
where
    N: AstNode<Language = PureScript>,
{
    pub(crate) fn new(raw: CstId) -> AstId<N> {
        AstId { raw, _marker: PhantomData::default() }
    }

    pub fn in_file(self, file_id: FileId) -> InFileAstId<N> {
        InFile { file_id, value: self }
    }
}

/// An ID for an AST pointer in a specific file.
pub type InFileAstId<N> = InFile<AstId<N>>;

/// A value that exists within a [`FileId`].
#[derive(Debug, PartialEq, Eq)]
pub struct InFile<T> {
    file_id: FileId,
    value: T,
}
