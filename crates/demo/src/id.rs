//! IDs are abstractions over pointers.
//!
//! ## Terminlogy
//! * CST - an untyped syntax tree, basically [`rowan`]'s types.
//! * AST - a typed syntax tree, basically types with [`AstNode`].

use std::marker::PhantomData;

use files::FileId;
use la_arena::Idx;
use rowan::ast::{AstNode, AstPtr};
use syntax::{PureScript, SyntaxNodePtr};

use crate::surface::SurfaceDatabase;

/// An ID for a CST pointer.
pub type CstId = Idx<SyntaxNodePtr>;

/// An ID for an AST pointer.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AstId<N: AstNode<Language = PureScript>> {
    pub(crate) raw: CstId,
    _marker: PhantomData<fn() -> N>,
}

impl<N: AstNode<Language = PureScript>> Clone for AstId<N> {
    fn clone(&self) -> Self {
        Self { raw: self.raw.clone(), _marker: PhantomData::default() }
    }
}

impl<N: AstNode<Language = PureScript>> Copy for AstId<N> {}

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

impl<N: AstNode<Language = PureScript>> InFileAstId<N> {
    pub fn ast_ptr(&self, db: &dyn SurfaceDatabase) -> AstPtr<N> {
        db.positional_map(self.file_id).get(self.value)
    }
}

/// A value that exists within a [`FileId`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InFile<T> {
    pub file_id: FileId,
    pub value: T,
}
