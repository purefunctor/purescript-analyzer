//! IDs used as query keys.

use std::marker::PhantomData;

use la_arena::Idx;
use rowan::ast::AstNode;
use syntax::{PureScript, SyntaxNodePtr};

/// See documentation for [`PositionalMap`].
///
/// [`PositionalMap`]: crate::resolver::PositionalMap
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AstId<N: AstNode<Language = PureScript>> {
    pub(crate) raw: Idx<SyntaxNodePtr>,
    _marker: PhantomData<fn() -> N>,
}

impl<N: AstNode<Language = PureScript>> AstId<N> {
    pub(crate) fn new(raw: Idx<SyntaxNodePtr>) -> AstId<N> {
        AstId { raw, _marker: PhantomData }
    }
}
