use rowan::ast::AstNode;

use crate::{PureScript, SyntaxKind, SyntaxNode, SyntaxToken};

#[derive(Debug)]
pub struct ModuleName {
    syntax: SyntaxNode,
}

impl ModuleName {
    /// Invariant: [`SyntaxToken::kind()`] is always [`SyntaxKind::Upper`].
    pub fn segments(&self) -> impl Iterator<Item = SyntaxToken> {
        self.syntax.children_with_tokens().filter_map(|element| {
            element.into_token().and_then(|token| {
                if matches!(token.kind(), SyntaxKind::Upper) {
                    Some(token)
                } else {
                    None
                }
            })
        })
    }
}

impl AstNode for ModuleName {
    type Language = PureScript;

    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized,
    {
        matches!(kind, SyntaxKind::ModuleName)
    }

    fn cast(node: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        if Self::can_cast(node.kind()) {
            Some(Self { syntax: node })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
