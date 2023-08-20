use la_arena::{Arena, Idx};
use rowan::ast::{AstNode, AstPtr};
use syntax::{ast, PureScript, SyntaxNode, SyntaxNodePtr};

pub(crate) type DeclarationId = Idx<SyntaxNodePtr>;

/// A mapping from CST pointers to stable IDs.
#[derive(Debug, Default, PartialEq, Eq)]
pub(crate) struct DeclarationMap {
    inner: Arena<SyntaxNodePtr>,
}

impl DeclarationMap {
    /// Constructs a [`DeclarationMap`] from the CST.
    pub(crate) fn from_source(node: &SyntaxNode) -> DeclarationMap {
        let mut declaration_map = DeclarationMap::default();

        let t: ast::Module = ast::Source::cast(node.clone()).unwrap().child().unwrap();
        for t in t.body().unwrap().declarations().unwrap().children() {
            match t {
                ast::Declaration::AnnotationDeclaration(_) => (),
                ast::Declaration::ValueDeclaration(t) => {
                    declaration_map.allocate(&t.syntax());
                }
            }
        }

        declaration_map
    }

    /// Finds the [`DeclarationId`] for a CST pointer.
    pub(crate) fn find<T: AstNode<Language = PureScript>>(&self, t: &T) -> DeclarationId {
        let target_node = SyntaxNodePtr::new(&t.syntax());
        self.inner
            .iter()
            .find_map(
                |(index, inner_node)| {
                    if &target_node == inner_node {
                        Some(index)
                    } else {
                        None
                    }
                },
            )
            .unwrap()
    }

    /// Gets the CST pointer for a [`DeclarationId`].
    pub(crate) fn get<T: AstNode<Language = PureScript>>(
        &self,
        declaration_id: DeclarationId,
    ) -> AstPtr<T> {
        self.inner[declaration_id].clone().cast().unwrap()
    }

    fn allocate(&mut self, node: &SyntaxNode) -> DeclarationId {
        self.inner.alloc(SyntaxNodePtr::new(node))
    }
}
