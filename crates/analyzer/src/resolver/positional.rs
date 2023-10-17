//! See documentation for [`PositionalMap`].

use std::sync::Arc;

use files::FileId;
use la_arena::{Arena, Idx};
use rowan::ast::{AstNode, AstPtr};
use syntax::{ast, PureScript, SyntaxNode, SyntaxNodePtr};

use crate::{id::AstId, ResolverDatabase};

/// Assigns [`AstId`]s to [`SyntaxNodePtr`]s.
///
/// [`AstId`]s serve the purpose of identifying syntactic elements without
/// invalidation by trivial changes to the source file like the insertion
/// of whitespace or comments. When combined with a [`FileId`], it can be
/// used to identify a specific syntactic element within a file, such as
/// declarations.
///
/// Queries such as type inference can then be designed to accept these
/// IDs, and internally, they can consult the positional map for syntactic
/// information. IDs allow queries to be cache-friendly in their arguments
/// versus if we were to implement them around line/column information.
///
/// But if queries consult the positional map, and the positional map
/// is sensitive to the contents of the source file, wouldn't that mean
/// that the dependent queries would be invalidated as well? Yes and no.
///
/// In this query structure, the answer would be yes. `Infer` would be
/// invalidated as often as `PositionalMap`.
///
/// ```ignore
/// Infer <- PositionalMap
/// ```
///
/// One thing to note about salsa is that if a query returns the same
/// result as was previously received by its dependents, then those
/// dependents would not need to be invalidated. Take for example the
/// following query structure:
///
/// ```ignore
/// Infer <- Lower <- PositionalMap
/// ```
///
/// `Lower` is invalidated as often as `PositionalMap`, however, if
/// it returns the same value that `Infer` had received, then `Infer`
/// would not need to be invalidated. Assuming that `Lower`'s return
/// value is cache-friendly in that it makes no mention of highly
/// volatile line/column information, resilience is preserved.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct PositionalMap {
    inner: Arena<SyntaxNodePtr>,
}

impl PositionalMap {
    pub(crate) fn positional_map_query(
        db: &dyn ResolverDatabase,
        file_id: FileId,
    ) -> Arc<PositionalMap> {
        let mut positional_map = PositionalMap::default();

        let node = db.parse_file(file_id);
        let declarations = ast::Source::<ast::Module>::cast(node)
            .and_then(|source| Some(source.child()?.body()?.declarations()?.children()));
        if let Some(declarations) = declarations {
            for declaration in declarations {
                match declaration {
                    ast::Declaration::AnnotationDeclaration(annotation) => {
                        positional_map.alloc(annotation.syntax());
                    }
                    ast::Declaration::ForeignDataDeclaration(data) => {
                        positional_map.alloc(data.syntax());
                    }
                    ast::Declaration::ValueDeclaration(value) => {
                        positional_map.alloc(value.syntax());
                    }
                }
            }
        }

        Arc::new(positional_map)
    }

    pub fn ast_id<N: AstNode<Language = PureScript>>(&self, node: &N) -> AstId<N> {
        let target = SyntaxNodePtr::new(node.syntax());
        self.inner
            .iter()
            .find_map(|(raw, inner)| if &target == inner { Some(AstId::new(raw)) } else { None })
            .unwrap()
    }

    pub fn ast_ptr<N: AstNode<Language = PureScript>>(&self, id: AstId<N>) -> AstPtr<N> {
        self.inner[id.raw].clone().cast().unwrap()
    }

    fn alloc(&mut self, node: &SyntaxNode) -> Idx<SyntaxNodePtr> {
        self.inner.alloc(SyntaxNodePtr::new(node))
    }
}
