//! An index from relative positions to stable IDs.

use std::sync::Arc;

use files::FileId;
use la_arena::{Arena, Idx};
use rowan::ast::{AstNode, AstPtr};
use syntax::{ast, PureScript, SyntaxNode, SyntaxNodePtr};

use crate::{id::AstId, IndexDatabase};

/// An index from relative positions to stable IDs.
///
/// More specifically, this provides stable IDs, [`AstId`], as a way to refer
/// to nodes in the AST, but without explicit position information unlike the
/// [`SyntaxNodePtr`] type.
///
/// Stable IDs like [`AstId`] are much more suited for use as query keys since
/// they're less prone to cache invalidation by trivial changes to the source
/// file like the insertion of whitespace or comments.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct PositionalMap {
    inner: Arena<SyntaxNodePtr>,
}

impl PositionalMap {
    pub fn ast_id<N: AstNode<Language = PureScript>>(&self, node: &N) -> AstId<N> {
        let target = SyntaxNodePtr::new(node.syntax());
        self.inner
            .iter()
            .find_map(|(raw, inner)| if &target == inner { Some(AstId::new(raw)) } else { None })
            .unwrap()
    }

    pub fn ast_ptr<N: AstNode<Language = PureScript>>(&self, id: AstId<N>) -> AstPtr<N> {
        self.inner[id.raw].cast().unwrap()
    }

    fn alloc(&mut self, node: &SyntaxNode) -> Idx<SyntaxNodePtr> {
        self.inner.alloc(SyntaxNodePtr::new(node))
    }
}

pub(super) fn positional_map_query(db: &dyn IndexDatabase, file_id: FileId) -> Arc<PositionalMap> {
    let mut positional_map = PositionalMap::default();

    let node = db.parse_file(file_id);
    let declarations = ast::Source::<ast::Module>::cast(node)
        .and_then(|source| Some(source.child()?.body()?.declarations()?.children()));

    if let Some(declarations) = declarations {
        for declaration in declarations {
            match declaration {
                ast::Declaration::ClassDeclaration(class) => {
                    positional_map.alloc(class.syntax());
                    if let Some(members) = class.members() {
                        for member in members.children() {
                            positional_map.alloc(member.syntax());
                        }
                    }
                }
                ast::Declaration::ClassSignature(class) => {
                    positional_map.alloc(class.syntax());
                }
                ast::Declaration::DataAnnotation(data) => {
                    positional_map.alloc(data.syntax());
                }
                ast::Declaration::DataDeclaration(data) => {
                    positional_map.alloc(data.syntax());
                    if let Some(constructors) = data.constructors() {
                        for constructor in constructors.children() {
                            positional_map.alloc(constructor.syntax());
                        }
                    }
                }
                ast::Declaration::InstanceChain(chain) => {
                    positional_map.alloc(chain.syntax());
                    for declaration in chain.declarations() {
                        positional_map.alloc(declaration.syntax());
                        if let Some(members) = declaration.members() {
                            for member in members.children() {
                                positional_map.alloc(member.syntax());
                            }
                        }
                    }
                }
                ast::Declaration::ForeignDataDeclaration(data) => {
                    positional_map.alloc(data.syntax());
                }
                ast::Declaration::ValueEquationDeclaration(value) => {
                    positional_map.alloc(value.syntax());
                }
                ast::Declaration::ValueAnnotationDeclaration(annotation) => {
                    positional_map.alloc(annotation.syntax());
                }
            }
        }
    }

    Arc::new(positional_map)
}
