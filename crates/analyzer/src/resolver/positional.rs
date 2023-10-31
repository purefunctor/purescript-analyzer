//! See documentation for [`PositionalMap`].

use std::{mem, sync::Arc};

use files::FileId;
use la_arena::{Arena, Idx};
use rowan::ast::{AstNode, AstPtr};
use rustc_hash::FxHashSet;
use syntax::{ast, PureScript, SyntaxNode, SyntaxNodePtr};

use crate::{
    id::{AstId, GroupAstId},
    ResolverDatabase,
};

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
    group: Arena<FxHashSet<Idx<SyntaxNodePtr>>>,
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

        let mut current_value_name = None;
        let mut current_value_group = FxHashSet::default();

        if let Some(declarations) = declarations {
            for declaration in declarations {
                match &declaration {
                    ast::Declaration::DataDeclaration(data) => {
                        positional_map.alloc(data.syntax());
                        if let Some(constructors) = data.constructors() {
                            for constructor in constructors.children() {
                                positional_map.alloc(constructor.syntax());
                            }
                        }
                    }
                    ast::Declaration::ForeignDataDeclaration(data) => {
                        positional_map.alloc(data.syntax());
                    }
                    ast::Declaration::ValueDeclaration(value) => {
                        if let Some(name) = value.name().and_then(|name| name.as_str()) {
                            let id = positional_map.alloc(value.syntax());

                            if current_value_name.is_none() {
                                current_value_name = Some(name.clone());
                            }

                            if current_value_name.as_ref() != Some(&name) {
                                current_value_name = Some(name);
                                let built_value_group = mem::take(&mut current_value_group);
                                positional_map.alloc_group(built_value_group);
                            }

                            current_value_group.insert(id);
                        } else {
                            positional_map.alloc(value.syntax());
                        }
                    }
                    ast::Declaration::ValueAnnotationDeclaration(annotation) => {
                        if let Some(name) = annotation.name().and_then(|name| name.as_str()) {
                            let id = positional_map.alloc(annotation.syntax());

                            if current_value_name.is_none() {
                                current_value_name = Some(name.clone());
                            }

                            if current_value_name.as_ref() != Some(&name) {
                                current_value_name = Some(name);
                                let built_value_group = mem::take(&mut current_value_group);
                                positional_map.alloc_group(built_value_group);
                            }

                            current_value_group.insert(id);
                        } else {
                            positional_map.alloc(annotation.syntax());
                        }
                    }
                };
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

    pub fn group_ast_id<N: AstNode<Language = PureScript>>(&self, node: &N) -> GroupAstId<N> {
        let target = self.ast_id(node);
        self.group
            .iter()
            .find_map(
                |(raw, inner)| {
                    if inner.contains(&target.raw) {
                        Some(GroupAstId::new(raw))
                    } else {
                        None
                    }
                },
            )
            .unwrap()
    }

    pub fn group_ast_ptrs<N: AstNode<Language = PureScript>>(
        &self,
        id: GroupAstId<N>,
    ) -> impl Iterator<Item = AstPtr<N>> + '_ {
        self.group[id.raw].iter().map(|id| self.inner[*id].clone().cast().unwrap())
    }

    fn alloc(&mut self, node: &SyntaxNode) -> Idx<SyntaxNodePtr> {
        self.inner.alloc(SyntaxNodePtr::new(node))
    }

    fn alloc_group(
        &mut self,
        group: FxHashSet<Idx<SyntaxNodePtr>>,
    ) -> Idx<FxHashSet<Idx<SyntaxNodePtr>>> {
        self.group.alloc(group.into())
    }
}
