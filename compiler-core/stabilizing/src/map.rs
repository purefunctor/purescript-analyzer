use std::{any, hash::BuildHasher, num::NonZeroU32};

use hashbrown::HashTable;
use rowan::ast::{AstNode, AstPtr};
use rustc_hash::FxBuildHasher;
use syntax::{PureScript, SyntaxNode, SyntaxNodePtr};

use crate::AstId;

pub struct StabilizedModule {
    arena: Vec<SyntaxNodePtr>,
    table: HashTable<NonZeroU32>,
}

impl Default for StabilizedModule {
    fn default() -> StabilizedModule {
        let arena = vec![];
        let table = HashTable::default();
        StabilizedModule { arena, table }
    }
}

impl StabilizedModule {
    pub fn allocate(&mut self, node: &SyntaxNode) {
        let ptr = SyntaxNodePtr::new(node);
        let hash = FxBuildHasher.hash_one(ptr);

        let id = {
            self.arena.push(ptr);
            let index = self.arena.len();
            // SAFETY: Vec::push ensures that the subsequent Vec::len
            // returns a non-zero value to be used as a 1-based index.
            unsafe { NonZeroU32::new_unchecked(index as u32) }
        };

        self.table.insert_unique(hash, id, |&id| arena_hasher(&self.arena, id));
    }

    pub fn lookup_cst<N: AstNode<Language = PureScript>>(&self, cst: &N) -> Option<AstId<N>> {
        let ptr = AstPtr::new(cst);
        self.lookup_ptr(&ptr)
    }

    pub fn lookup_ptr<N: AstNode<Language = PureScript>>(
        &self,
        ptr: &AstPtr<N>,
    ) -> Option<AstId<N>> {
        let ptr = ptr.syntax_node_ptr();
        let hash = FxBuildHasher.hash_one(ptr);
        self.table
            .find(hash, |&id| {
                let inner_ptr = arena_index(&self.arena, id).unwrap_or_else(|| {
                    unreachable!("invariant violated: {id} is not a valid index");
                });
                inner_ptr == ptr
            })
            .map(|&id| AstId::new(id))
    }

    pub fn ast_ptr<N: AstNode<Language = PureScript>>(&self, id: AstId<N>) -> Option<AstPtr<N>> {
        self.syntax_ptr(id)?.cast()
    }

    pub fn syntax_ptr<N: AstNode<Language = PureScript>>(
        &self,
        id: AstId<N>,
    ) -> Option<SyntaxNodePtr> {
        arena_index(&self.arena, id.id)
    }

    pub fn shrink_to_fit(&mut self) {
        self.arena.shrink_to_fit();
        self.table.shrink_to_fit(|&id| arena_hasher(&self.arena, id));
    }
}

impl PartialEq for StabilizedModule {
    fn eq(&self, other: &Self) -> bool {
        if self.arena.len() != other.arena.len() {
            return false;
        }

        if self.table.len() != other.table.len() {
            return false;
        }

        for &self_id in self.table.iter() {
            let self_ptr = arena_index(&self.arena, self_id).unwrap_or_else(|| {
                unreachable!("invariant violated: {self_id} is not a valid index");
            });

            let self_hash = FxBuildHasher.hash_one(self_ptr);

            let other_found = other.table.find(self_hash, |&other_id| {
                let other_ptr = arena_index(&self.arena, other_id).unwrap_or_else(|| {
                    unreachable!("invariant violated: {other_id} is not a valid index");
                });
                self_ptr == other_ptr
            });

            if other_found.is_none() {
                return false;
            }
        }

        true
    }
}

impl Eq for StabilizedModule {}

pub trait ExpectId<N>
where
    N: AstNode<Language = PureScript>,
{
    fn expect_id(&self) -> AstId<N>;
}

impl<N: AstNode<Language = PureScript>> ExpectId<N> for Option<AstId<N>> {
    #[inline]
    fn expect_id(&self) -> AstId<N> {
        self.unwrap_or_else(|| unreachable!("invariant violated: {}", any::type_name::<N>()))
    }
}

#[inline]
fn arena_index(arena: &[SyntaxNodePtr], id: NonZeroU32) -> Option<SyntaxNodePtr> {
    let index = id.get() as usize;
    arena.get(index - 1).copied()
}

#[inline]
fn arena_hasher(arena: &[SyntaxNodePtr], id: NonZeroU32) -> u64 {
    let ptr = arena_index(arena, id).unwrap_or_else(|| {
        unreachable!("invariant violated: {id} is not a valid index");
    });
    FxBuildHasher.hash_one(ptr)
}

#[cfg(test)]
mod tests {
    use rowan::{GreenNode, GreenToken, NodeOrToken, ast::AstPtr};
    use syntax::{SyntaxKind, SyntaxNode, SyntaxNodePtr, cst};

    use super::StabilizedModule;

    #[test]
    fn test_api() {
        let zero = SyntaxNode::new_root(GreenNode::new(
            SyntaxKind::Annotation.into(),
            vec![NodeOrToken::Token(GreenToken::new(SyntaxKind::TEXT.into(), "ZERO"))],
        ));

        let one = SyntaxNode::new_root(GreenNode::new(
            SyntaxKind::Annotation.into(),
            vec![NodeOrToken::Token(GreenToken::new(SyntaxKind::TEXT.into(), "ONE"))],
        ));

        // In revision 1, we only allocate zero
        let mut map_1 = StabilizedModule::default();
        map_1.allocate(&zero);

        let zero_ptr: AstPtr<cst::Annotation> = SyntaxNodePtr::new(&zero).cast().unwrap();
        assert!(
            map_1
                .lookup_ptr(&zero_ptr)
                .is_some_and(|id| map_1.ast_ptr(id).as_ref() == Some(&zero_ptr))
        );

        // In revision 2, we allocate zero and one
        let mut map_2 = StabilizedModule::default();
        map_2.allocate(&zero);
        map_2.allocate(&one);

        let one_ptr: AstPtr<cst::Annotation> = SyntaxNodePtr::new(&one).cast().unwrap();

        // Zero is valid in revision 2
        assert!(
            map_2
                .lookup_ptr(&zero_ptr)
                .is_some_and(|id| map_2.ast_ptr(id).as_ref() == Some(&zero_ptr))
        );

        // One is valid in revision 2
        assert!(
            map_2
                .lookup_ptr(&one_ptr)
                .is_some_and(|id| map_2.ast_ptr(id).as_ref() == Some(&one_ptr))
        );

        // One is invalid in revision 1.
        assert!(map_1.lookup_ptr(&one_ptr).is_none());
    }

    #[test]
    fn test_equality() {
        let zero = SyntaxNode::new_root(GreenNode::new(
            SyntaxKind::Annotation.into(),
            vec![NodeOrToken::Token(GreenToken::new(SyntaxKind::TEXT.into(), "ZERO"))],
        ));
        let one = SyntaxNode::new_root(GreenNode::new(
            SyntaxKind::Annotation.into(),
            vec![NodeOrToken::Token(GreenToken::new(SyntaxKind::TEXT.into(), "ONE"))],
        ));
        let two = SyntaxNode::new_root(GreenNode::new(
            SyntaxKind::Annotation.into(),
            vec![NodeOrToken::Token(GreenToken::new(SyntaxKind::TEXT.into(), "TWO TWO"))],
        ));

        {
            let mut map_a = StabilizedModule::default();
            let mut map_b = StabilizedModule::default();
            let mut map_c = StabilizedModule::default();

            let _ = map_a.allocate(&zero);
            let _ = map_b.allocate(&zero);
            let _ = map_c.allocate(&zero);

            // Symmetric
            assert!(map_a == map_b);
            assert!(map_b == map_a);

            // Transitive
            assert!(map_b == map_c);
            assert!(map_a == map_c);
        }

        {
            let mut map_a = StabilizedModule::default();
            let mut map_b = StabilizedModule::default();

            let _ = map_a.allocate(&zero);
            let _ = map_a.allocate(&two);

            let _ = map_b.allocate(&one);
            let _ = map_b.allocate(&two);

            // Symmetric
            assert!(map_a != map_b);
            assert!(!(map_a == map_b));
        }

        {
            let mut map_a = StabilizedModule::default();
            let mut map_b = StabilizedModule::default();

            let _ = map_a.allocate(&zero);
            let _ = map_b.allocate(&one);
            let _ = map_b.allocate(&two);

            // Length check
            assert!(map_a != map_b);
        }

        {
            let mut map_a = StabilizedModule::default();
            let mut map_b = StabilizedModule::default();

            let _ = map_a.allocate(&zero);
            let _ = map_a.allocate(&one);

            let _ = map_b.allocate(&one);
            let _ = map_b.allocate(&zero);

            // Index check
            assert!(map_a != map_b);
        }
    }
}
