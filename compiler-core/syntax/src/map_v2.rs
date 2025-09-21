use std::{any, fmt, hash::BuildHasher, marker::PhantomData, num::NonZeroU32};

use hashbrown::HashTable;
use rowan::ast::{AstNode, AstPtr};
use rustc_hash::FxBuildHasher;

use crate::{PureScript, SyntaxNode, SyntaxNodePtr};

pub struct AstId<N: AstNode<Language = PureScript>> {
    id: NonZeroU32,
    phantom: PhantomData<fn() -> AstPtr<N>>,
}

impl<N: AstNode<Language = PureScript>> AstId<N> {
    fn new(id: NonZeroU32) -> AstId<N> {
        AstId { id, phantom: PhantomData }
    }
}

impl<N: AstNode<Language = PureScript>> fmt::Debug for AstId<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("AstId<{}>({})", any::type_name::<N>(), self.id))
    }
}

impl<N: AstNode<Language = PureScript>> Clone for AstId<N> {
    fn clone(&self) -> AstId<N> {
        *self
    }
}

impl<N: AstNode<Language = PureScript>> Copy for AstId<N> {}

impl<N: AstNode<Language = PureScript>> PartialEq for AstId<N> {
    fn eq(&self, other: &AstId<N>) -> bool {
        self.id == other.id
    }
}

pub struct AstPtrMap {
    arena: Vec<SyntaxNodePtr>,
    table: HashTable<NonZeroU32>,
}

impl Default for AstPtrMap {
    fn default() -> AstPtrMap {
        let arena = vec![];
        let table = HashTable::default();
        AstPtrMap { arena, table }
    }
}

impl AstPtrMap {
    pub fn allocate(&mut self, node: &SyntaxNode) -> NonZeroU32 {
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

        id
    }

    pub fn allocate_cst<N: AstNode<Language = PureScript>>(&mut self, cst: &N) -> AstId<N> {
        let node = cst.syntax();
        let id = self.allocate(node);
        AstId::new(id)
    }

    pub fn lookup<N: AstNode<Language = PureScript>>(&self, ptr: &AstPtr<N>) -> Option<AstId<N>> {
        let hash = FxBuildHasher.hash_one(ptr);
        let ptr = ptr.syntax_node_ptr();
        self.table
            .find(hash, |&id| arena_index(&self.arena, id) == Some(&ptr))
            .map(|&id| AstId::new(id))
    }

    pub fn index<N: AstNode<Language = PureScript>>(&self, id: AstId<N>) -> Option<AstPtr<N>> {
        arena_index(&self.arena, id.id)?.cast()
    }

    pub fn shrink_to_fit(&mut self) {
        self.arena.shrink_to_fit();
        self.table.shrink_to_fit(|&id| arena_hasher(&self.arena, id));
    }
}

impl PartialEq for AstPtrMap {
    fn eq(&self, other: &Self) -> bool {
        if self.arena.len() != other.arena.len() {
            return false;
        }

        if self.table.len() != other.table.len() {
            return false;
        }

        for &self_id in self.table.iter() {
            let self_ptr = arena_index(&self.arena, self_id).unwrap_or_else(|| {
                unreachable!("invariant violated: self_id is not a valid index")
            });

            let self_hash = FxBuildHasher.hash_one(self_ptr);

            let other_found = other.table.find(self_hash, |&other_id| {
                let other_ptr = arena_index(&self.arena, other_id).unwrap_or_else(|| {
                    unreachable!("invariant violated: other_id is not a valid index")
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

#[inline]
fn arena_index(arena: &[SyntaxNodePtr], id: NonZeroU32) -> Option<&SyntaxNodePtr> {
    let index = id.get() as usize;
    arena.get(index - 1)
}

#[inline]
fn arena_hasher(arena: &[SyntaxNodePtr], id: NonZeroU32) -> u64 {
    let ptr = arena_index(arena, id);
    FxBuildHasher.hash_one(ptr)
}

#[cfg(test)]
mod tests {
    use rowan::{
        GreenNode, GreenToken, NodeOrToken,
        ast::{AstNode, AstPtr},
    };

    use crate::{
        SyntaxKind, SyntaxNode, SyntaxNodePtr,
        cst::{self, Annotation},
    };

    use super::AstPtrMap;

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
        let mut map_1 = AstPtrMap::default();
        map_1.allocate(&zero);

        let zero_ptr: AstPtr<Annotation> = SyntaxNodePtr::new(&zero).cast().unwrap();
        assert!(
            map_1.lookup(&zero_ptr).is_some_and(|id| map_1.index(id).as_ref() == Some(&zero_ptr))
        );

        // In revision 2, we allocate zero and one
        let mut map_2 = AstPtrMap::default();
        map_2.allocate(&zero);
        map_2.allocate(&one);

        let one_ptr: AstPtr<Annotation> = SyntaxNodePtr::new(&one).cast().unwrap();

        // Zero is valid in revision 2
        assert!(
            map_2.lookup(&zero_ptr).is_some_and(|id| map_2.index(id).as_ref() == Some(&zero_ptr))
        );

        // One is valid in revision 2
        assert!(
            map_2.lookup(&one_ptr).is_some_and(|id| map_2.index(id).as_ref() == Some(&one_ptr))
        );

        // One is invalid in revision 1.
        assert!(
            map_1.lookup(&one_ptr).is_some_and(|id| map_1.index(id).as_ref() == Some(&one_ptr))
        );
    }

    #[test]
    fn test_api_cst() {
        let zero = SyntaxNode::new_root(GreenNode::new(
            SyntaxKind::Annotation.into(),
            vec![NodeOrToken::Token(GreenToken::new(SyntaxKind::TEXT.into(), "ZERO"))],
        ));
        let one = SyntaxNode::new_root(GreenNode::new(
            SyntaxKind::Annotation.into(),
            vec![NodeOrToken::Token(GreenToken::new(SyntaxKind::TEXT.into(), "ONE"))],
        ));

        let zero_cst = cst::Annotation::cast(zero).unwrap();
        let zero_ptr = AstPtr::new(&zero_cst);

        let one_cst = cst::Annotation::cast(one).unwrap();
        let one_ptr = AstPtr::new(&one_cst);

        // In revision 1, we only allocate zero.
        let mut map_1 = AstPtrMap::default();

        let zero_id = map_1.allocate_cst(&zero_cst);
        assert_eq!(map_1.lookup(&zero_ptr), Some(zero_id));
        assert_eq!(map_1.index(zero_id).as_ref(), Some(&zero_ptr));

        // In revision 2, we allocate zero and one.
        let mut map_2 = AstPtrMap::default();

        let _ = map_2.allocate_cst(&zero_cst);
        let one_id = map_2.allocate_cst(&one_cst);

        // Zero is still valid in revision 2
        assert_eq!(map_2.lookup(&zero_ptr), Some(zero_id));
        assert_eq!(map_2.index(zero_id).as_ref(), Some(&zero_ptr));

        // One is valid in revision 2
        assert_eq!(map_2.lookup(&one_ptr), Some(one_id));
        assert_eq!(map_2.index(one_id).as_ref(), Some(&one_ptr));

        // One is invalid in revision 1.
        assert_eq!(map_1.lookup(&one_ptr), None);
        assert_eq!(map_1.index(one_id), None);
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

        let zero = cst::Annotation::cast(zero).unwrap();
        let one = cst::Annotation::cast(one).unwrap();
        let two = cst::Annotation::cast(two).unwrap();

        {
            let mut map_a = AstPtrMap::default();
            let mut map_b = AstPtrMap::default();
            let mut map_c = AstPtrMap::default();

            let _ = map_a.allocate_cst(&zero);
            let _ = map_b.allocate_cst(&zero);
            let _ = map_c.allocate_cst(&zero);

            // Symmetric
            assert!(map_a == map_b);
            assert!(map_b == map_a);

            // Transitive
            assert!(map_b == map_c);
            assert!(map_a == map_c);
        }

        {
            let mut map_a = AstPtrMap::default();
            let mut map_b = AstPtrMap::default();

            let _ = map_a.allocate_cst(&zero);
            let _ = map_a.allocate_cst(&two);

            let _ = map_b.allocate_cst(&one);
            let _ = map_b.allocate_cst(&two);

            // Symmetric
            assert!(map_a != map_b);
            assert!(!(map_a == map_b));
        }

        {
            let mut map_a = AstPtrMap::default();
            let mut map_b = AstPtrMap::default();

            let _ = map_a.allocate_cst(&zero);
            let _ = map_b.allocate_cst(&one);
            let _ = map_b.allocate_cst(&two);

            // Length check
            assert!(map_a != map_b);
        }

        {
            let mut map_a = AstPtrMap::default();
            let mut map_b = AstPtrMap::default();

            let _ = map_a.allocate_cst(&zero);
            let _ = map_a.allocate_cst(&one);

            let _ = map_b.allocate_cst(&one);
            let _ = map_b.allocate_cst(&zero);

            // Index check
            assert!(map_a != map_b);
        }
    }
}
