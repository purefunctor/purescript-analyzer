use std::{hash::BuildHasher, ops::Index};

use hashbrown::HashTable;
use la_arena::{Arena, Idx};
use rowan::ast::{AstNode, AstPtr};
use rustc_hash::FxBuildHasher;

/// Assigns stable IDs to source ranges.
///
/// This structure stores [`AstPtr`] in an [`Arena`], returning [`Idx`] that
/// is less sensitive to non-semantic changes then naive source ranges.
///
/// [`create_source`] is the intended way to construct groups of these maps
/// where they make sense. For instance, indexing uses it to assign stable
/// IDs to top-level declarations, while lowering uses it to assign stable
/// IDs to recursive structures like expressions, binders, and types.
///
/// [`create_association`] can then be used to associate these [`Idx`] to
/// the semantic information that the compiler collects. For instance in
/// lowering, this is used to identify the kind of a syntax node, as well
/// as to identify which scope graph node it resolved local names from.
///
/// [`create_source`]: crate::create_source
/// [`create_association`]: crate::create_association
#[derive(Debug)]
pub struct AstPtrMap<N: AstNode> {
    arena: Arena<AstPtr<N>>,
    table: HashTable<Idx<AstPtr<N>>>,
}

impl<N: AstNode> Default for AstPtrMap<N> {
    fn default() -> AstPtrMap<N> {
        AstPtrMap { arena: Arena::default(), table: HashTable::default() }
    }
}

impl<N: AstNode> AstPtrMap<N> {
    pub fn allocate(&mut self, ptr: &N) -> Idx<AstPtr<N>> {
        let ptr = AstPtr::new(ptr);
        let hash = FxBuildHasher.hash_one(&ptr);
        let id = self.arena.alloc(ptr);
        self.table.insert_unique(hash, id, |&id| {
            let ptr = &self.arena[id];
            FxBuildHasher.hash_one(ptr)
        });
        id
    }

    pub fn lookup(&self, ptr: &AstPtr<N>) -> Option<Idx<AstPtr<N>>> {
        let hash = FxBuildHasher.hash_one(ptr);
        self.table.find(hash, |&id| &self.arena[id] == ptr).copied()
    }

    pub fn shrink_to_fit(&mut self) {
        self.arena.shrink_to_fit();
        self.table.shrink_to_fit(|&id| {
            let ptr = &self.arena[id];
            FxBuildHasher.hash_one(ptr)
        });
    }
}

impl<N: AstNode> Index<Idx<AstPtr<N>>> for AstPtrMap<N> {
    type Output = AstPtr<N>;

    fn index(&self, id: Idx<AstPtr<N>>) -> &AstPtr<N> {
        &self.arena[id]
    }
}

impl<N: AstNode> PartialEq for AstPtrMap<N> {
    fn eq(&self, other: &Self) -> bool {
        if self.arena.len() != other.arena.len() {
            return false;
        }
        self.arena.iter().all(|(id, ptr)| {
            let other_id = other.lookup(ptr);
            other_id.is_some_and(|other_id| id == other_id)
        })
    }
}

impl<N: AstNode> Eq for AstPtrMap<N> {}

/// Creates a collection of [`AstPtrMap`].
///
/// # Example
///
/// ```rust
/// syntax::create_source! {
///     pub struct Source {
///         expression: syntax::cst::Expression as Expression
///     }
/// }
/// ```
///
/// See documentation of [`AstPtrMap`] for more information.
#[macro_export]
macro_rules! create_source {
    (pub struct $t:ident { $($field:ident: $cst:ty as $name:ident),* $(,)? }) => {
        paste::paste! {
            $(
                pub type [<$name Ptr>] = rowan::ast::AstPtr<$cst>;
                pub type [<$name Id>] = la_arena::Idx<[<$name Ptr>]>;
            )*

            #[derive(Debug, Default, PartialEq, Eq)]
            pub struct $t {
                $(
                    $field: $crate::AstPtrMap<$cst>,
                )*
            }

            impl $t {
                $(
                    pub fn [<allocate_ $field>](&mut self, ptr: &$cst) -> [<$name Id>] {
                        self.$field.allocate(ptr)
                    }
                )*


                $(
                    pub fn [<lookup_ $field>](&self, ptr: &[<$name Ptr>]) -> Option<[<$name Id>]> {
                        self.$field.lookup(ptr)
                    }
                )*

                pub fn shrink_to_fit(&mut self) {
                    $(
                        self.$field.shrink_to_fit();
                    )*
                }
            }

            $(
                impl std::ops::Index<[<$name Id>]> for $t {
                    type Output = [<$name Ptr>];

                    fn index(&self, id: [<$name Id>]) -> &[<$name Ptr>] {
                        &self.$field[id]
                    }
                }
            )*
        }
    };
}

/// Creates a collection of [`ArenaMap`].
///
/// # Example
///
/// ```rust
/// syntax::create_source! {
///     pub struct Source {
///         expression: syntax::cst::Expression as Expression
///     }
/// }
///
/// syntax::create_association! {
///     pub struct Information {
///         expression: ExpressionId => bool,
///     }
/// }
/// ```
///
/// [`ArenaMap`]: la_arena::ArenaMap
#[macro_export]
macro_rules! create_association {
    ($(#[$outer:meta])* pub struct $name:ident { $($field:ident: $id:ty => $v:ty),* $(,)? }) => {
        paste::paste! {
            $(#[$outer])*
            #[derive(Debug, Default, PartialEq, Eq)]
            pub struct $name {
                $(
                    $field: la_arena::ArenaMap<$id, $v>,
                )*
            }

            impl $name {
                $(
                    pub fn [<insert_ $field>](&mut self, k: $id, v: $v) {
                        self.$field.insert(k, v);
                    }

                    pub fn [<index_ $field>](&self, k: $id) -> Option<&$v> {
                        self.$field.get(k)
                    }
                )*
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use rowan::{ast::AstNode, GreenNode, GreenToken, NodeOrToken};

    use crate::{cst, SyntaxKind, SyntaxNode};

    use super::AstPtrMap;

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
            let mut map_a: AstPtrMap<cst::Annotation> = AstPtrMap::default();
            let mut map_b: AstPtrMap<cst::Annotation> = AstPtrMap::default();
            let mut map_c: AstPtrMap<cst::Annotation> = AstPtrMap::default();

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
            let mut map_a: AstPtrMap<cst::Annotation> = AstPtrMap::default();
            let mut map_b: AstPtrMap<cst::Annotation> = AstPtrMap::default();

            let _ = map_a.allocate(&zero);
            let _ = map_a.allocate(&two);

            let _ = map_b.allocate(&one);
            let _ = map_b.allocate(&two);

            // Symmetric
            assert!(map_a != map_b);
            assert!(!(map_a == map_b));
        }

        {
            let mut map_a: AstPtrMap<cst::Annotation> = AstPtrMap::default();
            let mut map_b: AstPtrMap<cst::Annotation> = AstPtrMap::default();

            let _ = map_a.allocate(&zero);
            let _ = map_b.allocate(&one);
            let _ = map_b.allocate(&two);

            // Length check
            assert!(map_a != map_b);
        }

        {
            let mut map_a: AstPtrMap<cst::Annotation> = AstPtrMap::default();
            let mut map_b: AstPtrMap<cst::Annotation> = AstPtrMap::default();

            let _ = map_a.allocate(&zero);
            let _ = map_a.allocate(&one);

            let _ = map_b.allocate(&one);
            let _ = map_b.allocate(&zero);

            // Index check
            assert!(map_a != map_b);
        }
    }
}
