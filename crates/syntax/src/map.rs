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
            FxBuildHasher.hash_one(&ptr)
        });
        id
    }

    pub fn lookup(&self, ptr: &N) -> Option<Idx<AstPtr<N>>> {
        let ptr = AstPtr::new(ptr);
        let hash = FxBuildHasher.hash_one(&ptr);
        self.table.find(hash, |&id| self.arena[id] == ptr).copied()
    }

    pub fn shrink_to_fit(&mut self) {
        self.arena.shrink_to_fit();
        self.table.shrink_to_fit(|&id| {
            let ptr = &self.arena[id];
            FxBuildHasher.hash_one(&ptr)
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
        if self.table.len() != other.table.len() {
            return false;
        }
        self.arena.values().all(|ptr| {
            let hash = FxBuildHasher.hash_one(ptr);
            self.table.find(hash, |&id| &self.arena[id] == ptr).is_some()
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
                    pub fn [<lookup_ $field>](&self, ptr: &$cst) -> Option<[<$name Id>]> {
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
    (pub struct $name:ident { $($field:ident: $id:ty => $v:ty),* $(,)? }) => {
        paste::paste! {
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
