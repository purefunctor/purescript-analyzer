use std::{
    hash::{BuildHasher, Hash},
    ops,
};

use hashbrown::HashTable;
use la_arena::{Idx, RawIdx};
use rustc_hash::FxBuildHasher;

#[derive(Debug, Default)]
pub struct Interner<T> {
    inner: Vec<T>,
    table: HashTable<usize>,
}

impl<T: PartialEq> PartialEq for Interner<T> {
    fn eq(&self, other: &Interner<T>) -> bool {
        self.inner == other.inner
    }
}

impl<T: Eq + Hash> Interner<T> {
    pub fn intern(&mut self, value: T) -> Idx<T> {
        let hash = FxBuildHasher.hash_one(&value);
        let existing = self.table.find(hash, |&index| {
            let inner = &self.inner[index];
            &value == inner
        });
        let index = existing.copied().unwrap_or_else(|| {
            let index = self.inner.len();
            self.inner.push(value);
            self.table.insert_unique(hash, index, |&index| {
                let value = &self.inner[index];
                FxBuildHasher.hash_one(value)
            });
            index
        });
        Idx::from_raw(RawIdx::from_u32(index as u32))
    }
}

impl<T> ops::Index<Idx<T>> for Interner<T> {
    type Output = T;

    fn index(&self, index: Idx<T>) -> &Self::Output {
        let index = index.into_raw().into_u32() as usize;
        &self.inner[index]
    }
}

#[cfg(test)]
mod tests {
    use super::Interner;

    #[test]
    fn test_basic() {
        let mut interner = Interner::default();

        let hello_a = interner.intern("hello");
        let hello_b = interner.intern("hello");

        assert_eq!(hello_a, hello_b);
    }

    #[test]
    fn test_eq() {
        let mut interner_a = Interner::default();
        interner_a.intern("hello");
        interner_a.intern("world");

        let mut interner_b = Interner::default();
        interner_b.intern("hello");
        interner_b.intern("world");

        let mut interner_c = Interner::default();
        interner_c.intern("world");
        interner_c.intern("hello");

        assert_eq!(interner_a, interner_b);
        assert_ne!(interner_a, interner_c);
        assert_ne!(interner_b, interner_c);
    }
}
