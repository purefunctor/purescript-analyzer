//! [`Arc<str>`]-based string interning API.
//!
//! [`Arc`]: std::sync::Arc

use std::{
    hash::{BuildHasher, BuildHasherDefault},
    sync::{Arc, Mutex, Weak},
};

use hashbrown::{hash_table::Entry, HashTable};
use rustc_hash::FxHasher;

/// An internally-mutable string interner.
///
/// This struct is meant to be used in conjunction with [`Arc`] or the
/// [`lazy_static`] crate.
///
/// Internally, this is implemented as a specialized [`HashTable`] which
/// associates the hash of the string with a [`Weak`] pointer that can be
/// upgraded if the [`Arc`] allocation for said string still exists. The
/// implementation also uses the hash as the "key" for the table, which
/// makes it similar to a [`BTreeMap`]-based representation.
///
/// ```rust
/// # use std::sync::Arc;
/// # use interner::Interner;
/// let interner = Arc::from(Interner::default());
///
/// let name_0 = interner.intern("hello");
/// let name_1 = interner.intern("hello");
///
/// assert!(Arc::ptr_eq(&name_0, &name_1));
/// ```
///
/// [`BTreeMap`]: std::collections::BTreeMap
/// [`lazy_static`]: https://crates.io/crates/lazy_static
#[derive(Debug, Default)]
pub struct Interner {
    inner: Mutex<HashTable<(u64, Weak<str>)>>,
    hasher: BuildHasherDefault<FxHasher>,
}

impl Interner {
    /// Interns a string.
    ///
    /// If an entry does not exist for the given string, this allocates a fresh
    /// [`Arc`]. If an entry does exist, this tries to upgrade the associated
    /// [`Weak`] pointer; otherwise, a fresh [`Arc`] is allocated and the stale
    /// [`Weak`] pointer is replaced.
    pub fn intern(&self, value: impl AsRef<str>) -> Arc<str> {
        let mut inner = self.inner.lock().unwrap();

        let value = value.as_ref();
        let hash = self.hasher.hash_one(value);

        match inner.entry(hash, |(inner, _)| hash == *inner, |(inner, _)| *inner) {
            Entry::Occupied(mut o) => {
                let (_, weak) = o.get();
                weak.upgrade().unwrap_or_else(|| {
                    let strong = Arc::from(value);
                    let (_, ref mut weak) = o.get_mut();
                    *weak = Arc::downgrade(&strong);
                    strong
                })
            }
            Entry::Vacant(v) => {
                let strong = Arc::from(value);
                v.insert((hash, Arc::downgrade(&strong)));
                strong
            }
        }
    }

    /// Prunes stale entries.
    ///
    /// An entry is considered stale if its associated [`Weak`] pointer cannot
    /// be upgraded into an [`Arc`] anymore; as in if [`Weak::strong_count`]
    /// drops to zero.
    pub fn prune(&self) {
        let mut inner = self.inner.lock().unwrap();
        inner.retain(|(_, weak)| weak.strong_count() > 0)
    }
}

#[cfg(test)]
mod tests {
    use std::{ptr, sync::Arc};

    use crate::Interner;

    #[test]
    fn api_test() {
        let interner = Arc::new(Interner::default());

        {
            let name_0 = interner.intern("name");
            let name_1 = interner.intern("name");
            assert!(ptr::eq(name_0.as_ptr(), name_1.as_ptr()));
        }

        let name_2 = interner.intern("name");
        assert!(ptr::eq(name_2.as_ptr(), name_2.as_ptr()));
    }
}
