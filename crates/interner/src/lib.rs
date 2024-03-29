//! [`Arc<str>`]-based string interning API.
//!
//! [`Arc`]: std::sync::Arc

use std::{
    collections::hash_map::Entry,
    hash::{BuildHasher, BuildHasherDefault},
    sync::{Arc, Mutex, Weak},
};

use rustc_hash::{FxHashMap, FxHasher};

/// An internally-mutable string interner.
///
/// This struct is meant to be used in conjunction with [`Arc`] or the
/// [`lazy_static`] crate.
///
/// Internally, this is implemented as a [`HashMap`] which associates
/// the hash of the string with a [`Weak`] pointer that can be upgraded
/// if the [`Arc`] allocation for said string still exists.
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
/// [`HashMap`]: std::collections::HashMap
/// [`lazy_static`]: https://crates.io/crates/lazy_static
#[derive(Debug, Default)]
pub struct Interner {
    inner: Mutex<FxHashMap<u64, Weak<str>>>,
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

        match inner.entry(hash) {
            Entry::Occupied(mut o) => o.get().upgrade().unwrap_or_else(|| {
                let strong = Arc::from(value);
                o.insert(Arc::downgrade(&strong));
                strong
            }),
            Entry::Vacant(v) => {
                let strong = Arc::from(value);
                v.insert(Arc::downgrade(&strong));
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
        inner.retain(|_, weak| weak.strong_count() > 0)
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
