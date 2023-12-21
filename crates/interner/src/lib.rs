//! [`Arc<str>`]-based string interning API.
//!
//! [`Arc`]: std::sync::Arc

use std::{
    hash::{BuildHasher, BuildHasherDefault},
    sync::{Arc, Mutex, Weak},
};

use intmap::IntMap;
use rustc_hash::FxHasher;

/// An internally-mutable string interner.
///
/// This struct is meant to be used in conjunction with [`Arc`] or the
/// [`lazy_static`] crate.
///
/// Internally, this maps the hash of a string to a [`Weak`] reference of an
/// [`Arc`]-allocated string. If an entry does not exist for a given string,
/// an allocation is made. If an entry does exist for a given string, it tries
/// to upgrade the associated [`Weak`] pointer; otherwise, it allocates a new
/// [`Arc`] and replaces the stale pointer.
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
/// [`lazy_static`]: https://crates.io/crates/lazy_static
#[derive(Debug, Default)]
pub struct Interner {
    inner: Mutex<IntMap<Weak<str>>>,
}

impl Interner {
    fn hash_one(value: &str) -> u64 {
        BuildHasherDefault::<FxHasher>::default().hash_one(value)
    }

    /// Interns a string.
    ///
    /// If an entry does not exist for the given string, this allocates a fresh
    /// [`Arc`]. If an entry does exist, this tries to upgrade the associated
    /// [`Weak`] pointer; otherwise, a fresh [`Arc`] is allocated and the stale
    /// [`Weak`] pointer is replaced.
    pub fn intern(&self, value: impl AsRef<str>) -> Arc<str> {
        let mut inner = self.inner.lock().unwrap();

        let value = value.as_ref();
        let key = Interner::hash_one(value);

        match inner.entry(key) {
            intmap::Entry::Occupied(mut o) => {
                // Replace stale pointers with fresh ones.
                o.get().upgrade().unwrap_or_else(|| {
                    let strong = Arc::from(value);
                    o.insert(Arc::downgrade(&strong));
                    strong
                })
            }
            intmap::Entry::Vacant(v) => {
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
        inner.retain(|_, value| value.strong_count() > 0);
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
