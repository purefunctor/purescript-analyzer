use std::{cmp, fmt::Debug, marker::PhantomData};

/// An index associated with a type.
pub struct Id<T> {
    pub(crate) index: usize,
    _marker: PhantomData<fn() -> T>,
}

impl<T> Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Id").field(&self.index).finish()
    }
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Id<T> {
        *self
    }
}

impl<T> Copy for Id<T> {}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Id<T>) -> bool {
        self.index == other.index
    }
}

impl<T> Eq for Id<T> {}

impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

impl<T> Id<T> {
    pub fn from_raw(index: usize) -> Id<T> {
        Id { index, _marker: PhantomData }
    }
}

#[test]
fn from_raw() {
    let id: Id<()> = Id::from_raw(42);
    assert_eq!(format!("{:?}", id), "Id(42)");
    assert_eq!(id.clone(), id);
}
