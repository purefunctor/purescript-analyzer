//! The core of the analyzer as a library.

pub mod id;
pub mod index;
pub mod interner;
pub mod source;

pub use index::IndexDatabase;
pub use interner::InternerDatabase;
pub use source::SourceDatabase;

#[salsa::database(index::IndexStorage, interner::InternerStorage, source::SourceStorage)]
pub struct RootDatabase {
    storage: salsa::Storage<RootDatabase>,
}

impl salsa::Database for RootDatabase {}

impl Default for RootDatabase {
    fn default() -> RootDatabase {
        let mut db = RootDatabase { storage: Default::default() };
        db.set_interner(Default::default());
        db
    }
}

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}
