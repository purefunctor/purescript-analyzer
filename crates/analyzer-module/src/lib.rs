//! The core of the analyzer as a library.

pub mod source;

pub use source::SourceDatabase;

#[salsa::database(source::SourceStorage)]
pub struct RootDatabase {
    storage: salsa::Storage<RootDatabase>,
}

impl salsa::Database for RootDatabase {}

impl Default for RootDatabase {
    fn default() -> RootDatabase {
        let db = RootDatabase { storage: Default::default() };
        db
    }
}

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}
