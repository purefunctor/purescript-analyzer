//! The core of the analyzer as a library.

pub mod id;
pub mod index;
pub mod interner;
pub mod scope;
pub mod source;
pub mod surface;

pub use index::IndexDatabase;
pub use interner::InternerDatabase;
pub use scope::ScopeDatabase;
pub use source::SourceDatabase;
pub use surface::SurfaceDatabase;

#[salsa::database(
    index::IndexStorage,
    interner::InternerStorage,
    scope::ScopeStorage,
    source::SourceStorage,
    surface::SurfaceStorage
)]
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
