//! The core of the analyzer.

pub mod id;
pub mod infer;
pub mod names;
pub mod resolver;
pub mod scope;
pub mod source;
pub mod sugar;
pub mod surface;

pub use infer::InferDatabase;
pub use resolver::ResolverDatabase;
pub use scope::ScopeDatabase;
pub use source::SourceDatabase;
pub use sugar::SugarDatabase;
pub use surface::SurfaceDatabase;

/// The analyzer's core database.
#[derive(Default)]
#[salsa::database(
    infer::InferStorage,
    resolver::ResolverStorage,
    scope::ScopeStorage,
    source::SourceStorage,
    sugar::SugarStorage,
    surface::SurfaceStorage
)]
pub struct RootDatabase {
    storage: salsa::Storage<RootDatabase>,
}

impl salsa::Database for RootDatabase {}

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

impl Upcast<dyn ResolverDatabase> for RootDatabase {
    fn upcast(&self) -> &(dyn ResolverDatabase + 'static) {
        self
    }
}

impl Upcast<dyn InferDatabase> for RootDatabase {
    fn upcast(&self) -> &(dyn InferDatabase + 'static) {
        self
    }
}
