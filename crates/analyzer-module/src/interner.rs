use std::sync::Arc;

use interner::Interner;
use syntax::ast;

#[salsa::query_group(InternerStorage)]
pub trait InternerDatabase {
    #[salsa::input]
    fn interner(&self) -> Arc<Interner>;
}

pub trait InDb: Sized {
    fn in_db<Db>(self, db: &Db) -> Option<Arc<str>>
    where
        Db: InternerDatabase + ?Sized;
}

impl InDb for ast::Name {
    fn in_db<Db>(self, db: &Db) -> Option<Arc<str>>
    where
        Db: InternerDatabase + ?Sized,
    {
        self.as_str().map(|name| db.interner().intern(name))
    }
}

impl InDb for ast::NameRef {
    fn in_db<Db>(self, db: &Db) -> Option<Arc<str>>
    where
        Db: InternerDatabase + ?Sized,
    {
        self.as_str().map(|name| db.interner().intern(name))
    }
}
