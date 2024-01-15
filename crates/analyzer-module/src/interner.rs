//! Queries pertaining to interning and similar utilities.

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

impl InDb for ast::ModuleName {
    fn in_db<Db>(self, db: &Db) -> Option<Arc<str>>
    where
        Db: InternerDatabase + ?Sized,
    {
        let mut buffer = String::default();
        let mut children = self.children().peekable();
        while let Some(name_ref) = children.next() {
            if let Some(token) = name_ref.token() {
                buffer.push_str(token.text())
            } else {
                buffer.push_str("?InvalidToken")
            }
            if children.peek().is_some() {
                buffer.push('.');
            }
        }
        Some(db.interner().intern(buffer))
    }
}

impl InDb for ast::QualifiedPrefix {
    fn in_db<Db>(self, db: &Db) -> Option<Arc<str>>
    where
        Db: InternerDatabase + ?Sized,
    {
        let mut buffer = String::default();
        let mut children = self.children().peekable();
        while let Some(name_ref) = children.next() {
            if let Some(token) = name_ref.token() {
                buffer.push_str(token.text())
            } else {
                buffer.push_str("?InvalidToken")
            }
            if children.peek().is_some() {
                buffer.push('.');
            }
        }
        Some(db.interner().intern(buffer))
    }
}
