//! Names in PureScript.

use std::sync::Arc;

use rowan::ast::AstNode;
use smallvec::SmallVec;
use syntax::ast;

use crate::SurfaceDatabase;

/// A trait for constructing interned names from the [`ast`].
pub(crate) trait InDb<Target>: Sized
where
    Self: AstNode,
{
    fn in_db(self, db: &dyn SurfaceDatabase) -> Option<Target>;
}

/// Names separated by a period.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleName {
    segments: SmallVec<[Arc<str>; 3]>,
}

impl ModuleName {
    pub fn iter(&self) -> impl Iterator<Item = &str> {
        self.segments.iter().map(|name| &**name)
    }
}

impl InDb<ModuleName> for ast::ModuleName {
    fn in_db(self, db: &dyn SurfaceDatabase) -> Option<ModuleName> {
        let segments = self
            .children()
            .map(|name| {
                if let Some(token) = name.token() {
                    db.interner().intern(token.text())
                } else {
                    db.interner().intern("?InvalidToken")
                }
            })
            .collect();
        Some(ModuleName { segments })
    }
}

impl InDb<ModuleName> for ast::QualifiedPrefix {
    fn in_db(self, db: &dyn SurfaceDatabase) -> Option<ModuleName> {
        let segments = self
            .children()
            .map(|name| {
                if let Some(token) = name.token() {
                    db.interner().intern(token.text())
                } else {
                    db.interner().intern("?InvalidToken")
                }
            })
            .collect();
        Some(ModuleName { segments })
    }
}

/// Names appearing as bindings.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name(Arc<str>);

impl InDb<Name> for ast::Name {
    fn in_db(self, db: &dyn SurfaceDatabase) -> Option<Name> {
        Some(Name(db.interner().intern(self.token()?.text())))
    }
}

impl AsRef<str> for Name {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// Names appearing as usages.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NameRef(Arc<str>);

impl InDb<NameRef> for ast::NameRef {
    fn in_db(self, db: &dyn SurfaceDatabase) -> Option<NameRef> {
        Some(NameRef(db.interner().intern(self.token()?.text())))
    }
}

impl AsRef<str> for NameRef {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// Values optionally qualified by a [`ModuleName`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Qualified<N> {
    pub(crate) prefix: Option<ModuleName>,
    pub(crate) value: N,
}

impl Qualified<NameRef> {
    pub(crate) fn in_db(
        db: &dyn SurfaceDatabase,
        qualified_name: ast::QualifiedName,
    ) -> Option<Qualified<NameRef>> {
        let prefix = qualified_name.prefix().and_then(|prefix| prefix.in_db(db));
        let value = qualified_name.name_ref()?.in_db(db)?;
        Some(Qualified { prefix, value })
    }
}

impl InDb<Qualified<NameRef>> for ast::QualifiedName {
    fn in_db(self, db: &dyn SurfaceDatabase) -> Option<Qualified<NameRef>> {
        let prefix = self.prefix().and_then(|prefix| prefix.in_db(db));
        let value = self.name_ref()?.in_db(db)?;
        Some(Qualified { prefix, value })
    }
}
