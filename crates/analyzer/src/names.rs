//! Names in PureScript.

use std::sync::Arc;

use syntax::ast;

/// e.g. `Main`, `Data.Maybe`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleName {
    inner: Arc<str>,
}

impl TryFrom<ast::ModuleName> for ModuleName {
    type Error = &'static str;

    fn try_from(value: ast::ModuleName) -> Result<Self, Self::Error> {
        let inner: Result<Vec<_>, _> = value
            .children()
            .map(|name| name.as_str().ok_or("Cannot convert ModuleName segment."))
            .collect();
        let inner = inner?.join(".").into();
        Ok(ModuleName { inner })
    }
}

impl TryFrom<ast::QualifiedPrefix> for ModuleName {
    type Error = &'static str;

    fn try_from(value: ast::QualifiedPrefix) -> Result<Self, Self::Error> {
        let inner: Result<Vec<_>, _> = value
            .children()
            .map(|name| name.as_str().ok_or("Cannot convert ModuleName segment."))
            .collect();
        let inner = inner?.join(".").into();
        Ok(ModuleName { inner })
    }
}

/// e.g. `Just`, `fromMaybe`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NameRef {
    inner: Arc<str>,
}

impl AsRef<str> for NameRef {
    fn as_ref(&self) -> &str {
        &self.inner
    }
}

impl TryFrom<ast::NameRef> for NameRef {
    type Error = &'static str;

    fn try_from(value: ast::NameRef) -> Result<Self, Self::Error> {
        let name_ref = value.as_str().ok_or("Cannot convert NameRef")?.to_string();
        Ok(NameRef { inner: name_ref.into() })
    }
}

/// e.g. `M.Just`, `M.fromMaybe`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Qualified<N> {
    prefix: Option<ModuleName>,
    value: N,
}

impl TryFrom<ast::QualifiedName> for Qualified<NameRef> {
    type Error = &'static str;

    fn try_from(value: ast::QualifiedName) -> Result<Self, Self::Error> {
        let prefix = value.prefix().map(ModuleName::try_from).transpose()?;
        let value =
            value.name_ref().ok_or("QualifiedName has no NameRef").and_then(NameRef::try_from)?;
        Ok(Qualified { prefix, value })
    }
}
