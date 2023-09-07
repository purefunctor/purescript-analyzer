//! Names in PureScript.

use std::sync::Arc;

use itertools::Itertools;
use rowan::ast::AstNode;
use syntax::ast;

/// e.g. `Main`, `Data.Maybe`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleName {
    inner: Arc<str>,
}

impl From<ast::ModuleName> for ModuleName {
    fn from(value: ast::ModuleName) -> Self {
        let inner = value
            .children()
            .map(|name| {
                if let Some(name) = name.as_str() {
                    name.into()
                } else {
                    format!("${}", name.syntax().text())
                }
            })
            .join(".")
            .into();

        ModuleName { inner }
    }
}

impl From<ast::QualifiedPrefix> for ModuleName {
    fn from(value: ast::QualifiedPrefix) -> Self {
        let inner = value
            .children()
            .map(|name| {
                if let Some(name) = name.as_str() {
                    name.into()
                } else {
                    format!("${}", name.syntax().text())
                }
            })
            .join(".")
            .into();

        ModuleName { inner }
    }
}

/// e.g. `Just`, `fromMaybe`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NameRef {
    inner: Arc<str>,
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
        let prefix = value.prefix().map(ModuleName::from);
        let value =
            value.name_ref().ok_or("QualifiedName has no NameRef").and_then(NameRef::try_from)?;
        Ok(Qualified { prefix, value })
    }
}
