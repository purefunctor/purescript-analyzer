//! Names in PureScript.

use smallvec::SmallVec;
use smol_str::SmolStr;
use syntax::ast;

/// e.g. `Main`, `Data.Maybe`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleName {
    segments: SmallVec<[SmolStr; 3]>,
}

impl TryFrom<ast::ModuleName> for ModuleName {
    type Error = &'static str;

    fn try_from(value: ast::ModuleName) -> Result<Self, Self::Error> {
        let segments = value
            .children()
            .map(|name| name.as_str().ok_or("Cannot convert ModuleName segment."))
            .collect::<Result<_, _>>()?;
        Ok(ModuleName { segments })
    }
}

impl TryFrom<ast::QualifiedPrefix> for ModuleName {
    type Error = &'static str;

    fn try_from(value: ast::QualifiedPrefix) -> Result<Self, Self::Error> {
        let segments = value
            .children()
            .map(|name| name.as_str().ok_or("Cannot convert ModuleName segment."))
            .collect::<Result<_, _>>()?;
        Ok(ModuleName { segments })
    }
}

/// e.g. `a = 0`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    name: SmolStr,
}

impl AsRef<str> for Name {
    fn as_ref(&self) -> &str {
        &self.name
    }
}

impl TryFrom<ast::Name> for Name {
    type Error = &'static str;

    fn try_from(value: ast::Name) -> Result<Self, Self::Error> {
        let name = value.as_str().ok_or("Cannot convert Name")?;
        Ok(Name { name })
    }
}

/// e.g. `Just`, `fromMaybe`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NameRef {
    name_ref: SmolStr,
}

impl AsRef<str> for NameRef {
    fn as_ref(&self) -> &str {
        &self.name_ref
    }
}

impl TryFrom<ast::NameRef> for NameRef {
    type Error = &'static str;

    fn try_from(value: ast::NameRef) -> Result<Self, Self::Error> {
        let name_ref = value.as_str().ok_or("Cannot convert NameRef")?;
        Ok(NameRef { name_ref })
    }
}

/// e.g. `M.Just`, `M.fromMaybe`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Qualified<N> {
    pub(crate) prefix: Option<ModuleName>,
    pub(crate) value: N,
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
