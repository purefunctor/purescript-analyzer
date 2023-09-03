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
