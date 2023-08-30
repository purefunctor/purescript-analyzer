//! Names in PureScript.

use std::sync::Arc;

use rowan::ast::AstNode;
use syntax::ast;

/// e.g. `Main`, `Data.Maybe`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleName {
    inner: Arc<str>,
}

impl From<ast::ModuleName> for ModuleName {
    fn from(value: ast::ModuleName) -> Self {
        let capacity = value.syntax().text().len().into();
        let mut accumulator = String::with_capacity(capacity);

        for name in value.children() {
            if let Some(name) = name.as_str() {
                accumulator.push_str(&name);
            } else {
                let name = format!("${}", name.syntax().text());
                accumulator.push_str(&name);
            }
        }

        let inner = accumulator.into();
        ModuleName { inner }
    }
}
