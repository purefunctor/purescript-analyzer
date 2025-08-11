use async_lsp::lsp_types::*;
use building::QueryEngine;

use super::{filter::HasPrefix, prelude::*};

/// Yields the qualified names of imports.
///
/// For example:
/// ```purescript
/// import Halogen as H
/// import Halogen.HTML as HH
///
/// -- candidates -> [H, HH]
/// ```
pub struct QualifiedModules;

impl Source for QualifiedModules {
    type Filter = HasPrefix;

    fn candidates(
        engine: &QueryEngine,
        context: &Context,
        filter: Self::Filter,
    ) -> impl Iterator<Item = CompletionItem> {
        let source =
            context.resolved.qualified.iter().filter(move |(name, _)| filter.matches(name));
        source.filter_map(|(name, import)| {
            let (parsed, _) = engine.parsed(import.file).ok()?;
            let description = parsed.module_name().map(|name| name.to_string());
            Some(completion_item(name, name, CompletionItemKind::MODULE, description, None))
        })
    }
}
