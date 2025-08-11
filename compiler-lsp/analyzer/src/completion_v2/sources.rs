use async_lsp::lsp_types::*;
use indexing::ImportKind;

use crate::completion::resolve::CompletionResolveData;

use super::prelude::*;

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
    fn candidates<F: Filter>(
        &self,
        context: &Context,
        filter: F,
    ) -> impl Iterator<Item = CompletionItem> {
        let source =
            context.resolved.qualified.iter().filter(move |(name, _)| filter.matches(name));
        source.filter_map(|(name, import)| {
            let (parsed, _) = context.engine.parsed(import.file).ok()?;
            let description = parsed.module_name().map(|name| name.to_string());
            Some(completion_item(
                name,
                name,
                CompletionItemKind::MODULE,
                description,
                context.range,
                CompletionResolveData::Import(import.file),
            ))
        })
    }
}

/// Yields terms defined in the current module.
pub struct LocalTerms;

impl Source for LocalTerms {
    fn candidates<F: Filter>(
        &self,
        context: &Context,
        filter: F,
    ) -> impl Iterator<Item = CompletionItem> {
        let source =
            context.resolved.locals.iter_terms().filter(move |(name, _, _)| filter.matches(name));
        source.map(|(name, f, t)| {
            let description = Some("Local".to_string());
            completion_item(
                name,
                name,
                CompletionItemKind::VALUE,
                description,
                context.range,
                CompletionResolveData::TermItem(f, t),
            )
        })
    }
}

/// Yields types defined in the current module.
pub struct LocalTypes;

impl Source for LocalTypes {
    fn candidates<F: Filter>(
        &self,
        context: &Context,
        filter: F,
    ) -> impl Iterator<Item = CompletionItem> {
        let source =
            context.resolved.locals.iter_types().filter(move |(name, _, _)| filter.matches(name));
        source.map(|(name, f, t)| {
            let description = Some("Local".to_string());
            completion_item(
                name,
                name,
                CompletionItemKind::STRUCT,
                description,
                context.range,
                CompletionResolveData::TypeItem(f, t),
            )
        })
    }
}

/// Yields terms from unqualified imports.
pub struct ImportedTerms;

impl Source for ImportedTerms {
    fn candidates<F: Filter>(
        &self,
        context: &Context,
        filter: F,
    ) -> impl Iterator<Item = CompletionItem> {
        let source = context.resolved.unqualified.values().flatten();
        source.flat_map(move |import| {
            let source = import.iter_terms().filter(move |(name, _, _, kind)| {
                filter.matches(name) && !matches!(kind, ImportKind::Hidden)
            });
            source.filter_map(|(name, f, t, _)| {
                let (parsed, _) = context.engine.parsed(f).ok()?;
                let description = parsed.module_name().map(|name| name.to_string());
                Some(completion_item(
                    name,
                    name,
                    CompletionItemKind::VALUE,
                    description,
                    context.range,
                    CompletionResolveData::TermItem(f, t),
                ))
            })
        })
    }
}

/// Yields types from unqualified imports.
pub struct ImportedTypes;

impl Source for ImportedTypes {
    fn candidates<F: Filter>(
        &self,
        context: &Context,
        filter: F,
    ) -> impl Iterator<Item = CompletionItem> {
        let source = context.resolved.unqualified.values().flatten();
        source.flat_map(move |import| {
            let source = import.iter_types().filter(move |(name, _, _, kind)| {
                filter.matches(name) && !matches!(kind, ImportKind::Hidden)
            });
            source.filter_map(|(name, f, t, _)| {
                let (parsed, _) = context.engine.parsed(f).ok()?;
                let description = parsed.module_name().map(|name| name.to_string());
                Some(completion_item(
                    name,
                    name,
                    CompletionItemKind::STRUCT,
                    description,
                    context.range,
                    CompletionResolveData::TypeItem(f, t),
                ))
            })
        })
    }
}

/// Yields terms from qualified imports.
pub struct QualifiedTerms<'a>(pub &'a str);

impl Source for QualifiedTerms<'_> {
    fn candidates<F: Filter>(
        &self,
        context: &Context,
        filter: F,
    ) -> impl Iterator<Item = CompletionItem> {
        let source = context.resolved.qualified.get(self.0);
        source.into_iter().flat_map(move |import| {
            let source = import.iter_terms().filter(move |(name, _, _, kind)| {
                filter.matches(name) && !matches!(kind, ImportKind::Hidden)
            });
            source.filter_map(|(name, f, t, _)| {
                let (parsed, _) = context.engine.parsed(f).ok()?;
                let edit = format!("{}.{}", self.0, name);
                let description = parsed.module_name().map(|name| name.to_string());
                Some(completion_item(
                    name,
                    edit,
                    CompletionItemKind::VALUE,
                    description,
                    context.range,
                    CompletionResolveData::TermItem(f, t),
                ))
            })
        })
    }
}

/// Yields types from qualified imports.
pub struct QualifiedTypes<'a>(pub &'a str);

impl Source for QualifiedTypes<'_> {
    fn candidates<F: Filter>(
        &self,
        context: &Context,
        filter: F,
    ) -> impl Iterator<Item = CompletionItem> {
        let source = context.resolved.qualified.get(self.0);
        source.into_iter().flat_map(move |import| {
            let source = import.iter_types().filter(move |(name, _, _, kind)| {
                filter.matches(name) && !matches!(kind, ImportKind::Hidden)
            });
            source.filter_map(|(name, f, t, _)| {
                let (parsed, _) = context.engine.parsed(f).ok()?;
                let edit = format!("{}.{}", self.0, name);
                let description = parsed.module_name().map(|name| name.to_string());
                Some(completion_item(
                    name,
                    edit,
                    CompletionItemKind::STRUCT,
                    description,
                    context.range,
                    CompletionResolveData::TypeItem(f, t),
                ))
            })
        })
    }
}
