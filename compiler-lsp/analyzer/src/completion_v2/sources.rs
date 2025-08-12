use async_lsp::lsp_types::*;
use files::FileId;
use indexing::{ImportKind, TermItemId, TypeItemId};
use resolving::FullResolvedModule;
use smol_str::SmolStr;

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

/// Yields suggestions for terms.
pub struct SuggestedTerms;

/// Yields suggestions for types.
pub struct SuggestedTypes;

trait SuggestionsHelper {
    type ItemId;

    fn exports(
        resolved: &FullResolvedModule,
    ) -> impl Iterator<Item = (&SmolStr, FileId, Self::ItemId)>;

    fn candidate(
        context: &Context,
        name: &SmolStr,
        file_id: FileId,
        item_id: Self::ItemId,
    ) -> CompletionItem;
}

impl SuggestionsHelper for SuggestedTerms {
    type ItemId = TermItemId;

    fn exports(
        resolved: &FullResolvedModule,
    ) -> impl Iterator<Item = (&SmolStr, FileId, TermItemId)> {
        resolved.exports.iter_terms()
    }

    fn candidate(
        context: &Context,
        name: &SmolStr,
        file_id: FileId,
        item_id: Self::ItemId,
    ) -> CompletionItem {
        let description = context.engine.parsed(file_id).ok().and_then(|(parsed, _)| {
            let module_name = parsed.module_name()?;
            Some(module_name.to_string())
        });
        completion_item(
            name,
            name,
            CompletionItemKind::VALUE,
            description,
            context.range,
            CompletionResolveData::TermItem(file_id, item_id),
        )
    }
}

impl SuggestionsHelper for SuggestedTypes {
    type ItemId = TypeItemId;

    fn exports(
        resolved: &FullResolvedModule,
    ) -> impl Iterator<Item = (&SmolStr, FileId, Self::ItemId)> {
        resolved.exports.iter_types()
    }

    fn candidate(
        context: &Context,
        name: &SmolStr,
        file_id: FileId,
        item_id: Self::ItemId,
    ) -> CompletionItem {
        let description = context.engine.parsed(file_id).ok().and_then(|(parsed, _)| {
            let module_name = parsed.module_name()?;
            Some(module_name.to_string())
        });
        completion_item(
            name,
            name,
            CompletionItemKind::STRUCT,
            description,
            context.range,
            CompletionResolveData::TypeItem(file_id, item_id),
        )
    }
}

impl<T> Source for T
where
    T: SuggestionsHelper,
{
    fn candidates<F: Filter>(
        &self,
        context: &Context,
        filter: F,
    ) -> impl Iterator<Item = CompletionItem> {
        let prim_id = context.engine.prim_id();
        let has_prim =
            context.resolved.unqualified.values().flatten().any(|import| import.file == prim_id);

        let file_ids = context.files.iter_id().filter(move |&id| {
            let not_self = id != context.id;
            let not_prim = id != prim_id;
            not_self && (not_prim || has_prim)
        });

        let mut items = vec![];

        for import_id in file_ids {
            let Some(resolved) = context.engine.resolved(import_id).ok() else {
                continue;
            };

            let source = T::exports(&resolved)
                .filter(|(name, file_id, _)| filter.matches(name) && *file_id == import_id)
                .map(|(name, file_id, item_id)| T::candidate(context, name, file_id, item_id));

            items.extend(source);
        }

        items.into_iter()
    }
}

/// Yields terms for implicit Prim.
pub struct PrimTerms;

impl Source for PrimTerms {
    fn candidates<F: Filter>(
        &self,
        context: &Context,
        filter: F,
    ) -> impl Iterator<Item = CompletionItem> {
        let source = context
            .prim_resolved
            .exports
            .iter_terms()
            .filter(move |(name, _, _)| filter.matches(name));
        source.map(|(name, f, t)| {
            let description = Some("Prim".to_string());
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

/// Yields types for implicit Prim.
pub struct PrimTypes;

impl Source for PrimTypes {
    fn candidates<F: Filter>(
        &self,
        context: &Context,
        filter: F,
    ) -> impl Iterator<Item = CompletionItem> {
        let source = context
            .prim_resolved
            .exports
            .iter_types()
            .filter(move |(name, _, _)| filter.matches(name));
        source.map(|(name, f, t)| {
            let description = Some("Prim".to_string());
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
