use async_lsp::lsp_types::*;
use files::FileId;
use indexing::{ImportKind, TermItemId, TypeItemId};
use resolving::ResolvedModule;
use smol_str::SmolStr;

use crate::AnalyzerError;

use super::{
    edit, filter::PerfectSegmentFuzzy, item::CompletionItemSpec, prelude::*,
    resolve::CompletionResolveData,
};

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

impl CompletionSource for QualifiedModules {
    type T = ();

    fn collect_into<F: Filter>(
        &self,
        context: &Context,
        filter: F,
        items: &mut Vec<CompletionItem>,
    ) -> Result<Self::T, AnalyzerError> {
        let source = context.resolved.qualified.iter();
        let source = source.filter(move |(name, _)| filter.matches(name));

        for (name, import) in source {
            let (parsed, _) = context.engine.parsed(import.file)?;
            let description = parsed.module_name().map(|name| name.to_string());

            let mut item = CompletionItemSpec::new(
                name.to_string(),
                context.range,
                CompletionItemKind::MODULE,
                CompletionResolveData::Import(import.file),
            );

            if let Some(description) = description {
                item.label_description(description);
            }

            items.push(item.build());
        }

        Ok(())
    }
}

/// Yields terms defined in the current module.
pub struct LocalTerms;

impl CompletionSource for LocalTerms {
    type T = ();

    fn collect_into<F: Filter>(
        &self,
        context: &Context,
        filter: F,
        items: &mut Vec<CompletionItem>,
    ) -> Result<Self::T, AnalyzerError> {
        let source = context.resolved.locals.iter_terms();
        let source = source.filter(move |(name, _, _)| filter.matches(name));

        for (name, file_id, term_id) in source {
            let mut item = CompletionItemSpec::new(
                name.to_string(),
                context.range,
                CompletionItemKind::VALUE,
                CompletionResolveData::TermItem(file_id, term_id),
            );

            item.label_description("Local".to_string());

            items.push(item.build())
        }

        Ok(())
    }
}

/// Yields types defined in the current module.
pub struct LocalTypes;

impl CompletionSource for LocalTypes {
    type T = ();

    fn collect_into<F: Filter>(
        &self,
        context: &Context,
        filter: F,
        items: &mut Vec<CompletionItem>,
    ) -> Result<Self::T, AnalyzerError> {
        let source = context.resolved.locals.iter_types();
        let source = source.filter(move |(name, _, _)| filter.matches(name));

        for (name, file_id, type_id) in source {
            let mut item = CompletionItemSpec::new(
                name.to_string(),
                context.range,
                CompletionItemKind::STRUCT,
                CompletionResolveData::TypeItem(file_id, type_id),
            );

            item.label_description("Local".to_string());

            items.push(item.build())
        }

        Ok(())
    }
}

/// Yields terms from unqualified imports.
pub struct ImportedTerms;

impl CompletionSource for ImportedTerms {
    type T = ();

    fn collect_into<F: Filter>(
        &self,
        context: &Context,
        filter: F,
        items: &mut Vec<CompletionItem>,
    ) -> Result<Self::T, AnalyzerError> {
        let source = context.resolved.unqualified.values().flatten();

        for import in source {
            let source = import.iter_terms().filter(move |(name, _, _, kind)| {
                filter.matches(name) && !matches!(kind, ImportKind::Hidden)
            });

            for (name, file_id, term_id, _) in source {
                let (parsed, _) = context.engine.parsed(file_id)?;
                let description = parsed.module_name().map(|name| name.to_string());

                let mut item = CompletionItemSpec::new(
                    name.to_string(),
                    context.range,
                    CompletionItemKind::VALUE,
                    CompletionResolveData::TermItem(file_id, term_id),
                );

                if let Some(description) = description {
                    item.label_description(description);
                }

                items.push(item.build())
            }
        }

        Ok(())
    }
}

/// Yields types from unqualified imports.
pub struct ImportedTypes;

impl CompletionSource for ImportedTypes {
    type T = ();

    fn collect_into<F: Filter>(
        &self,
        context: &Context,
        filter: F,
        items: &mut Vec<CompletionItem>,
    ) -> Result<Self::T, AnalyzerError> {
        let source = context.resolved.unqualified.values().flatten();

        for import in source {
            let source = import.iter_types().filter(move |(name, _, _, kind)| {
                filter.matches(name) && !matches!(kind, ImportKind::Hidden)
            });
            for (name, f, t, _) in source {
                let (parsed, _) = context.engine.parsed(f)?;
                let description = parsed.module_name().map(|name| name.to_string());

                let mut item = CompletionItemSpec::new(
                    name.to_string(),
                    context.range,
                    CompletionItemKind::STRUCT,
                    CompletionResolveData::TypeItem(f, t),
                );

                if let Some(description) = description {
                    item.label_description(description);
                }

                items.push(item.build())
            }
        }

        Ok(())
    }
}

/// Yields terms from qualified imports.
pub struct QualifiedTerms<'a>(pub &'a str);

impl CompletionSource for QualifiedTerms<'_> {
    type T = ();

    fn collect_into<F: Filter>(
        &self,
        context: &Context,
        filter: F,
        items: &mut Vec<CompletionItem>,
    ) -> Result<Self::T, AnalyzerError> {
        let Some(import) = context.resolved.qualified.get(self.0) else {
            return Ok(());
        };

        let source = import.iter_terms().filter(move |(name, _, _, kind)| {
            filter.matches(name) && !matches!(kind, ImportKind::Hidden)
        });

        for (name, file_id, term_id, _) in source {
            let (parsed, _) = context.engine.parsed(file_id)?;
            let description = parsed.module_name().map(|name| name.to_string());

            let mut item = CompletionItemSpec::new(
                name.to_string(),
                context.range,
                CompletionItemKind::VALUE,
                CompletionResolveData::TermItem(file_id, term_id),
            );

            item.edit_text(format!("{}.{name}", self.0));
            if let Some(description) = description {
                item.label_description(description);
            }

            items.push(item.build())
        }

        Ok(())
    }
}

/// Yields types from qualified imports.
pub struct QualifiedTypes<'a>(pub &'a str);

impl CompletionSource for QualifiedTypes<'_> {
    type T = ();

    fn collect_into<F: Filter>(
        &self,
        context: &Context,
        filter: F,
        items: &mut Vec<CompletionItem>,
    ) -> Result<Self::T, AnalyzerError> {
        let Some(import) = context.resolved.qualified.get(self.0) else {
            return Ok(());
        };

        let source = import.iter_types().filter(move |(name, _, _, kind)| {
            filter.matches(name) && !matches!(kind, ImportKind::Hidden)
        });

        for (name, file_id, type_id, _) in source {
            let (parsed, _) = context.engine.parsed(file_id)?;
            let description = parsed.module_name().map(|name| name.to_string());

            let mut item = CompletionItemSpec::new(
                name.to_string(),
                context.range,
                CompletionItemKind::STRUCT,
                CompletionResolveData::TypeItem(file_id, type_id),
            );

            item.edit_text(format!("{}.{name}", self.0));
            if let Some(description) = description {
                item.label_description(description);
            }

            items.push(item.build())
        }

        Ok(())
    }
}

/// Yields suggestions for terms.
pub struct SuggestedTerms;

/// Yields suggestions for types.
pub struct SuggestedTypes;

trait SuggestionsHelper {
    type ItemId;

    fn exports(resolved: &ResolvedModule)
    -> impl Iterator<Item = (&SmolStr, FileId, Self::ItemId)>;

    fn candidate(
        &self,
        context: &Context,
        name: &SmolStr,
        import_id: FileId,
        file_id: FileId,
        item_id: Self::ItemId,
    ) -> Result<Option<CompletionItem>, AnalyzerError>;
}

impl SuggestionsHelper for SuggestedTerms {
    type ItemId = TermItemId;

    fn exports(resolved: &ResolvedModule) -> impl Iterator<Item = (&SmolStr, FileId, TermItemId)> {
        resolved.exports.iter_terms()
    }

    fn candidate(
        &self,
        context: &Context,
        name: &SmolStr,
        import_id: FileId,
        file_id: FileId,
        item_id: Self::ItemId,
    ) -> Result<Option<CompletionItem>, AnalyzerError> {
        assert_eq!(import_id, file_id);

        if context.has_term_import(None, name) {
            return Ok(None);
        }

        let (parsed, _) = context.engine.parsed(file_id)?;
        let Some(module_name) = parsed.module_name() else {
            return Ok(None);
        };

        let mut item = CompletionItemSpec::new(
            name.to_string(),
            context.range,
            CompletionItemKind::VALUE,
            CompletionResolveData::TermItem(file_id, item_id),
        );

        let (import_text, import_range) =
            edit::term_import_item(context, &module_name, name, file_id, item_id);

        let range_new_text =
            import_range.or_else(|| context.insert_import_range()).zip(import_text);

        item.label_detail(format!(" (import {module_name})"));
        item.label_description(format!("{module_name}"));
        item.sort_text(format!("{module_name}.{name}"));

        if let Some((range, new_text)) = range_new_text {
            item.additional_text_edits(vec![TextEdit { range, new_text }]);
        }

        Ok(Some(item.build()))
    }
}

impl SuggestionsHelper for SuggestedTypes {
    type ItemId = TypeItemId;

    fn exports(
        resolved: &ResolvedModule,
    ) -> impl Iterator<Item = (&SmolStr, FileId, Self::ItemId)> {
        resolved.exports.iter_types()
    }

    fn candidate(
        &self,
        context: &Context,
        name: &SmolStr,
        import_id: FileId,
        file_id: FileId,
        item_id: Self::ItemId,
    ) -> Result<Option<CompletionItem>, AnalyzerError> {
        assert_eq!(import_id, file_id);

        if context.has_type_import(None, name) {
            return Ok(None);
        }

        let (parsed, _) = context.engine.parsed(file_id)?;
        let Some(module_name) = parsed.module_name() else {
            return Ok(None);
        };

        let mut item = CompletionItemSpec::new(
            name.to_string(),
            context.range,
            CompletionItemKind::STRUCT,
            CompletionResolveData::TypeItem(file_id, item_id),
        );

        let (import_text, import_range) =
            edit::type_import_item(context, &module_name, name, file_id, item_id);

        let range_new_text =
            import_range.or_else(|| context.insert_import_range()).zip(import_text);

        item.label_detail(format!(" (import {module_name})"));
        item.label_description(format!("{module_name}"));
        item.sort_text(format!("{module_name}.{name}"));

        if let Some((range, new_text)) = range_new_text {
            item.additional_text_edits(vec![TextEdit { range, new_text }]);
        }

        Ok(Some(item.build()))
    }
}

fn suggestions_candidates<T: SuggestionsHelper>(
    this: &T,
    context: &Context,
    filter: impl Filter,
    items: &mut Vec<CompletionItem>,
) -> Result<(), AnalyzerError> {
    let has_prim = context
        .resolved
        .unqualified
        .values()
        .flatten()
        .any(|import| import.file == context.prim_id);

    let file_ids = context.files.iter_id().filter(move |&id| {
        let not_self = id != context.current_file;
        let not_prim = id != context.prim_id;
        not_self && (not_prim || has_prim)
    });

    for import_id in file_ids {
        let resolved = context.engine.resolved(import_id)?;

        let source = T::exports(&resolved)
            .filter(|(name, file_id, _)| filter.matches(name) && *file_id == import_id);

        for (name, file_id, item_id) in source {
            if let Some(item) = this.candidate(context, name, import_id, file_id, item_id)? {
                items.push(item);
            }
        }
    }

    Ok(())
}

impl CompletionSource for SuggestedTerms {
    type T = ();

    fn collect_into<F: Filter>(
        &self,
        context: &Context,
        filter: F,
        items: &mut Vec<CompletionItem>,
    ) -> Result<Self::T, AnalyzerError> {
        suggestions_candidates(self, context, filter, items)
    }
}

impl CompletionSource for SuggestedTypes {
    type T = ();

    fn collect_into<F: Filter>(
        &self,
        context: &Context,
        filter: F,
        items: &mut Vec<CompletionItem>,
    ) -> Result<Self::T, AnalyzerError> {
        suggestions_candidates(self, context, filter, items)
    }
}

/// Yields terms for implicit Prim.
pub struct PrimTerms;

impl CompletionSource for PrimTerms {
    type T = ();

    fn collect_into<F: Filter>(
        &self,
        context: &Context,
        filter: F,
        items: &mut Vec<CompletionItem>,
    ) -> Result<Self::T, AnalyzerError> {
        let source = context
            .prim_resolved
            .exports
            .iter_terms()
            .filter(move |(name, _, _)| filter.matches(name));

        for (name, file_id, term_id) in source {
            let mut item = CompletionItemSpec::new(
                name.to_string(),
                context.range,
                CompletionItemKind::VALUE,
                CompletionResolveData::TermItem(file_id, term_id),
            );

            item.label_description("Prim".to_string());

            items.push(item.build())
        }

        Ok(())
    }
}

/// Yields types for implicit Prim.
pub struct PrimTypes;

impl CompletionSource for PrimTypes {
    type T = ();

    fn collect_into<F: Filter>(
        &self,
        context: &Context,
        filter: F,
        items: &mut Vec<CompletionItem>,
    ) -> Result<Self::T, AnalyzerError> {
        let source = context
            .prim_resolved
            .exports
            .iter_types()
            .filter(move |(name, _, _)| filter.matches(name));

        for (name, file_id, type_item) in source {
            let mut item = CompletionItemSpec::new(
                name.to_string(),
                context.range,
                CompletionItemKind::STRUCT,
                CompletionResolveData::TypeItem(file_id, type_item),
            );

            item.label_description("Prim".to_string());

            items.push(item.build())
        }

        Ok(())
    }
}

/// Yields suggestions for qualified terms.
pub struct QualifiedTermsSuggestions<'a>(pub &'a str);

/// Yields suggestions for qualified types.
pub struct QualifiedTypesSuggestions<'a>(pub &'a str);

impl SuggestionsHelper for QualifiedTermsSuggestions<'_> {
    type ItemId = TermItemId;

    fn exports(
        resolved: &ResolvedModule,
    ) -> impl Iterator<Item = (&SmolStr, FileId, Self::ItemId)> {
        resolved.exports.iter_terms()
    }

    fn candidate(
        &self,
        context: &Context,
        name: &SmolStr,
        import_id: FileId,
        file_id: FileId,
        item_id: Self::ItemId,
    ) -> Result<Option<CompletionItem>, AnalyzerError> {
        let (parsed, _) = context.engine.parsed(import_id)?;
        let Some(module_name) = parsed.module_name() else {
            return Ok(None);
        };

        let mut item = CompletionItemSpec::new(
            name.to_string(),
            context.range,
            CompletionItemKind::VALUE,
            CompletionResolveData::TermItem(file_id, item_id),
        );

        item.label_detail(format!(" (import {module_name} as {})", self.0));
        item.label_description(format!("{module_name}"));

        item.edit_text(format!("{}.{name}", self.0));
        item.sort_text(format!("{module_name}.{name}"));

        if let Some(range) = context.insert_import_range() {
            let new_text = format!("import {module_name} as {}\n", self.0);
            item.additional_text_edits(vec![TextEdit { range, new_text }]);
        }

        Ok(Some(item.build()))
    }
}

impl SuggestionsHelper for QualifiedTypesSuggestions<'_> {
    type ItemId = TypeItemId;

    fn exports(
        resolved: &ResolvedModule,
    ) -> impl Iterator<Item = (&SmolStr, FileId, Self::ItemId)> {
        resolved.exports.iter_types()
    }

    fn candidate(
        &self,
        context: &Context,
        name: &SmolStr,
        import_id: FileId,
        file_id: FileId,
        item_id: Self::ItemId,
    ) -> Result<Option<CompletionItem>, AnalyzerError> {
        let (parsed, _) = context.engine.parsed(import_id)?;
        let Some(module_name) = parsed.module_name() else {
            return Ok(None);
        };

        let mut item = CompletionItemSpec::new(
            name.to_string(),
            context.range,
            CompletionItemKind::STRUCT,
            CompletionResolveData::TypeItem(file_id, item_id),
        );

        item.label_detail(format!(" (import {module_name} as {})", self.0));
        item.label_description(format!("{module_name}"));

        item.edit_text(format!("{}.{name}", self.0));
        item.sort_text(format!("{module_name}.{name}"));

        if let Some(range) = context.insert_import_range() {
            let new_text = format!("import {module_name} as {}\n", self.0);
            item.additional_text_edits(vec![TextEdit { range, new_text }]);
        }

        Ok(Some(item.build()))
    }
}

fn suggestions_candidates_qualified<T: SuggestionsHelper>(
    this: &T,
    prefix: &str,
    context: &Context,
    filter: impl Filter,
    items: &mut Vec<CompletionItem>,
) -> Result<(), AnalyzerError> {
    let has_prim = context.resolved.qualified.values().any(|import| import.file == context.prim_id);

    let file_ids = context.files.iter_id().filter(move |&id| {
        let not_self = id != context.current_file;
        let not_prim = id != context.prim_id;
        not_self && (not_prim || has_prim)
    });

    for import_id in file_ids {
        let (parsed, _) = context.engine.parsed(import_id)?;
        let resolved = context.engine.resolved(import_id)?;

        if parsed.module_name().is_some_and(|module_name| {
            let filter = PerfectSegmentFuzzy(&module_name);
            !filter.matches(prefix)
        }) {
            continue;
        }

        let source = T::exports(&resolved).filter(|(name, _, _)| filter.matches(name));

        for (name, file_id, item_id) in source {
            if let Some(item) = this.candidate(context, name, import_id, file_id, item_id)? {
                items.push(item);
            }
        }
    }

    Ok(())
}

impl CompletionSource for QualifiedTermsSuggestions<'_> {
    type T = ();

    fn collect_into<F: Filter>(
        &self,
        context: &Context,
        filter: F,
        items: &mut Vec<CompletionItem>,
    ) -> Result<Self::T, AnalyzerError> {
        suggestions_candidates_qualified(self, self.0, context, filter, items)
    }
}

impl CompletionSource for QualifiedTypesSuggestions<'_> {
    type T = ();

    fn collect_into<F: Filter>(
        &self,
        context: &Context,
        filter: F,
        items: &mut Vec<CompletionItem>,
    ) -> Result<Self::T, AnalyzerError> {
        suggestions_candidates_qualified(self, self.0, context, filter, items)
    }
}

/// Yields module names in the workspace.
pub struct WorkspaceModules;

impl CompletionSource for WorkspaceModules {
    type T = ();

    fn collect_into<F: Filter>(
        &self,
        context: &Context,
        filter: F,
        items: &mut Vec<CompletionItem>,
    ) -> Result<Self::T, AnalyzerError> {
        for id in context.files.iter_id() {
            let (parsed, _) = context.engine.parsed(id)?;

            let Some(module_name) = parsed.module_name() else {
                continue;
            };

            if !filter.matches(&module_name) {
                continue;
            }

            let mut item = CompletionItemSpec::new(
                module_name.to_string(),
                context.range,
                CompletionItemKind::MODULE,
                CompletionResolveData::Import(id),
            );

            item.label_description(format!("{module_name}"));

            items.push(item.build())
        }

        Ok(())
    }
}
