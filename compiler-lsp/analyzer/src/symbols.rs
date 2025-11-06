use std::sync::Arc;

use async_lsp::lsp_types::*;
use building::QueryEngine;
use files::Files;
use radix_trie::Trie;

use crate::{AnalyzerError, common};

pub fn workspace(
    engine: &QueryEngine,
    files: &Files,
    cache: &mut WorkspaceSymbolsCache,
    query: &str,
) -> Result<Option<WorkspaceSymbolResponse>, AnalyzerError> {
    if query.is_empty() {
        return Ok(None);
    }

    let query = query.to_lowercase();

    if let Some(exact_symbols) = cache.get(&query) {
        tracing::debug!("Found exact match for '{query}'");
        let flat = Vec::clone(&*exact_symbols);
        return Ok(Some(WorkspaceSymbolResponse::Flat(flat)));
    }

    let symbols = if let Some(prefix_symbols) = cache.get_ancestor_value(&query) {
        tracing::debug!("Found prefix match for '{query}'");
        let filtered_symbols = filter_symbols(prefix_symbols, &query);
        if filtered_symbols.len() == prefix_symbols.len() {
            Arc::clone(prefix_symbols)
        } else {
            Arc::new(filtered_symbols)
        }
    } else {
        tracing::debug!("Initialising cache for '{query}'");
        let filtered_symbols = build_symbol_list(engine, files, &query)?;
        Arc::new(filtered_symbols)
    };

    let key = String::clone(&query);
    let value = Arc::clone(&symbols);
    cache.insert(key, value);

    let flat = Vec::clone(&*symbols);
    Ok(Some(WorkspaceSymbolResponse::Flat(flat)))
}

fn filter_symbols(cached: &[SymbolInformation], query: &str) -> Vec<SymbolInformation> {
    cached.iter().filter(|symbol| symbol.name.to_lowercase().starts_with(query)).cloned().collect()
}

fn build_symbol_list(
    engine: &QueryEngine,
    files: &Files,
    query: &str,
) -> Result<Vec<SymbolInformation>, AnalyzerError> {
    let mut symbols = vec![];

    for file_id in files.iter_id() {
        let resolved = engine.resolved(file_id)?;
        let uri = common::file_uri(engine, files, file_id)?;

        for (name, _, term_id) in resolved.locals.iter_terms() {
            if !name.to_lowercase().starts_with(query) {
                continue;
            }
            let location = common::file_term_location(engine, uri.clone(), file_id, term_id)?;
            symbols.push(SymbolInformation {
                name: name.to_string(),
                kind: SymbolKind::FUNCTION,
                tags: None,
                #[allow(deprecated)]
                deprecated: None,
                location,
                container_name: None,
            });
        }

        for (name, _, type_id) in resolved.locals.iter_types() {
            if !name.to_lowercase().starts_with(query) {
                continue;
            }
            let location = common::file_type_location(engine, uri.clone(), file_id, type_id)?;
            symbols.push(SymbolInformation {
                name: name.to_string(),
                kind: SymbolKind::CLASS,
                tags: None,
                #[allow(deprecated)]
                deprecated: None,
                location,
                container_name: None,
            });
        }
    }

    Ok(symbols)
}

pub type WorkspaceSymbolsCache = Trie<String, Arc<Vec<SymbolInformation>>>;
