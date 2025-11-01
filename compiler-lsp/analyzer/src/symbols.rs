use async_lsp::lsp_types::*;
use building::QueryEngine;
use files::{FileId, Files};
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

    let mut output = vec![];

    let candidates =
        if let Some(cached_files) = cache.get(query).or_else(|| cache.get_ancestor_value(query)) {
            collect_files(engine, files, query, &mut output, cached_files.iter().copied())?
        } else {
            collect_files(engine, files, query, &mut output, files.iter_id())?
        };

    let query = query.to_string();
    cache.insert(query, candidates);

    Ok(Some(WorkspaceSymbolResponse::Flat(output)))
}

fn collect_files(
    engine: &QueryEngine,
    files: &Files,
    query: &str,
    output: &mut Vec<SymbolInformation>,
    to_search: impl Iterator<Item = FileId>,
) -> Result<Vec<FileId>, AnalyzerError> {
    let mut candidates = vec![];

    for file_id in to_search {
        let previous_size = output.len();
        collect_terms_types(engine, files, query, output, file_id)?;
        if output.len() > previous_size {
            candidates.push(file_id);
        }
    }

    Ok(candidates)
}

fn collect_terms_types(
    engine: &QueryEngine,
    files: &Files,
    query: &str,
    output: &mut Vec<SymbolInformation>,
    file_id: FileId,
) -> Result<(), AnalyzerError> {
    let resolved = engine.resolved(file_id)?;
    let uri = common::file_uri(engine, files, file_id)?;

    let terms = resolved
        .locals
        .iter_terms()
        .filter_map(|(name, _, id)| if name.starts_with(query) { Some((name, id)) } else { None });

    let types = resolved
        .locals
        .iter_types()
        .filter_map(|(name, _, id)| if name.starts_with(query) { Some((name, id)) } else { None });

    for (name, term_id) in terms {
        let uri = uri.clone();
        let location = common::file_term_location(engine, uri, file_id, term_id)?;
        output.push(SymbolInformation {
            name: name.to_string(),
            kind: SymbolKind::FUNCTION,
            tags: None,
            #[allow(deprecated)]
            deprecated: None,
            location,
            container_name: None,
        })
    }

    for (name, type_id) in types {
        let uri = uri.clone();
        let location = common::file_type_location(engine, uri, file_id, type_id)?;
        output.push(SymbolInformation {
            name: name.to_string(),
            kind: SymbolKind::CLASS,
            tags: None,
            #[allow(deprecated)]
            deprecated: None,
            location,
            container_name: None,
        })
    }

    Ok(())
}

pub type WorkspaceSymbolsCache = Trie<String, Vec<FileId>>;
