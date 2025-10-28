use async_lsp::lsp_types::*;
use building::QueryEngine;
use files::Files;

use crate::{AnalyzerError, common};

pub fn workspace(
    engine: &QueryEngine,
    files: &Files,
    query: &str,
) -> Result<Option<WorkspaceSymbolResponse>, AnalyzerError> {
    let mut symbols = vec![];

    let allow_query = |name: &str| query.is_empty() || name.starts_with(query);

    for file_id in files.iter_id() {
        let resolved = engine.resolved(file_id)?;

        let terms = resolved
            .locals
            .iter_terms()
            .filter_map(|(name, _, id)| if allow_query(&name) { Some((name, id)) } else { None });

        let types = resolved
            .locals
            .iter_types()
            .filter_map(|(name, _, id)| if allow_query(&name) { Some((name, id)) } else { None });

        for (name, term_id) in terms {
            let location = common::file_term_location(engine, files, file_id, term_id)?;
            symbols.push(SymbolInformation {
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
            let location = common::file_type_location(engine, files, file_id, type_id)?;
            symbols.push(SymbolInformation {
                name: name.to_string(),
                kind: SymbolKind::CLASS,
                tags: None,
                #[allow(deprecated)]
                deprecated: None,
                location,
                container_name: None,
            })
        }
    }

    Ok(Some(WorkspaceSymbolResponse::Flat(symbols)))
}
