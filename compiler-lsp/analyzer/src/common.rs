use async_lsp::lsp_types::*;
use building::{QueryEngine, prim};
use files::{FileId, Files};
use indexing::{TermItemId, TypeItemId};
use syntax::{SyntaxNode, SyntaxNodePtr};

use crate::{AnalyzerError, locate};

pub fn file_term_location(
    engine: &QueryEngine,
    uri: Url,
    file_id: FileId,
    term_id: TermItemId,
) -> Result<Location, AnalyzerError> {
    let content = engine.content(file_id);
    let (parsed, _) = engine.parsed(file_id)?;

    let stabilized = engine.stabilized(file_id)?;
    let indexed = engine.indexed(file_id)?;

    let root = parsed.syntax_node();
    let pointers = indexed.term_item_ptr(&stabilized, term_id);

    let range = pointers_range(&content, root, pointers)?;
    Ok(Location { uri, range })
}

pub fn file_type_location(
    engine: &QueryEngine,
    uri: Url,
    file_id: FileId,
    type_id: TypeItemId,
) -> Result<Location, AnalyzerError> {
    let content = engine.content(file_id);
    let (parsed, _) = engine.parsed(file_id)?;

    let stabilized = engine.stabilized(file_id)?;
    let indexed = engine.indexed(file_id)?;

    let root = parsed.syntax_node();
    let pointers = indexed.type_item_ptr(&stabilized, type_id);

    let range = pointers_range(&content, root, pointers)?;

    Ok(Location { uri, range })
}

pub fn file_uri(
    engine: &QueryEngine,
    files: &Files,
    file_id: FileId,
) -> Result<Url, AnalyzerError> {
    let path = files.path(file_id);
    let content = engine.content(file_id);

    let uri = Url::parse(&path)?;
    prim::handle_generated(uri, &content).ok_or(AnalyzerError::NonFatal)
}

fn pointers_range(
    content: &str,
    root: SyntaxNode,
    pointers: impl Iterator<Item = SyntaxNodePtr>,
) -> Result<Range, AnalyzerError> {
    pointers
        .filter_map(|ptr| locate::syntax_range(content, &root, &ptr))
        .reduce(|start, end| Range { start: start.start, end: end.end })
        .ok_or(AnalyzerError::NonFatal)
}
