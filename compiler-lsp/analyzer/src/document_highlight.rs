use async_lsp::lsp_types::*;
use building::QueryEngine;
use files::{FileId, Files};
use lowering::{
    BinderId, ExpressionId, ExpressionKind, LetBindingNameGroupId, TermVariableResolution,
};
use rowan::ast::AstNode;
use stabilizing::{AstId, StabilizedModule};
use syntax::{PureScript, SyntaxNode, SyntaxNodePtr, cst};

use crate::{AnalyzerError, locate};

fn top_level_definition_name_ranges(
    engine: &QueryEngine,
    file_id: FileId,
    term_id: indexing::TermItemId,
) -> Result<Vec<Range>, AnalyzerError> {
    let content = engine.content(file_id);
    let (parsed, _) = engine.parsed(file_id)?;
    let stabilized = engine.stabilized(file_id)?;
    let indexed = engine.indexed(file_id)?;

    let root = parsed.syntax_node();
    let mut ranges = vec![];

    if let indexing::TermItemKind::Value { signature, equations } = &indexed.items[term_id].kind {
        if let Some(sig_id) = signature
            && let Some(ptr) = stabilized.ast_ptr(*sig_id)
            && let Some(node) = ptr.try_to_node(&root)
            && let Some(tok) = node.name_token()
            && let Some(range) = locate::text_range_to_range(&content, tok.text_range())
        {
            ranges.push(range);
        }

        for eq_id in equations {
            if let Some(ptr) = stabilized.ast_ptr(*eq_id)
                && let Some(node) = ptr.try_to_node(&root)
                && let Some(tok) = node.name_token()
                && let Some(range) = locate::text_range_to_range(&content, tok.text_range())
            {
                ranges.push(range);
            }
        }
    }

    Ok(ranges)
}

fn id_range<T>(
    content: &str,
    parsed: &parsing::ParsedModule,
    stabilized: &StabilizedModule,
    item_id: AstId<T>,
) -> Option<Range>
where
    T: AstNode<Language = PureScript>,
{
    let root = parsed.syntax_node();
    let ptr = stabilized.syntax_ptr(item_id)?;
    locate::syntax_range(content, &root, &ptr)
}

fn current_file_id(files: &Files, uri: &Url) -> Result<FileId, AnalyzerError> {
    let uri = uri.as_str();
    files.id(uri).ok_or(AnalyzerError::NonFatal)
}

fn token_range(content: &str, range: rowan::TextRange) -> Option<Range> {
    locate::text_range_to_range(content, range)
}

fn binder_name_range(content: &str, root: &SyntaxNode, ptr: &SyntaxNodePtr) -> Option<Range> {
    let node = ptr.try_to_node(root)?;

    if let Some(binder) = cst::BinderVariable::cast(node.clone()) {
        let tok = binder.name_token()?;
        return token_range(content, tok.text_range());
    }

    if let Some(binder) = cst::BinderNamed::cast(node) {
        let tok = binder.name_token()?;
        return token_range(content, tok.text_range());
    }

    None
}

fn let_signature_name_range(
    content: &str,
    root: &SyntaxNode,
    ptr: &SyntaxNodePtr,
) -> Option<Range> {
    let node = ptr.try_to_node(root)?;
    let sig = cst::LetBindingSignature::cast(node)?;
    let tok = sig.name_token()?;
    token_range(content, tok.text_range())
}

fn let_equation_name_range(content: &str, root: &SyntaxNode, ptr: &SyntaxNodePtr) -> Option<Range> {
    let node = ptr.try_to_node(root)?;
    let eq = cst::LetBindingEquation::cast(node)?;
    let tok = eq.name_token()?;
    token_range(content, tok.text_range())
}

fn highlight_binder(
    engine: &QueryEngine,
    current_file: FileId,
    binder_id: BinderId,
) -> Result<Option<Vec<DocumentHighlight>>, AnalyzerError> {
    let content = engine.content(current_file);
    let (parsed, _) = engine.parsed(current_file)?;
    let stabilized = engine.stabilized(current_file)?;
    let lowered = engine.lowered(current_file)?;

    let root = parsed.syntax_node();
    let ptr = stabilized.syntax_ptr(binder_id).ok_or(AnalyzerError::NonFatal)?;

    let mut ranges: Vec<Range> = vec![];

    if let Some(range) = binder_name_range(&content, &root, &ptr)
        .or_else(|| locate::syntax_range(&content, &root, &ptr))
    {
        ranges.push(range);
    }

    for (expr_id, expr_kind) in lowered.info.iter_expression() {
        if let ExpressionKind::Variable {
            resolution: Some(TermVariableResolution::Binder(id)), ..
        } = expr_kind
            && *id == binder_id
            && let Some(range) = id_range(&content, &parsed, &stabilized, expr_id)
        {
            ranges.push(range);
        }
    }

    ranges.sort_by_key(|r| (r.start.line, r.start.character, r.end.line, r.end.character));
    ranges.dedup();

    Ok((!ranges.is_empty())
        .then(|| ranges.into_iter().map(|range| DocumentHighlight { range, kind: None }).collect()))
}

fn highlight_let(
    engine: &QueryEngine,
    current_file: FileId,
    let_binding_id: LetBindingNameGroupId,
) -> Result<Option<Vec<DocumentHighlight>>, AnalyzerError> {
    let content = engine.content(current_file);
    let (parsed, _) = engine.parsed(current_file)?;
    let stabilized = engine.stabilized(current_file)?;
    let lowered = engine.lowered(current_file)?;

    let root = parsed.syntax_node();
    let binding = lowered.info.get_let_binding_group(let_binding_id);

    let mut ranges: Vec<Range> = vec![];

    if let Some(sig) = binding.signature {
        let ptr = stabilized.syntax_ptr(sig).ok_or(AnalyzerError::NonFatal)?;
        if let Some(range) = let_signature_name_range(&content, &root, &ptr)
            .or_else(|| locate::syntax_range(&content, &root, &ptr))
        {
            ranges.push(range);
        }
    }

    for &eq in binding.equations.iter() {
        let ptr = stabilized.syntax_ptr(eq).ok_or(AnalyzerError::NonFatal)?;
        if let Some(range) = let_equation_name_range(&content, &root, &ptr)
            .or_else(|| locate::syntax_range(&content, &root, &ptr))
        {
            ranges.push(range);
        }
    }

    for (expr_id, expr_kind) in lowered.info.iter_expression() {
        if let ExpressionKind::Variable {
            resolution: Some(TermVariableResolution::Let(id)), ..
        } = expr_kind
            && *id == let_binding_id
            && let Some(range) = id_range(&content, &parsed, &stabilized, expr_id)
        {
            ranges.push(range);
        }
    }

    ranges.sort_by_key(|r| (r.start.line, r.start.character, r.end.line, r.end.character));
    ranges.dedup();

    Ok((!ranges.is_empty())
        .then(|| ranges.into_iter().map(|range| DocumentHighlight { range, kind: None }).collect()))
}

fn highlight_expression(
    engine: &QueryEngine,
    current_file: FileId,
    expression_id: ExpressionId,
) -> Result<Option<Vec<DocumentHighlight>>, AnalyzerError> {
    let lowered = engine.lowered(current_file)?;
    let kind = lowered.info.get_expression_kind(expression_id).ok_or(AnalyzerError::NonFatal)?;

    if let ExpressionKind::Variable { resolution: Some(resolution), .. } = kind {
        match resolution {
            TermVariableResolution::Binder(binder_id) => {
                highlight_binder(engine, current_file, *binder_id)
            }
            TermVariableResolution::Let(let_binding_id) => {
                highlight_let(engine, current_file, *let_binding_id)
            }
            TermVariableResolution::RecordPun(_) => Ok(None),
            TermVariableResolution::Reference(..) => Ok(None),
        }
    } else {
        Ok(None)
    }
}

pub fn implementation(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    position: Position,
) -> Result<Option<Vec<DocumentHighlight>>, AnalyzerError> {
    let current_file = current_file_id(files, &uri)?;

    // If the cursor resolves to a top-level value in the current file, include
    // the definition name tokens alongside the reference highlights.
    let mut extra_ranges: Vec<Range> = vec![];

    // Local binders/lets do not have stable (file, item) identities in the workspace
    // index, so `textDocument/references` intentionally returns `None` for them.
    // For `textDocument/documentHighlight`, we still want local occurrences.
    let located = locate::locate(engine, current_file, position)?;
    match located {
        locate::Located::Binder(binder_id) => {
            if let Some(highlights) = highlight_binder(engine, current_file, binder_id)? {
                return Ok(Some(highlights));
            }
        }
        locate::Located::Expression(expression_id) => {
            if let Some(highlights) = highlight_expression(engine, current_file, expression_id)? {
                return Ok(Some(highlights));
            }

            let lowered = engine.lowered(current_file)?;
            if let Some(ExpressionKind::Variable {
                resolution: Some(TermVariableResolution::Reference(f_id, t_id)),
                ..
            }) = lowered.info.get_expression_kind(expression_id)
                && (*f_id) == current_file
            {
                extra_ranges = top_level_definition_name_ranges(engine, current_file, *t_id)?;
            }
        }
        _ => {}
    }

    // If the cursor is on a `where`/`let` *definition* name token, `locate` currently
    // returns `Nothing` (it's a LetBinding* node, not an Expression). Recover by
    // mapping the CST let binding to its lowering group id.
    {
        let content = engine.content(current_file);
        let (parsed, _) = engine.parsed(current_file)?;
        let stabilized = engine.stabilized(current_file)?;
        let lowered = engine.lowered(current_file)?;

        if let Some(offset) = locate::position_to_offset(&content, position) {
            let root = parsed.syntax_node();
            let token = match root.token_at_offset(offset) {
                rowan::TokenAtOffset::None => None,
                rowan::TokenAtOffset::Single(token) => Some(token),
                rowan::TokenAtOffset::Between(_, right) => Some(right),
            };

            if let Some(token) = token {
                for node in token.parent_ancestors() {
                    if let Some(eq) = cst::LetBindingEquation::cast(node.clone())
                        && let Some(eq_id) = stabilized.lookup_cst(&eq)
                        && let Some(group_id) = lowered.info.let_binding_group_for_equation(eq_id)
                        && let Some(highlights) = highlight_let(engine, current_file, group_id)?
                    {
                        return Ok(Some(highlights));
                    }
                    if let Some(sig) = cst::LetBindingSignature::cast(node)
                        && let Some(sig_id) = stabilized.lookup_cst(&sig)
                        && let Some(group_id) = lowered.info.let_binding_group_for_signature(sig_id)
                        && let Some(highlights) = highlight_let(engine, current_file, group_id)?
                    {
                        return Ok(Some(highlights));
                    }
                }
            }
        }
    }

    let Some(locations) = crate::references::implementation(engine, files, uri.clone(), position)?
    else {
        return Ok(None);
    };

    let mut ranges: Vec<Range> = locations
        .into_iter()
        .filter(|location| location.uri == uri)
        .map(|location| location.range)
        .collect();
    ranges.extend(extra_ranges);

    ranges.sort_by_key(|r| (r.start.line, r.start.character, r.end.line, r.end.character));
    ranges.dedup();

    Ok(Some(ranges.into_iter().map(|range| DocumentHighlight { range, kind: None }).collect()))
}
