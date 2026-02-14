use async_lsp::lsp_types::*;
use building::QueryEngine;
use files::{FileId, Files};
use indexing::{ImportId, ImportItemId, ImportKind, TermItemId, TypeItemId};
use lowering::{
    BinderId, BinderKind, ExpressionId, ExpressionKind, TermVariableResolution, TypeId, TypeKind,
};
use parsing::ParsedModule;
use resolving::ResolvedImport;
use rowan::ast::{AstNode, AstPtr};
use rustc_hash::FxHashSet;
use smol_str::ToSmolStr;
use stabilizing::{AstId, StabilizedModule};
use syntax::{PureScript, cst};

use crate::{AnalyzerError, common, locate};

pub fn implementation(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    position: Position,
) -> Result<Option<Vec<Location>>, AnalyzerError> {
    let current_file = {
        let uri = uri.as_str();
        files.id(uri).ok_or(AnalyzerError::NonFatal)?
    };

    let located = locate::locate(engine, current_file, position)?;

    match located {
        locate::Located::ModuleName(module_name) => {
            references_module_name(engine, files, current_file, module_name)
        }
        locate::Located::ImportItem(import_id) => {
            references_import(engine, files, current_file, import_id)
        }
        locate::Located::Binder(binder_id) => {
            references_binder(engine, files, current_file, binder_id)
        }
        locate::Located::Expression(expression_id) => {
            references_expression(engine, files, current_file, expression_id)
        }
        locate::Located::Type(type_id) => references_type(engine, files, current_file, type_id),
        locate::Located::TermOperator(operator_id) => {
            let lowered = engine.lowered(current_file)?;
            let (f_id, t_id) =
                lowered.info.get_term_operator(operator_id).ok_or(AnalyzerError::NonFatal)?;
            references_file_term(engine, files, current_file, f_id, t_id)
        }
        locate::Located::TypeOperator(operator_id) => {
            let lowered = engine.lowered(current_file)?;
            let (f_id, t_id) =
                lowered.info.get_type_operator(operator_id).ok_or(AnalyzerError::NonFatal)?;
            references_file_type(engine, files, current_file, f_id, t_id)
        }
        locate::Located::TermItem(term_id) => {
            references_file_term(engine, files, current_file, current_file, term_id)
        }
        locate::Located::TypeItem(type_id) => {
            references_file_type(engine, files, current_file, current_file, type_id)
        }
        locate::Located::Nothing => Ok(None),
    }
}

fn references_module_name(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    module_name: AstPtr<cst::ModuleName>,
) -> Result<Option<Vec<Location>>, AnalyzerError> {
    let (parsed, _) = engine.parsed(current_file)?;

    let root = parsed.syntax_node();
    let module_name = module_name.try_to_node(&root).ok_or(AnalyzerError::NonFatal)?;

    let module_name = module_name.syntax().text().to_smolstr();
    let module_id = engine.module_file(&module_name).ok_or(AnalyzerError::NonFatal)?;

    let candidates = probe_imports_for(engine, files, module_id)?;

    let mut locations = vec![];
    for (candidate_id, import_id) in candidates {
        let uri = common::file_uri(engine, files, candidate_id)?;

        let content = engine.content(candidate_id);
        let (parsed, _) = engine.parsed(candidate_id)?;
        let root = parsed.syntax_node();

        let stabilized = engine.stabilized(candidate_id)?;
        let ptr = stabilized.syntax_ptr(import_id).ok_or(AnalyzerError::NonFatal)?;
        let range = locate::syntax_range(&content, &root, &ptr).ok_or(AnalyzerError::NonFatal)?;

        locations.push(Location { uri, range });
    }

    Ok(Some(locations))
}

fn references_import(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    import_id: ImportItemId,
) -> Result<Option<Vec<Location>>, AnalyzerError> {
    let (parsed, _) = engine.parsed(current_file)?;
    let stabilized = engine.stabilized(current_file)?;

    let root = parsed.syntax_node();
    let ptr = stabilized.ast_ptr(import_id).ok_or(AnalyzerError::NonFatal)?;
    let node = ptr.try_to_node(&root).ok_or(AnalyzerError::NonFatal)?;

    let statement = node
        .syntax()
        .ancestors()
        .find_map(cst::ImportStatement::cast)
        .ok_or(AnalyzerError::NonFatal)?;
    let module_name = statement.module_name().ok_or(AnalyzerError::NonFatal)?.syntax().to_smolstr();

    let import_resolved = {
        let import_id = engine.module_file(&module_name).ok_or(AnalyzerError::NonFatal)?;
        engine.resolved(import_id)?
    };

    let references_term = |engine: &QueryEngine, files: &Files, name: &str| {
        let name = name.trim_start_matches("(").trim_end_matches(")");
        let (f_id, t_id) =
            import_resolved.exports.lookup_term(name).ok_or(AnalyzerError::NonFatal)?;
        references_file_term(engine, files, current_file, f_id, t_id)
    };

    let references_type = |engine: &QueryEngine, files: &Files, name: &str| {
        let name = name.trim_start_matches("(").trim_end_matches(")");
        let (f_id, t_id) = import_resolved
            .exports
            .lookup_type(name)
            .or_else(|| import_resolved.exports.lookup_class(name))
            .ok_or(AnalyzerError::NonFatal)?;
        references_file_type(engine, files, current_file, f_id, t_id)
    };

    let references_class = |engine: &QueryEngine, files: &Files, name: &str| {
        let name = name.trim_start_matches("(").trim_end_matches(")");
        let (f_id, t_id) = import_resolved
            .exports
            .lookup_class(name)
            .or_else(|| import_resolved.exports.lookup_type(name))
            .ok_or(AnalyzerError::NonFatal)?;
        references_file_type(engine, files, current_file, f_id, t_id)
    };

    match node {
        cst::ImportItem::ImportValue(cst) => {
            let token = cst.name_token().ok_or(AnalyzerError::NonFatal)?;
            let name = token.text();
            references_term(engine, files, name)
        }
        cst::ImportItem::ImportClass(cst) => {
            let token = cst.name_token().ok_or(AnalyzerError::NonFatal)?;
            let name = token.text();
            references_class(engine, files, name)
        }
        cst::ImportItem::ImportType(cst) => {
            let token = cst.name_token().ok_or(AnalyzerError::NonFatal)?;
            let name = token.text();
            references_type(engine, files, name)
        }
        cst::ImportItem::ImportOperator(cst) => {
            let token = cst.name_token().ok_or(AnalyzerError::NonFatal)?;
            let name = token.text();
            references_term(engine, files, name)
        }
        cst::ImportItem::ImportTypeOperator(cst) => {
            let token = cst.name_token().ok_or(AnalyzerError::NonFatal)?;
            let name = token.text();
            references_type(engine, files, name)
        }
    }
}

fn references_binder(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    binder_id: BinderId,
) -> Result<Option<Vec<Location>>, AnalyzerError> {
    let lowered = engine.lowered(current_file)?;
    let kind = lowered.info.get_binder_kind(binder_id).ok_or(AnalyzerError::NonFatal)?;
    match kind {
        lowering::BinderKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            references_file_term(engine, files, current_file, *f_id, *t_id)
        }
        _ => Ok(None),
    }
}

fn references_expression(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    expression_id: ExpressionId,
) -> Result<Option<Vec<Location>>, AnalyzerError> {
    let lowered = engine.lowered(current_file)?;
    let kind = lowered.info.get_expression_kind(expression_id).ok_or(AnalyzerError::NonFatal)?;
    match kind {
        ExpressionKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            references_file_term(engine, files, current_file, *f_id, *t_id)
        }
        ExpressionKind::Variable { resolution, .. } => {
            let resolution = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            match resolution {
                TermVariableResolution::Binder(_) => Ok(None),
                TermVariableResolution::Let(_) => Ok(None),
                TermVariableResolution::RecordPun(_) => Ok(None),
                TermVariableResolution::Reference(f_id, t_id) => {
                    references_file_term(engine, files, current_file, *f_id, *t_id)
                }
            }
        }
        ExpressionKind::OperatorName { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            references_file_term(engine, files, current_file, *f_id, *t_id)
        }
        _ => Ok(None),
    }
}

fn references_type(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    type_id: TypeId,
) -> Result<Option<Vec<Location>>, AnalyzerError> {
    let lowered = engine.lowered(current_file)?;
    let kind = lowered.info.get_type_kind(type_id).ok_or(AnalyzerError::NonFatal)?;
    match kind {
        TypeKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            references_file_type(engine, files, current_file, *f_id, *t_id)
        }
        TypeKind::Operator { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            references_file_type(engine, files, current_file, *f_id, *t_id)
        }
        _ => Ok(None),
    }
}

fn id_range<T>(
    content: &str,
    parsed: &ParsedModule,
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

fn references_file_term(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    file_id: FileId,
    term_id: TermItemId,
) -> Result<Option<Vec<Location>>, AnalyzerError> {
    let candidates = probe_term_references(engine, files, current_file, file_id, term_id)?;

    let mut locations = vec![];
    for candidate_id in candidates {
        let uri = common::file_uri(engine, files, candidate_id)?;

        let content = engine.content(candidate_id);
        let (parsed, _) = engine.parsed(candidate_id)?;

        let stabilized = engine.stabilized(candidate_id)?;
        let lowered = engine.lowered(candidate_id)?;

        for (expr_id, expr_kind) in lowered.info.iter_expression() {
            if let ExpressionKind::Constructor { resolution: Some((f_id, t_id)) } = expr_kind
                && (*f_id, *t_id) == (file_id, term_id)
            {
                let range = id_range(&content, &parsed, &stabilized, expr_id)
                    .ok_or(AnalyzerError::NonFatal)?;
                locations.push(Location { uri: uri.clone(), range });
            } else if let ExpressionKind::OperatorName { resolution: Some((f_id, t_id)) } =
                expr_kind
                && (*f_id, *t_id) == (file_id, term_id)
            {
                let range = id_range(&content, &parsed, &stabilized, expr_id)
                    .ok_or(AnalyzerError::NonFatal)?;
                locations.push(Location { uri: uri.clone(), range });
            } else if let ExpressionKind::Variable { resolution: Some(resolution) } = expr_kind
                && let TermVariableResolution::Reference(f_id, t_id) = resolution
                && (*f_id, *t_id) == (file_id, term_id)
            {
                let range = id_range(&content, &parsed, &stabilized, expr_id)
                    .ok_or(AnalyzerError::NonFatal)?;
                locations.push(Location { uri: uri.clone(), range });
            }
        }

        for (binder_id, binder_kind) in lowered.info.iter_binder() {
            if let BinderKind::Constructor { resolution: Some((f_id, t_id)), .. } = binder_kind
                && (*f_id, *t_id) == (file_id, term_id)
            {
                let range = id_range(&content, &parsed, &stabilized, binder_id)
                    .ok_or(AnalyzerError::NonFatal)?;
                locations.push(Location { uri: uri.clone(), range });
            }
        }

        for (operator_id, f_id, t_id) in lowered.info.iter_term_operator() {
            if (f_id, t_id) == (file_id, term_id) {
                let range = id_range(&content, &parsed, &stabilized, operator_id)
                    .ok_or(AnalyzerError::NonFatal)?;
                locations.push(Location { uri: uri.clone(), range });
            }
        }
    }

    Ok(Some(locations))
}

fn references_file_type(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    file_id: FileId,
    type_id: TypeItemId,
) -> Result<Option<Vec<Location>>, AnalyzerError> {
    let candidates = probe_type_references(engine, files, current_file, file_id, type_id)?;

    let mut locations = vec![];
    for candidate_id in candidates {
        let uri = common::file_uri(engine, files, candidate_id)?;

        let content = engine.content(candidate_id);
        let (parsed, _) = engine.parsed(candidate_id)?;

        let stabilized = engine.stabilized(candidate_id)?;
        let lowered = engine.lowered(candidate_id)?;

        for (ty_id, ty_kind) in lowered.info.iter_type() {
            if let TypeKind::Constructor { resolution: Some((f_id, t_id)) } = ty_kind
                && (*f_id, *t_id) == (file_id, type_id)
            {
                let range = id_range(&content, &parsed, &stabilized, ty_id)
                    .ok_or(AnalyzerError::NonFatal)?;
                locations.push(Location { uri: uri.clone(), range });
            }
            if let TypeKind::Operator { resolution: Some((f_id, t_id)) } = ty_kind
                && (*f_id, *t_id) == (file_id, type_id)
            {
                let range = id_range(&content, &parsed, &stabilized, ty_id)
                    .ok_or(AnalyzerError::NonFatal)?;
                locations.push(Location { uri: uri.clone(), range });
            }
        }

        for (operator_id, f_id, t_id) in lowered.info.iter_type_operator() {
            if (f_id, t_id) == (file_id, type_id) {
                let range = id_range(&content, &parsed, &stabilized, operator_id)
                    .ok_or(AnalyzerError::NonFatal)?;
                locations.push(Location { uri: uri.clone(), range });
            }
        }
    }

    Ok(Some(locations))
}

fn probe_term_references(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    file_id: FileId,
    term_id: TermItemId,
) -> Result<FxHashSet<FileId>, AnalyzerError> {
    probe_workspace_imports(engine, files, current_file, file_id, |import| {
        import.iter_terms().any(|(_, f_id, t_id, kind)| {
            kind != ImportKind::Hidden && (f_id, t_id) == (file_id, term_id)
        })
    })
}

fn probe_type_references(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    file_id: FileId,
    type_id: TypeItemId,
) -> Result<FxHashSet<FileId>, AnalyzerError> {
    probe_workspace_imports(engine, files, current_file, file_id, |import| {
        import.iter_types().any(|(_, f_id, t_id, kind)| {
            kind != ImportKind::Hidden && (f_id, t_id) == (file_id, type_id)
        }) || import.iter_classes().any(|(_, f_id, t_id, kind)| {
            kind != ImportKind::Hidden && (f_id, t_id) == (file_id, type_id)
        })
    })
}

fn probe_workspace_imports(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    source_file: FileId,
    check_import: impl Fn(&ResolvedImport) -> bool,
) -> Result<FxHashSet<FileId>, AnalyzerError> {
    let mut probe = FxHashSet::from_iter([current_file, source_file]);

    for workspace_file_id in files.iter_id() {
        if workspace_file_id == current_file || workspace_file_id == source_file {
            continue;
        }

        let resolved = engine.resolved(workspace_file_id)?;

        let unqualified = resolved.unqualified.values().flatten();
        let qualified = resolved.qualified.values().flatten();
        let imports = unqualified.chain(qualified);

        for import in imports {
            if check_import(import) {
                probe.insert(workspace_file_id);
            }
        }
    }

    Ok(probe)
}

fn probe_imports_for(
    engine: &QueryEngine,
    files: &Files,
    module_id: FileId,
) -> Result<FxHashSet<(FileId, ImportId)>, AnalyzerError> {
    let mut probe = FxHashSet::default();

    for workspace_file_id in files.iter_id() {
        let resolved = engine.resolved(workspace_file_id)?;

        let unqualified = resolved.unqualified.values().flatten();
        let qualified = resolved.qualified.values().flatten();
        let imports = unqualified.chain(qualified);

        for import in imports {
            if import.file == module_id {
                probe.insert((workspace_file_id, import.id));
            }
        }
    }

    Ok(probe)
}
