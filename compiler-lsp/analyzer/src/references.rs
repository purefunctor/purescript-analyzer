use async_lsp::lsp_types::*;
use building::{QueryEngine, prim};
use files::{FileId, Files};
use indexing::{ImportId, ImportItemId, ImportKind, TermItemId, TypeItemId};
use lowering::{BinderId, ExpressionId, ExpressionKind, TermVariableResolution, TypeId, TypeKind};
use resolving::ResolvedImport;
use rowan::ast::{AstNode, AstPtr};
use smol_str::ToSmolStr;
use syntax::cst;

use crate::{extract::AnnotationSyntaxRange, locate};

pub fn implementation(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    position: Position,
) -> Option<Vec<Location>> {
    let f_id = {
        let uri = uri.as_str();
        files.id(uri)?
    };

    let located = locate::locate(engine, f_id, position);

    match located {
        locate::Located::ModuleName(cst) => references_module_name(engine, files, f_id, cst),
        locate::Located::ImportItem(i_id) => references_import(engine, files, f_id, i_id),
        locate::Located::Binder(b_id) => references_binder(engine, files, f_id, b_id),
        locate::Located::Expression(e_id) => references_expression(engine, files, f_id, e_id),
        locate::Located::Type(t_id) => references_type(engine, files, f_id, t_id),
        locate::Located::TermOperator(o_id) => {
            let lowered = engine.lowered(f_id).ok()?;
            let (f_id, t_id) = lowered.intermediate.index_term_operator(o_id)?;
            references_file_term(engine, files, *f_id, *t_id)
        }
        locate::Located::TypeOperator(o_id) => {
            let lowered = engine.lowered(f_id).ok()?;
            let (f_id, t_id) = lowered.intermediate.index_type_operator(o_id)?;
            references_file_type(engine, files, *f_id, *t_id)
        }
        locate::Located::Nothing => None,
    }
}

fn references_module_name(
    engine: &QueryEngine,
    files: &Files,
    file_id: FileId,
    cst: AstPtr<cst::ModuleName>,
) -> Option<Vec<Location>> {
    let (parsed, _) = engine.parsed(file_id).ok()?;

    let root = parsed.syntax_node();
    let module_name = cst.try_to_node(&root)?;

    let module_name = module_name.syntax().text().to_smolstr();
    let module_id = engine.module_file(&module_name)?;

    let candidates = probe_imports_for(engine, files, file_id, module_id)?;

    let mut locations = vec![];
    for (candidate_id, import_id) in candidates {
        let path = files.path(candidate_id);
        let content = files.content(candidate_id);

        let uri = Url::parse(&path).ok()?;
        let uri = prim::handle_generated(uri, &content)?;

        let (parsed, _) = engine.parsed(candidate_id).ok()?;
        let root = parsed.syntax_node();

        let indexed = engine.indexed(candidate_id).ok()?;
        let import = indexed.source[import_id].try_to_node(&root)?;
        let import = import.syntax();

        let range = AnnotationSyntaxRange::from_node(import);
        let range = locate::text_range_to_range(&content, range.syntax?);

        locations.push(Location { uri, range });
    }

    Some(locations)
}

fn references_import(
    engine: &QueryEngine,
    files: &Files,
    f_id: FileId,
    i_id: ImportItemId,
) -> Option<Vec<Location>> {
    let (parsed, indexed) = {
        let (parsed, _) = engine.parsed(f_id).ok()?;
        let indexed = engine.indexed(f_id).ok()?;
        (parsed, indexed)
    };

    let root = parsed.syntax_node();
    let ptr = &indexed.source[i_id];
    let node = ptr.try_to_node(&root)?;

    let statement = node.syntax().ancestors().find_map(cst::ImportStatement::cast)?;
    let module_name = statement.module_name()?.syntax().to_smolstr();

    let import_resolved = {
        let import_id = engine.module_file(&module_name)?;
        engine.resolved(import_id).ok()?
    };

    let references_term = |engine: &QueryEngine, files: &Files, name: &str| {
        let name = name.trim_start_matches("(").trim_end_matches(")");
        let (f_id, t_id) = import_resolved.exports.lookup_term(name)?;
        references_file_term(engine, files, f_id, t_id)
    };

    let references_type = |engine: &QueryEngine, files: &Files, name: &str| {
        let name = name.trim_start_matches("(").trim_end_matches(")");
        let (f_id, t_id) = import_resolved.exports.lookup_type(name)?;
        references_file_type(engine, files, f_id, t_id)
    };

    match node {
        cst::ImportItem::ImportValue(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            references_term(engine, files, name)
        }
        cst::ImportItem::ImportClass(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            references_type(engine, files, name)
        }
        cst::ImportItem::ImportType(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            references_type(engine, files, name)
        }
        cst::ImportItem::ImportOperator(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            references_term(engine, files, name)
        }
        cst::ImportItem::ImportTypeOperator(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            references_type(engine, files, name)
        }
    }
}

fn references_binder(
    engine: &QueryEngine,
    files: &Files,
    f_id: FileId,
    b_id: BinderId,
) -> Option<Vec<Location>> {
    let lowered = engine.lowered(f_id).ok()?;
    let kind = lowered.intermediate.index_binder_kind(b_id)?;
    match kind {
        lowering::BinderKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            references_file_term(engine, files, *f_id, *t_id)
        }
        _ => None,
    }
}

fn references_expression(
    engine: &QueryEngine,
    files: &Files,
    f_id: FileId,
    e_id: ExpressionId,
) -> Option<Vec<Location>> {
    let lowered = engine.lowered(f_id).ok()?;
    let kind = lowered.intermediate.index_expression_kind(e_id)?;
    match kind {
        ExpressionKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            references_file_term(engine, files, *f_id, *t_id)
        }
        ExpressionKind::Variable { resolution, .. } => {
            let resolution = resolution.as_ref()?;
            match resolution {
                TermVariableResolution::Binder(_) => None,
                TermVariableResolution::Let(_) => None,
                TermVariableResolution::Reference(f_id, t_id) => {
                    references_file_term(engine, files, *f_id, *t_id)
                }
            }
        }
        ExpressionKind::OperatorName { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            references_file_term(engine, files, *f_id, *t_id)
        }
        _ => None,
    }
}

fn references_type(
    engine: &QueryEngine,
    files: &Files,
    f_id: FileId,
    t_id: TypeId,
) -> Option<Vec<Location>> {
    let lowered = engine.lowered(f_id).ok()?;
    let kind = lowered.intermediate.index_type_kind(t_id)?;
    match kind {
        TypeKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            references_file_type(engine, files, *f_id, *t_id)
        }
        TypeKind::Operator { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            references_file_type(engine, files, *f_id, *t_id)
        }
        _ => None,
    }
}

fn references_file_term(
    engine: &QueryEngine,
    files: &Files,
    file_id: FileId,
    term_id: TermItemId,
) -> Option<Vec<Location>> {
    let candidates = probe_term_references(engine, files, file_id, term_id)?;

    let mut locations = vec![];
    for candidate_id in candidates {
        let uri = {
            let path = files.path(candidate_id);
            let content = files.content(candidate_id);
            let uri = Url::parse(&path).ok()?;
            prim::handle_generated(uri, &content)?
        };

        let content = engine.content(candidate_id);
        let (parsed, _) = engine.parsed(candidate_id).ok()?;
        let lowered = engine.lowered(candidate_id).ok()?;

        for (expr_id, expr_kind) in lowered.intermediate.iter_expression() {
            if let ExpressionKind::Constructor { resolution: Some((f_id, t_id)) } = expr_kind
                && (*f_id, *t_id) == (file_id, term_id)
            {
                let root = parsed.syntax_node();
                let ptr = lowered.source[expr_id].syntax_node_ptr();
                let range = locate::syntax_range(&content, &root, &ptr)?;
                locations.push(Location { uri: uri.clone(), range });
            } else if let ExpressionKind::Variable { resolution: Some(resolution) } = expr_kind
                && let TermVariableResolution::Reference(f_id, t_id) = resolution
                && (*f_id, *t_id) == (file_id, term_id)
            {
                let root = parsed.syntax_node();
                let ptr = lowered.source[expr_id].syntax_node_ptr();
                let range = locate::syntax_range(&content, &root, &ptr)?;
                locations.push(Location { uri: uri.clone(), range });
            }
        }

        for (operator_id, f_id, t_id) in lowered.intermediate.iter_term_operator() {
            if (f_id, t_id) == (file_id, term_id) {
                let root = parsed.syntax_node();
                let ptr = lowered.source[operator_id].syntax_node_ptr();
                let range = locate::syntax_range(&content, &root, &ptr)?;
                locations.push(Location { uri: uri.clone(), range });
            }
        }
    }

    Some(locations)
}

fn references_file_type(
    engine: &QueryEngine,
    files: &Files,
    file_id: FileId,
    type_id: TypeItemId,
) -> Option<Vec<Location>> {
    let candidates = probe_type_references(engine, files, file_id, type_id)?;

    let mut locations = vec![];
    for candidate_id in candidates {
        let uri = {
            let path = files.path(candidate_id);
            let content = files.content(candidate_id);
            let uri = Url::parse(&path).ok()?;
            prim::handle_generated(uri, &content)?
        };

        let content = engine.content(candidate_id);
        let (parsed, _) = engine.parsed(candidate_id).ok()?;
        let lowered = engine.lowered(candidate_id).ok()?;

        for (ty_id, ty_kind) in lowered.intermediate.iter_type() {
            if let TypeKind::Constructor { resolution: Some((f_id, t_id)) } = ty_kind
                && (*f_id, *t_id) == (file_id, type_id)
            {
                let root = parsed.syntax_node();
                let ptr = lowered.source[ty_id].syntax_node_ptr();
                let range = locate::syntax_range(&content, &root, &ptr)?;
                locations.push(Location { uri: uri.clone(), range });
            }
        }

        for (operator_id, f_id, t_id) in lowered.intermediate.iter_type_operator() {
            if (f_id, t_id) == (file_id, type_id) {
                let root = parsed.syntax_node();
                let ptr = lowered.source[operator_id].syntax_node_ptr();
                let range = locate::syntax_range(&content, &root, &ptr)?;
                locations.push(Location { uri: uri.clone(), range });
            }
        }
    }

    Some(locations)
}

fn probe_term_references(
    engine: &QueryEngine,
    files: &Files,
    file_id: FileId,
    term_id: TermItemId,
) -> Option<Vec<FileId>> {
    probe_workspace_imports(engine, files, file_id, |import| {
        import.iter_terms().any(|(_, f_id, t_id, kind)| {
            kind != ImportKind::Hidden && (f_id, t_id) == (file_id, term_id)
        })
    })
}

fn probe_type_references(
    engine: &QueryEngine,
    files: &Files,
    file_id: FileId,
    type_id: TypeItemId,
) -> Option<Vec<FileId>> {
    probe_workspace_imports(engine, files, file_id, |import| {
        import.iter_types().any(|(_, f_id, t_id, kind)| {
            kind != ImportKind::Hidden && (f_id, t_id) == (file_id, type_id)
        })
    })
}

fn probe_workspace_imports(
    engine: &QueryEngine,
    files: &Files,
    file_id: FileId,
    check_import: impl Fn(&ResolvedImport) -> bool,
) -> Option<Vec<FileId>> {
    let mut probe = vec![file_id];

    for workspace_file_id in files.iter_id() {
        if workspace_file_id == file_id {
            continue;
        }

        let resolved = engine.resolved(workspace_file_id).ok()?;

        let unqualified = resolved.unqualified.values().flatten();
        let qualified = resolved.qualified.values();
        let imports = unqualified.chain(qualified);

        for import in imports {
            if check_import(import) {
                probe.push(workspace_file_id);
            }
        }
    }

    Some(probe)
}

fn probe_imports_for(
    engine: &QueryEngine,
    files: &Files,
    file_id: FileId,
    module_id: FileId,
) -> Option<Vec<(FileId, ImportId)>> {
    let mut probe = vec![];
    for workspace_file_id in files.iter_id() {
        if workspace_file_id == file_id {
            continue;
        }

        let resolved = engine.resolved(workspace_file_id).ok()?;

        let unqualified = resolved.unqualified.values().flatten();
        let qualified = resolved.qualified.values();
        let imports = unqualified.chain(qualified);

        for import in imports {
            if import.file == module_id {
                probe.push((workspace_file_id, import.id));
            }
        }
    }
    Some(probe)
}
