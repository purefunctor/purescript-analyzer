use std::ops;

use async_lsp::lsp_types::*;
use building::{QueryEngine, prim};
use files::{FileId, Files};
use indexing::{ImportId, ImportItemId, ImportKind, TermItemId, TypeItemId};
use la_arena::Idx;
use lowering::{
    BinderId, BinderKind, ExpressionId, ExpressionKind, FullLoweredModule, LoweringSource,
    TermVariableResolution, TypeId, TypeKind,
};
use parsing::ParsedModule;
use resolving::ResolvedImport;
use rowan::ast::{AstNode, AstPtr};
use rustc_hash::FxHashSet;
use smol_str::ToSmolStr;
use syntax::{PureScript, cst};

use crate::locate;

pub fn implementation(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    position: Position,
) -> Option<Vec<Location>> {
    let current_file = {
        let uri = uri.as_str();
        files.id(uri)?
    };

    let located = locate::locate(engine, current_file, position);

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
            let lowered = engine.lowered(current_file).ok()?;
            let (f_id, t_id) = lowered.intermediate.index_term_operator(operator_id)?;
            references_file_term(engine, files, current_file, *f_id, *t_id)
        }
        locate::Located::TypeOperator(operator_id) => {
            let lowered = engine.lowered(current_file).ok()?;
            let (f_id, t_id) = lowered.intermediate.index_type_operator(operator_id)?;
            references_file_type(engine, files, current_file, *f_id, *t_id)
        }
        locate::Located::Nothing => None,
    }
}

fn references_module_name(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    module_name: AstPtr<cst::ModuleName>,
) -> Option<Vec<Location>> {
    let (parsed, _) = engine.parsed(current_file).ok()?;

    let root = parsed.syntax_node();
    let module_name = module_name.try_to_node(&root)?;

    let module_name = module_name.syntax().text().to_smolstr();
    let module_id = engine.module_file(&module_name)?;

    let candidates = probe_imports_for(engine, files, module_id)?;

    let mut locations = vec![];
    for (candidate_id, import_id) in candidates {
        let path = files.path(candidate_id);
        let content = files.content(candidate_id);

        let uri = Url::parse(&path).ok()?;
        let uri = prim::handle_generated(uri, &content)?;

        let (parsed, _) = engine.parsed(candidate_id).ok()?;
        let root = parsed.syntax_node();

        let indexed = engine.indexed(candidate_id).ok()?;
        let ptr = indexed.source[import_id].syntax_node_ptr();
        let range = locate::syntax_range(&content, &root, &ptr)?;

        locations.push(Location { uri, range });
    }

    Some(locations)
}

fn references_import(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    import_id: ImportItemId,
) -> Option<Vec<Location>> {
    let (parsed, indexed) = {
        let (parsed, _) = engine.parsed(current_file).ok()?;
        let indexed = engine.indexed(current_file).ok()?;
        (parsed, indexed)
    };

    let root = parsed.syntax_node();
    let ptr = &indexed.source[import_id];
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
        references_file_term(engine, files, current_file, f_id, t_id)
    };

    let references_type = |engine: &QueryEngine, files: &Files, name: &str| {
        let name = name.trim_start_matches("(").trim_end_matches(")");
        let (f_id, t_id) = import_resolved.exports.lookup_type(name)?;
        references_file_type(engine, files, current_file, f_id, t_id)
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
    current_file: FileId,
    binder_id: BinderId,
) -> Option<Vec<Location>> {
    let lowered = engine.lowered(current_file).ok()?;
    let kind = lowered.intermediate.index_binder_kind(binder_id)?;
    match kind {
        lowering::BinderKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            references_file_term(engine, files, current_file, *f_id, *t_id)
        }
        _ => None,
    }
}

fn references_expression(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    expression_id: ExpressionId,
) -> Option<Vec<Location>> {
    let lowered = engine.lowered(current_file).ok()?;
    let kind = lowered.intermediate.index_expression_kind(expression_id)?;
    match kind {
        ExpressionKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            references_file_term(engine, files, current_file, *f_id, *t_id)
        }
        ExpressionKind::Variable { resolution, .. } => {
            let resolution = resolution.as_ref()?;
            match resolution {
                TermVariableResolution::Binder(_) => None,
                TermVariableResolution::Let(_) => None,
                TermVariableResolution::Reference(f_id, t_id) => {
                    references_file_term(engine, files, current_file, *f_id, *t_id)
                }
            }
        }
        ExpressionKind::OperatorName { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            references_file_term(engine, files, current_file, *f_id, *t_id)
        }
        _ => None,
    }
}

fn references_type(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    type_id: TypeId,
) -> Option<Vec<Location>> {
    let lowered = engine.lowered(current_file).ok()?;
    let kind = lowered.intermediate.index_type_kind(type_id)?;
    match kind {
        TypeKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            references_file_type(engine, files, current_file, *f_id, *t_id)
        }
        TypeKind::Operator { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            references_file_type(engine, files, current_file, *f_id, *t_id)
        }
        _ => None,
    }
}

fn id_range<T>(
    content: &str,
    parsed: &ParsedModule,
    lowered: &FullLoweredModule,
    item_id: Idx<AstPtr<T>>,
) -> Option<Range>
where
    T: AstNode<Language = PureScript>,
    LoweringSource: ops::Index<Idx<AstPtr<T>>, Output = AstPtr<T>>,
{
    let root = parsed.syntax_node();
    let ptr = lowered.source[item_id].syntax_node_ptr();
    locate::syntax_range(content, &root, &ptr)
}

fn references_file_term(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    file_id: FileId,
    term_id: TermItemId,
) -> Option<Vec<Location>> {
    let candidates = probe_term_references(engine, files, current_file, file_id, term_id)?;

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
                let range = id_range(&content, &parsed, &lowered, expr_id)?;
                locations.push(Location { uri: uri.clone(), range });
            } else if let ExpressionKind::OperatorName { resolution: Some((f_id, t_id)) } =
                expr_kind
                && (*f_id, *t_id) == (file_id, term_id)
            {
                let range = id_range(&content, &parsed, &lowered, expr_id)?;
                locations.push(Location { uri: uri.clone(), range });
            } else if let ExpressionKind::Variable { resolution: Some(resolution) } = expr_kind
                && let TermVariableResolution::Reference(f_id, t_id) = resolution
                && (*f_id, *t_id) == (file_id, term_id)
            {
                let range = id_range(&content, &parsed, &lowered, expr_id)?;
                locations.push(Location { uri: uri.clone(), range });
            }
        }

        for (binder_id, binder_kind) in lowered.intermediate.iter_binder() {
            if let BinderKind::Constructor { resolution: Some((f_id, t_id)), .. } = binder_kind
                && (*f_id, *t_id) == (file_id, term_id)
            {
                let range = id_range(&content, &parsed, &lowered, binder_id)?;
                locations.push(Location { uri: uri.clone(), range });
            }
        }

        for (operator_id, f_id, t_id) in lowered.intermediate.iter_term_operator() {
            if (f_id, t_id) == (file_id, term_id) {
                let range = id_range(&content, &parsed, &lowered, operator_id)?;
                locations.push(Location { uri: uri.clone(), range });
            }
        }
    }

    Some(locations)
}

fn references_file_type(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    file_id: FileId,
    type_id: TypeItemId,
) -> Option<Vec<Location>> {
    let candidates = probe_type_references(engine, files, current_file, file_id, type_id)?;

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
                let range = id_range(&content, &parsed, &lowered, ty_id)?;
                locations.push(Location { uri: uri.clone(), range });
            }
            if let TypeKind::Operator { resolution: Some((f_id, t_id)) } = ty_kind
                && (*f_id, *t_id) == (file_id, type_id)
            {
                let range = id_range(&content, &parsed, &lowered, ty_id)?;
                locations.push(Location { uri: uri.clone(), range });
            }
        }

        for (operator_id, f_id, t_id) in lowered.intermediate.iter_type_operator() {
            if (f_id, t_id) == (file_id, type_id) {
                let range = id_range(&content, &parsed, &lowered, operator_id)?;
                locations.push(Location { uri: uri.clone(), range });
            }
        }
    }

    Some(locations)
}

fn probe_term_references(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    file_id: FileId,
    term_id: TermItemId,
) -> Option<FxHashSet<FileId>> {
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
) -> Option<FxHashSet<FileId>> {
    probe_workspace_imports(engine, files, current_file, file_id, |import| {
        import.iter_types().any(|(_, f_id, t_id, kind)| {
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
) -> Option<FxHashSet<FileId>> {
    let mut probe = FxHashSet::from_iter([current_file, source_file]);

    for workspace_file_id in files.iter_id() {
        if workspace_file_id == current_file || workspace_file_id == source_file {
            continue;
        }

        let resolved = engine.resolved(workspace_file_id).ok()?;

        let unqualified = resolved.unqualified.values().flatten();
        let qualified = resolved.qualified.values();
        let imports = unqualified.chain(qualified);

        for import in imports {
            if check_import(import) {
                probe.insert(workspace_file_id);
            }
        }
    }

    Some(probe)
}

fn probe_imports_for(
    engine: &QueryEngine,
    files: &Files,
    module_id: FileId,
) -> Option<FxHashSet<(FileId, ImportId)>> {
    let mut probe = FxHashSet::default();

    for workspace_file_id in files.iter_id() {
        let resolved = engine.resolved(workspace_file_id).ok()?;

        let unqualified = resolved.unqualified.values().flatten();
        let qualified = resolved.qualified.values();
        let imports = unqualified.chain(qualified);

        for import in imports {
            if import.file == module_id {
                probe.insert((workspace_file_id, import.id));
            }
        }
    }

    Some(probe)
}
