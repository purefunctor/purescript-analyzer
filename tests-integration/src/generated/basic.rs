use std::fmt::Write;

use analyzer::{QueryEngine, locate};
use checking::core::pretty;
use files::FileId;
use indexing::{ImportKind, TermItem, TypeItem};
use lowering::{
    ExpressionKind, GraphNode, ImplicitTypeVariable, TermVariableResolution, TypeKind,
    TypeVariableResolution,
};
use rowan::ast::AstNode;
use syntax::cst;

pub fn report_resolved(engine: &QueryEngine, id: FileId, name: &str) -> String {
    let resolved = engine.resolved(id).unwrap();

    let mut buffer = String::default();
    writeln!(buffer, "module {name}").unwrap();

    writeln!(buffer).unwrap();
    writeln!(buffer, "Unqualified Imports:").unwrap();
    for import in resolved.unqualified.values().flatten() {
        writeln!(buffer).unwrap();
        writeln!(buffer, "Terms:").unwrap();
        for (name, _, _, kind) in import.iter_terms() {
            if matches!(kind, ImportKind::Hidden) {
                continue;
            }
            writeln!(buffer, "  - {name} is {kind:?}").unwrap();
        }

        writeln!(buffer).unwrap();
        writeln!(buffer, "Types:").unwrap();
        for (name, _, _, kind) in import.iter_types() {
            if matches!(kind, ImportKind::Hidden) {
                continue;
            }
            writeln!(buffer, "  - {name} is {kind:?}").unwrap();
        }
    }

    writeln!(buffer).unwrap();
    writeln!(buffer, "Qualified Imports:").unwrap();
    for (name, import) in &resolved.qualified {
        writeln!(buffer).unwrap();
        writeln!(buffer, "{name} Terms:").unwrap();
        for (name, _, _, kind) in import.iter_terms() {
            if matches!(kind, ImportKind::Hidden) {
                continue;
            }
            writeln!(buffer, "  - {name} is {kind:?}").unwrap();
        }

        writeln!(buffer).unwrap();
        writeln!(buffer, "{name} Types:").unwrap();
        for (name, _, _, kind) in import.iter_types() {
            if matches!(kind, ImportKind::Hidden) {
                continue;
            }
            writeln!(buffer, "  - {name} is {kind:?}").unwrap();
        }
    }

    writeln!(buffer).unwrap();
    writeln!(buffer, "Exported Terms:").unwrap();
    for (name, _, _) in resolved.exports.iter_terms() {
        writeln!(buffer, "  - {name}").unwrap();
    }

    writeln!(buffer).unwrap();
    writeln!(buffer, "Exported Types:").unwrap();
    for (name, _, _) in resolved.exports.iter_types() {
        writeln!(buffer, "  - {name}").unwrap();
    }

    writeln!(buffer).unwrap();
    writeln!(buffer, "Local Terms:").unwrap();
    for (name, _, _) in resolved.locals.iter_terms() {
        writeln!(buffer, "  - {name}").unwrap();
    }

    writeln!(buffer).unwrap();
    writeln!(buffer, "Local Types:").unwrap();
    for (name, _, _) in resolved.locals.iter_types() {
        writeln!(buffer, "  - {name}").unwrap();
    }

    writeln!(buffer).unwrap();
    writeln!(buffer, "Errors:").unwrap();
    for error in &resolved.errors {
        writeln!(buffer, "  - {error:?}").unwrap();
    }

    buffer
}

pub fn report_lowered(engine: &QueryEngine, id: FileId, name: &str) -> String {
    let content = engine.content(id);
    let (parsed, _) = engine.parsed(id).unwrap();

    let stabilized = engine.stabilized(id).unwrap();
    let lowered = engine.lowered(id).unwrap();

    let module = parsed.cst();
    let info = &lowered.info;
    let graph = &lowered.graph;

    let mut buffer = String::default();
    writeln!(buffer, "module {name}").unwrap();

    writeln!(buffer).unwrap();
    writeln!(buffer, "Expressions:").unwrap();
    writeln!(buffer).unwrap();
    for (expression_id, _) in info.iter_expression() {
        let Some(kind) = info.get_expression_kind(expression_id) else {
            continue;
        };
        if let ExpressionKind::Variable { resolution, .. } = kind {
            report_on_term(
                &content,
                &stabilized,
                &module,
                info,
                &mut buffer,
                expression_id,
                resolution,
            );
        } else if let ExpressionKind::Record { record } = kind {
            for field in record.iter() {
                if let lowering::ExpressionRecordItem::RecordPun { resolution, .. } = field {
                    report_on_term(
                        &content,
                        &stabilized,
                        &module,
                        info,
                        &mut buffer,
                        expression_id,
                        resolution,
                    );
                }
            }
        } else {
            continue;
        }
    }

    writeln!(buffer, "\nTypes:\n").unwrap();

    for (type_id, _) in info.iter_type() {
        let Some(TypeKind::Variable { resolution, .. }) = info.get_type_kind(type_id) else {
            continue;
        };

        let cst = stabilized.ast_ptr(type_id).unwrap();
        let root = module.syntax();

        let node = cst.syntax_node_ptr().to_node(root);
        let text = node.text().to_string();

        let range = cst.syntax_node_ptr().text_range();
        let position = locate::offset_to_position(&content, range.start());

        writeln!(buffer, "{}@{:?}", text.trim(), position).unwrap();
        if let Some(resolution) = resolution {
            match resolution {
                TypeVariableResolution::Forall(id) => {
                    let cst = stabilized.ast_ptr(*id).unwrap();
                    let range = cst.syntax_node_ptr().text_range();
                    let position = locate::offset_to_position(&content, range.start());
                    writeln!(buffer, "  resolves to forall {position:?}").unwrap();
                }
                TypeVariableResolution::Implicit(ImplicitTypeVariable { binding, node, id }) => {
                    if let GraphNode::Implicit { bindings, .. } = &graph[*node] {
                        let (name, type_ids) =
                            bindings.get_index(*id).expect("invariant violated: invalid index");
                        if *binding {
                            writeln!(buffer, "  introduces a constraint variable {name:?}")
                                .unwrap();
                        } else {
                            writeln!(buffer, "  resolves to a constraint variable {name:?}")
                                .unwrap();
                            for &type_id in type_ids {
                                let cst = stabilized.ast_ptr(type_id).unwrap();
                                let range = cst.syntax_node_ptr().text_range();
                                let position = locate::offset_to_position(&content, range.start());
                                writeln!(buffer, "    {position:?}").unwrap();
                            }
                        }
                    } else {
                        writeln!(buffer, "  did not resolve to constraint variable!").unwrap();
                    }
                }
            }
        } else {
            writeln!(buffer, "  resolves to nothing").unwrap();
        }
    }

    buffer
}

fn report_on_term(
    content: &str,
    stabilized: &stabilizing::StabilizedModule,
    module: &cst::Module,
    info: &lowering::LoweringInfo,
    buffer: &mut String,
    expression_id: lowering::ExpressionId,
    resolution: &Option<TermVariableResolution>,
) {
    let cst = stabilized.ast_ptr(expression_id).unwrap();
    let root = module.syntax();

    let node = cst.syntax_node_ptr().to_node(root);
    let text = node.text().to_string();

    let range = node.text_range();
    let position = locate::offset_to_position(content, range.start());

    writeln!(buffer, "{}@{:?}", text.trim(), position).unwrap();
    if let Some(resolution) = resolution {
        match resolution {
            TermVariableResolution::Binder(binder) => {
                let cst = stabilized.ast_ptr(*binder).unwrap();
                let range = cst.syntax_node_ptr().text_range();
                let position = locate::offset_to_position(content, range.start());
                writeln!(buffer, "  resolves to binder {position:?}").unwrap();
            }
            TermVariableResolution::Let(let_binding_id) => {
                let let_binding = info.get_let_binding_group(*let_binding_id);
                if let Some(signature) = let_binding.signature {
                    let cst = stabilized.ast_ptr(signature).unwrap();
                    let range = cst.syntax_node_ptr().text_range();
                    let position = locate::offset_to_position(content, range.start());
                    writeln!(buffer, "  resolves to signature {position:?}").unwrap();
                }
                for equation in let_binding.equations.iter() {
                    let cst = stabilized.ast_ptr(*equation).unwrap();
                    let range = cst.syntax_node_ptr().text_range();
                    let position = locate::offset_to_position(content, range.start());
                    writeln!(buffer, "  resolves to equation {position:?}").unwrap();
                }
            }
            TermVariableResolution::RecordPun(record_pun) => {
                let cst = stabilized.ast_ptr(*record_pun).unwrap();
                let range = cst.syntax_node_ptr().text_range();
                let position = locate::offset_to_position(content, range.start());
                writeln!(buffer, "  resolves to record pun {position:?}").unwrap();
            }
            TermVariableResolution::Reference(_, _) => {
                writeln!(buffer, "  resolves to top-level name").unwrap();
            }
        }
    } else {
        writeln!(buffer, "  resolves to nothing").unwrap();
    }
}

pub fn report_checked(engine: &QueryEngine, id: FileId) -> String {
    let indexed = engine.indexed(id).unwrap();
    let checked = engine.checked(id).unwrap();

    let mut snapshot = String::default();

    writeln!(snapshot, "Terms").unwrap();
    for (id, TermItem { name, .. }) in indexed.items.iter_terms() {
        let Some(n) = name else { continue };
        let Some(t) = checked.lookup_term(id) else { continue };
        let t = pretty::print_global(engine, t);
        writeln!(snapshot, "{n} :: {t}").unwrap();
    }

    writeln!(snapshot, "\nTypes").unwrap();
    for (id, TypeItem { name, .. }) in indexed.items.iter_types() {
        let Some(n) = name else { continue };
        let Some(t) = checked.lookup_type(id) else { continue };
        let t = pretty::print_global(engine, t);
        writeln!(snapshot, "{n} :: {t}").unwrap();
    }

    if !checked.synonyms.is_empty() {
        writeln!(snapshot, "\nSynonyms").unwrap();
    }
    for (id, TypeItem { name, .. }) in indexed.items.iter_types() {
        let Some(name) = name else { continue };
        let Some(group) = checked.lookup_synonym(id) else { continue };
        let synonym = pretty::print_global(engine, group.synonym_type);
        writeln!(snapshot, "{name} = {synonym}").unwrap();
        writeln!(snapshot, "  Quantified = {}", group.quantified_variables).unwrap();
        writeln!(snapshot, "  Kind = {}", group.kind_variables).unwrap();
        writeln!(snapshot, "  Type = {}", group.type_variables).unwrap();
        writeln!(snapshot).unwrap();
    }

    if !checked.errors.is_empty() {
        writeln!(snapshot, "\nErrors").unwrap();
    }
    for error in &checked.errors {
        writeln!(snapshot, "{:?} at {:?}", error.kind, &error.step).unwrap();
    }

    snapshot
}
