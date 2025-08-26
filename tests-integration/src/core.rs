use std::fmt::Write;

use analyzer::QueryEngine;
use files::FileId;
use indexing::ImportKind;
use lowering::{
    ExpressionKind, GraphNode, ImplicitTypeVariable, LetBound, TermVariableResolution, TypeKind,
    TypeVariableResolution,
};
use rowan::ast::AstNode;

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
    let (parsed, _) = engine.parsed(id).unwrap();
    let lowered = engine.lowered(id).unwrap();

    let module = parsed.cst();
    let intermediate = &lowered.intermediate;
    let source = &lowered.source;
    let graph = &lowered.graph;

    let mut buffer = String::default();
    writeln!(buffer, "module {name}").unwrap();

    writeln!(buffer).unwrap();
    writeln!(buffer, "Expressions:").unwrap();
    writeln!(buffer).unwrap();
    for (expression_id, _) in intermediate.iter_expression() {
        let Some(ExpressionKind::Variable { resolution, .. }) =
            intermediate.index_expression_kind(expression_id)
        else {
            continue;
        };

        let cst = &source[expression_id];
        let root = module.syntax();

        let node = cst.syntax_node_ptr().to_node(root);
        let text = node.text().to_string();
        let range = node.text_range();

        writeln!(buffer, "{}@{:?}", text.trim(), range).unwrap();
        if let Some(resolution) = resolution {
            match resolution {
                TermVariableResolution::Binder(binder) => {
                    let cst = &source[*binder];
                    let range = cst.syntax_node_ptr().text_range();
                    writeln!(buffer, "  resolves to binder {range:?}").unwrap();
                }
                TermVariableResolution::Let(LetBound { signature, equations }) => {
                    if let Some(signature) = signature {
                        let cst = &source[*signature];
                        let range = cst.syntax_node_ptr().text_range();
                        writeln!(buffer, "  resolves to signature {range:?}").unwrap();
                    }
                    for equation in equations.iter() {
                        let cst = &source[*equation];
                        let range = cst.syntax_node_ptr().text_range();
                        writeln!(buffer, "  resolves to equation {range:?}").unwrap();
                    }
                }
                TermVariableResolution::Reference(_, _) => {
                    writeln!(buffer, "  resolves to top-level name").unwrap();
                }
            }
        } else {
            writeln!(buffer, "  resolves to nothing").unwrap();
        }
    }

    writeln!(buffer, "\nTypes:\n").unwrap();

    for (type_id, _) in intermediate.iter_type() {
        let Some(TypeKind::Variable { resolution, .. }) = intermediate.index_type_kind(type_id)
        else {
            continue;
        };
        let cst = &source[type_id];
        let range = cst.syntax_node_ptr().text_range();
        writeln!(buffer, "{range:?}").unwrap();
        if let Some(resolution) = resolution {
            match resolution {
                TypeVariableResolution::Forall(id) => {
                    let cst = &source[*id];
                    let range = cst.syntax_node_ptr().text_range();
                    writeln!(buffer, "  resolves to forall {range:?}").unwrap();
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
                                let cst = &source[type_id];
                                let range = cst.syntax_node_ptr().text_range();
                                writeln!(buffer, "    {range:?}").unwrap();
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
