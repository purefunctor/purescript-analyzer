mod shared;

use lowering::{
    ExpressionKind, FullLoweredModule, GraphNode, LetBindingResolution, TermResolution, TypeKind,
    TypeVariableResolution,
};
use rowan::ast::AstNode;
use std::fmt::Write;
use test_each_file::test_each_file;

fn variable_scope_check(content: &str) -> String {
    let (module, _, FullLoweredModule { intermediate, source, graph, .. }) =
        shared::lower_source(content);

    let mut snapshot = String::default();

    writeln!(snapshot, "Expressions:").unwrap();
    writeln!(snapshot).unwrap();
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

        writeln!(snapshot, "{}@{:?}", text.trim(), range).unwrap();
        if let Some(resolution) = resolution {
            match resolution {
                TermResolution::Global => {
                    writeln!(snapshot, "  resolves to top-level name").unwrap();
                }
                TermResolution::Binder(binder) => {
                    let cst = &source[*binder];
                    let range = cst.syntax_node_ptr().text_range();
                    writeln!(snapshot, "  resolves to binder {range:?}").unwrap();
                }
                TermResolution::Let(LetBindingResolution { signature, equations }) => {
                    if let Some(signature) = signature {
                        let cst = &source[*signature];
                        let range = cst.syntax_node_ptr().text_range();
                        writeln!(snapshot, "  resolves to signature {range:?}").unwrap();
                    }
                    for equation in equations.iter() {
                        let cst = &source[*equation];
                        let range = cst.syntax_node_ptr().text_range();
                        writeln!(snapshot, "  resolves to equation {range:?}").unwrap();
                    }
                }
                TermResolution::AdHoc { .. } => {
                    writeln!(snapshot, "  resolves to top-level name").unwrap();
                }
            }
        } else {
            writeln!(snapshot, "  resolves to nothing").unwrap();
        }
    }

    writeln!(snapshot, "\nTypes:\n").unwrap();

    for (type_id, _) in intermediate.iter_type() {
        let Some(TypeKind::Variable { resolution, .. }) = intermediate.index_type_kind(type_id)
        else {
            continue;
        };
        let cst = &source[type_id];
        let range = cst.syntax_node_ptr().text_range();
        writeln!(snapshot, "{range:?}").unwrap();
        if let Some(resolution) = resolution {
            match resolution {
                TypeVariableResolution::Forall(id) => {
                    let cst = &source[*id];
                    let range = cst.syntax_node_ptr().text_range();
                    writeln!(snapshot, "  resolves to forall {range:?}").unwrap();
                }
                TypeVariableResolution::Implicit { binding, node, id } => {
                    if let GraphNode::Implicit { bindings, .. } = &graph[*node] {
                        let (name, type_ids) =
                            bindings.get_index(*id).expect("invariant violated: invalid index");
                        if *binding {
                            writeln!(snapshot, "  introduces a constraint variable {name:?}")
                                .unwrap();
                        } else {
                            writeln!(snapshot, "  resolves to a constraint variable {name:?}")
                                .unwrap();
                            for &type_id in type_ids {
                                let cst = &source[type_id];
                                let range = cst.syntax_node_ptr().text_range();
                                writeln!(snapshot, "    {range:?}").unwrap();
                            }
                        }
                    } else {
                        writeln!(snapshot, "  did not resolve to constraint variable!").unwrap();
                    }
                }
            }
        } else {
            writeln!(snapshot, "  resolves to nothing").unwrap();
        }
    }

    snapshot
}

test_each_file! { in "./compiler-core/lowering/tests/scope" as variable => |content: &str| {
    let snapshot = variable_scope_check(content);
    insta::assert_snapshot!(snapshot);
}}
