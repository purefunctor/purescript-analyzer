mod shared;

use lowering::{ExpressionKind, LetBindingResolution, TermResolution, TypeKind};
use std::fmt::Write;
use test_each_file::test_each_file;

fn variable_scope_check(content: &str) -> String {
    let (_, ir, source, _) = shared::index_source(content);

    let mut snapshot = String::default();

    writeln!(snapshot, "Expressions:").unwrap();
    writeln!(snapshot).unwrap();
    for (expression_id, _) in ir.iter_expression() {
        let Some(ExpressionKind::Variable { resolution }) = ir.index_expression_kind(expression_id)
        else {
            continue;
        };
        let cst = &source[expression_id];
        let range = cst.syntax_node_ptr().text_range();
        writeln!(snapshot, "{:?}", range).unwrap();
        if let Some(resolution) = resolution {
            match resolution {
                TermResolution::Root(_) => {
                    writeln!(snapshot, "  resolves to top-level name").unwrap();
                }
                TermResolution::Binder(binder) => {
                    let cst = &source[*binder];
                    let range = cst.syntax_node_ptr().text_range();
                    writeln!(snapshot, "  resolved to binder {:?}", range).unwrap();
                }
                TermResolution::Let(LetBindingResolution { signature, equations }) => {
                    if let Some(signature) = signature {
                        let cst = &source[*signature];
                        let range = cst.syntax_node_ptr().text_range();
                        writeln!(snapshot, "  resolves to signature {:?}", range).unwrap();
                    }
                    for equation in equations.iter() {
                        let cst = &source[*equation];
                        let range = cst.syntax_node_ptr().text_range();
                        writeln!(snapshot, "  resolves to equation {:?}", range).unwrap();
                    }
                }
            }
        } else {
            writeln!(snapshot, "  resolves to nothing").unwrap();
        }
    }

    writeln!(snapshot, "\nTypes:\n").unwrap();

    for (type_id, _) in ir.iter_type() {
        let Some(TypeKind::Variable { resolution }) = ir.index_type_kind(type_id) else {
            continue;
        };
        let cst = &source[type_id];
        let range = cst.syntax_node_ptr().text_range();
        writeln!(snapshot, "{:?}", range).unwrap();
        if let Some(resolution) = resolution {
            let cst = &source[*resolution];
            let range = cst.syntax_node_ptr().text_range();
            writeln!(snapshot, "  resolves to {:?}", range).unwrap();
        } else {
            writeln!(snapshot, "  resolves to nothing").unwrap();
        }
    }

    snapshot
}

test_each_file! { in "./crates/lowering/tests/scope" as variable => |content: &str| {
    let snapshot = variable_scope_check(content);
    insta::assert_snapshot!(snapshot);
}}
