use std::fmt::Write;

use analyzer::{QueryEngine, locate};
use checking::core::pretty;
use files::FileId;
use indexing::{ImportKind, TermItem, TypeItem, TypeItemKind};
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
    writeln!(buffer, "Class Members:").unwrap();
    let indexed = engine.indexed(id).unwrap();
    let mut class_member_entries: Vec<_> = resolved.class.iter().collect();
    class_member_entries.sort_by_key(|(class_id, name, _, _)| (class_id.into_raw(), name.as_str()));
    for (class_id, member_name, member_file, _) in class_member_entries {
        let class_name: String = if member_file == id {
            let name = indexed.items[class_id].name.as_ref();
            name.map_or_else(|| "<unknown>".to_string(), |name| name.to_string())
        } else {
            let indexed = engine.indexed(member_file).unwrap();
            let name = indexed.items[class_id].name.as_ref();
            name.map_or_else(|| "<imported>".to_string(), |name| name.to_string())
        };
        let locality = if member_file == id { "" } else { " (imported)" };
        writeln!(buffer, "  - {class_name}.{member_name}{locality}").unwrap();
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
        let signature = pretty::print_signature_global(engine, n, t);
        writeln!(snapshot, "{signature}").unwrap();
    }

    writeln!(snapshot, "\nTypes").unwrap();
    for (id, TypeItem { name, .. }) in indexed.items.iter_types() {
        let Some(n) = name else { continue };
        let Some(t) = checked.lookup_type(id) else { continue };
        let signature = pretty::print_signature_global(engine, n, t);
        writeln!(snapshot, "{signature}").unwrap();
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

    if !checked.classes.is_empty() {
        writeln!(snapshot, "\nClasses").unwrap();
    }
    for (type_id, TypeItem { name, kind, .. }) in indexed.items.iter_types() {
        let TypeItemKind::Class { .. } = kind else { continue };
        let Some(name) = name else { continue };
        let Some(class) = checked.lookup_class(type_id) else { continue };

        let mut class_line = String::new();

        // Print superclasses first (before <=)
        if !class.superclasses.is_empty() {
            for (i, (superclass_type, _)) in class.superclasses.iter().enumerate() {
                if i > 0 {
                    class_line.push_str(", ");
                }
                let type_str = pretty::print_global(engine, *superclass_type);
                class_line.push_str(&type_str);
            }
            class_line.push_str(" <= ");
        }

        class_line.push_str(name);

        // Print class type variables with their kinds
        // level = quantified_variables + kind_variables + index (matches localize_class)
        for (index, &kind) in class.type_variable_kinds.iter().enumerate() {
            let level = class.quantified_variables.0 + class.kind_variables.0 + index as u32;
            let kind_str = pretty::print_global(engine, kind);
            class_line.push_str(&format!(" (&{level} :: {kind_str})"));
        }

        writeln!(snapshot, "class {class_line}").unwrap();
    }

    if !checked.instances.is_empty() {
        writeln!(snapshot, "\nInstances").unwrap();
    }
    let mut instance_entries: Vec<_> = checked.instances.iter().collect();
    instance_entries.sort_by_key(|(id, _)| format!("{:?}", id));
    for (_instance_id, instance) in instance_entries {
        // Build instance head: constraints => ClassName (arg :: kind)...
        let (class_file, class_type_id) = instance.resolution;
        let class_name: String = if class_file == id {
            indexed.items[class_type_id]
                .name
                .as_ref()
                .map(|s| s.to_string())
                .unwrap_or_else(|| "<unknown>".to_string())
        } else {
            engine
                .indexed(class_file)
                .ok()
                .and_then(|idx| idx.items[class_type_id].name.as_ref().map(|s| s.to_string()))
                .unwrap_or_else(|| "<imported>".to_string())
        };

        let mut head = String::new();

        // Print constraints (without kinds)
        if !instance.constraints.is_empty() {
            let constraints: Vec<_> = instance
                .constraints
                .iter()
                .map(|(t, _)| pretty::print_global(engine, *t))
                .collect();
            if constraints.len() == 1 {
                head.push_str(&constraints[0]);
            } else {
                head.push('(');
                head.push_str(&constraints.join(", "));
                head.push(')');
            }
            head.push_str(" => ");
        }

        // Print class name and arguments with kinds
        head.push_str(&class_name);
        for (arg_type, arg_kind) in &instance.arguments {
            let type_str = pretty::print_global(engine, *arg_type);
            let kind_str = pretty::print_global(engine, *arg_kind);
            head.push_str(&format!(" ({type_str} :: {kind_str})"));
        }

        writeln!(snapshot, "instance {head}").unwrap();
        if let checking::core::InstanceKind::Chain { position, .. } = instance.kind {
            writeln!(snapshot, "  chain: {}", position).unwrap();
        }
    }

    if !checked.errors.is_empty() {
        writeln!(snapshot, "\nErrors").unwrap();
    }
    for error in &checked.errors {
        match error.kind {
            checking::error::ErrorKind::CannotUnify { t1, t2 } => {
                let t1_pretty = pretty::print_global(engine, t1);
                let t2_pretty = pretty::print_global(engine, t2);
                writeln!(
                    snapshot,
                    "CannotUnify {{ {t1_pretty}, {t2_pretty} }} at {:?}",
                    &error.step
                )
                .unwrap();
            }
            checking::error::ErrorKind::NoInstanceFound { constraint } => {
                let constraint_pretty = pretty::print_global(engine, constraint);
                writeln!(
                    snapshot,
                    "NoInstanceFound {{ {constraint_pretty} }} at {:?}",
                    &error.step
                )
                .unwrap();
            }
            checking::error::ErrorKind::AmbiguousConstraint { constraint } => {
                let constraint_pretty = pretty::print_global(engine, constraint);
                writeln!(
                    snapshot,
                    "AmbiguousConstraint {{ {constraint_pretty} }} at {:?}",
                    &error.step
                )
                .unwrap();
            }
            _ => {
                writeln!(snapshot, "{:?} at {:?}", error.kind, &error.step).unwrap();
            }
        }
    }

    snapshot
}
