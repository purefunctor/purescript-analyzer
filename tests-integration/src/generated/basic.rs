use std::fmt::Write;

use analyzer::{QueryEngine, locate};
use checking::core::pretty;
use diagnostics::{DiagnosticsContext, ToDiagnostics, format_rustc};
use files::FileId;
use indexing::{ImportKind, TermItem, TypeItem, TypeItemId, TypeItemKind};
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

        writeln!(buffer).unwrap();
        writeln!(buffer, "Classes:").unwrap();
        for (name, _, _, kind) in import.iter_classes() {
            if matches!(kind, ImportKind::Hidden) {
                continue;
            }
            writeln!(buffer, "  - {name} is {kind:?}").unwrap();
        }
    }

    writeln!(buffer).unwrap();
    writeln!(buffer, "Qualified Imports:").unwrap();
    for (name, imports) in &resolved.qualified {
        for import in imports {
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

            writeln!(buffer).unwrap();
            writeln!(buffer, "{name} Classes:").unwrap();
            for (name, _, _, kind) in import.iter_classes() {
                if matches!(kind, ImportKind::Hidden) {
                    continue;
                }
                writeln!(buffer, "  - {name} is {kind:?}").unwrap();
            }
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
    writeln!(buffer, "Exported Classes:").unwrap();
    for (name, _, _) in resolved.exports.iter_classes() {
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
    writeln!(buffer, "Local Classes:").unwrap();
    for (name, _, _) in resolved.locals.iter_classes() {
        writeln!(buffer, "  - {name}").unwrap();
    }

    writeln!(buffer).unwrap();
    writeln!(buffer, "Class Members:").unwrap();
    let indexed = engine.indexed(id).unwrap();
    let mut class_member_entries: Vec<_> = resolved.class.iter().collect();
    class_member_entries.sort_by_key(|(class_id, name, _, _)| (class_id.into_raw(), name.as_str()));
    for (class_id, member_name, member_file, _) in class_member_entries {
        let class_name = resolve_class_name(engine, &indexed, id, (member_file, class_id));
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

    macro_rules! pos {
        ($id:expr) => {{
            let cst = stabilized.ast_ptr($id).unwrap();
            let range = cst.syntax_node_ptr().text_range();
            let p = locate::offset_to_position(&content, range.start()).unwrap();
            format!("{}:{}", p.line, p.character)
        }};
    }

    for (type_id, _) in info.iter_type() {
        let Some(TypeKind::Variable { resolution, .. }) = info.get_type_kind(type_id) else {
            continue;
        };

        let cst = stabilized.ast_ptr(type_id).unwrap();
        let node = cst.syntax_node_ptr().to_node(module.syntax());
        let text = node.text().to_string();

        writeln!(buffer, "{}@{}", text.trim(), pos!(type_id)).unwrap();
        match resolution {
            Some(TypeVariableResolution::Forall(id)) => {
                writeln!(buffer, "  -> forall@{}", pos!(*id)).unwrap();
            }
            Some(TypeVariableResolution::Implicit(ImplicitTypeVariable { binding, node, id })) => {
                let GraphNode::Implicit { bindings, .. } = &graph[*node] else {
                    writeln!(buffer, "  did not resolve to constraint variable!").unwrap();
                    continue;
                };
                let (name, type_ids) =
                    bindings.get_index(*id).expect("invariant violated: invalid index");
                if *binding {
                    writeln!(buffer, "  introduces a constraint variable {name:?}").unwrap();
                } else {
                    writeln!(buffer, "  -> constraint variable {name:?}").unwrap();
                    for &tid in type_ids {
                        writeln!(buffer, "    {}", pos!(tid)).unwrap();
                    }
                }
            }
            None => {
                writeln!(buffer, "  -> nothing").unwrap();
            }
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
    let node = cst.syntax_node_ptr().to_node(module.syntax());
    let text = node.text().to_string();
    let position = locate::offset_to_position(content, node.text_range().start()).unwrap();

    writeln!(buffer, "{}@{}:{}", text.trim(), position.line, position.character).unwrap();

    macro_rules! pos {
        ($id:expr) => {{
            let cst = stabilized.ast_ptr($id).unwrap();
            let range = cst.syntax_node_ptr().text_range();
            let p = locate::offset_to_position(content, range.start()).unwrap();
            format!("{}:{}", p.line, p.character)
        }};
    }

    match resolution {
        Some(TermVariableResolution::Binder(id)) => {
            writeln!(buffer, "  -> binder@{}", pos!(*id)).unwrap();
        }
        Some(TermVariableResolution::Let(let_binding_id)) => {
            let let_binding = info.get_let_binding_group(*let_binding_id);
            if let Some(sig) = let_binding.signature {
                writeln!(buffer, "  -> signature@{}", pos!(sig)).unwrap();
            }
            for eq in let_binding.equations.iter() {
                writeln!(buffer, "  -> equation@{}", pos!(*eq)).unwrap();
            }
        }
        Some(TermVariableResolution::RecordPun(id)) => {
            writeln!(buffer, "  -> record pun@{}", pos!(*id)).unwrap();
        }
        Some(TermVariableResolution::Reference(..)) => {
            writeln!(buffer, "  -> top-level").unwrap();
        }
        None => {
            writeln!(buffer, "  -> nothing").unwrap();
        }
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

    if !checked.data.is_empty() {
        writeln!(snapshot, "\nData").unwrap();
    }
    for (id, TypeItem { name, kind, .. }) in indexed.items.iter_types() {
        let (TypeItemKind::Data { .. } | TypeItemKind::Newtype { .. }) = kind else {
            continue;
        };
        let Some(name) = name else { continue };
        let Some(data) = checked.lookup_data(id) else { continue };
        writeln!(snapshot, "{name}").unwrap();
        writeln!(snapshot, "  Quantified = {}", data.quantified_variables).unwrap();
        writeln!(snapshot, "  Kind = {}", data.kind_variables).unwrap();
        writeln!(snapshot).unwrap();
    }

    if !checked.roles.is_empty() {
        writeln!(snapshot, "\nRoles").unwrap();
    }
    for (id, TypeItem { name, kind, .. }) in indexed.items.iter_types() {
        let (TypeItemKind::Data { .. }
        | TypeItemKind::Newtype { .. }
        | TypeItemKind::Foreign { .. }) = kind
        else {
            continue;
        };
        let Some(name) = name else { continue };
        let Some(roles) = checked.lookup_roles(id) else { continue };
        let roles_str: Vec<_> = roles.iter().map(|r| format!("{r:?}")).collect();
        writeln!(snapshot, "{name} = [{}]", roles_str.join(", ")).unwrap();
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

        // Print class type variables with their kinds.
        for (name, &kind) in class.type_variable_names.iter().zip(class.type_variable_kinds.iter())
        {
            let kind_str = pretty::print_global(engine, kind);
            class_line.push_str(&format!(" ({} :: {kind_str})", name.text));
        }

        writeln!(snapshot, "class {class_line}").unwrap();
    }

    if !checked.instances.is_empty() {
        writeln!(snapshot, "\nInstances").unwrap();
    }
    let mut instance_entries: Vec<_> = checked.instances.iter().collect();
    instance_entries.sort_by_key(|(id, _)| format!("{:?}", id));
    for (_instance_id, instance) in instance_entries {
        let class_name = resolve_class_name(engine, &indexed, id, instance.resolution);
        let head =
            format_instance_head(engine, &class_name, &instance.constraints, &instance.arguments);
        let forall_prefix = format_forall_prefix(engine, &instance.kind_variables);
        writeln!(snapshot, "instance {forall_prefix}{head}").unwrap();
        if let checking::core::InstanceKind::Chain { position, .. } = instance.kind {
            writeln!(snapshot, "  chain: {}", position).unwrap();
        }
    }

    if !checked.derived.is_empty() {
        writeln!(snapshot, "\nDerived").unwrap();
    }
    let mut derived_entries: Vec<_> = checked.derived.iter().collect();
    derived_entries.sort_by_key(|(id, _)| format!("{:?}", id));
    for (_derive_id, instance) in derived_entries {
        let class_name = resolve_class_name(engine, &indexed, id, instance.resolution);
        let head =
            format_instance_head(engine, &class_name, &instance.constraints, &instance.arguments);
        let forall_prefix = format_forall_prefix(engine, &instance.kind_variables);
        writeln!(snapshot, "derive {forall_prefix}{head}").unwrap();
    }

    let content = engine.content(id);

    let (parsed, _) = engine.parsed(id).unwrap();
    let root = parsed.syntax_node();

    let stabilized = engine.stabilized(id).unwrap();
    let lowered = engine.lowered(id).unwrap();
    let resolved = engine.resolved(id).unwrap();

    let context =
        DiagnosticsContext::new(&content, &root, &stabilized, &indexed, &lowered, &checked);

    let mut all_diagnostics = vec![];

    for error in &lowered.errors {
        all_diagnostics.extend(error.to_diagnostics(&context));
    }

    for error in &resolved.errors {
        all_diagnostics.extend(error.to_diagnostics(&context));
    }

    for error in &checked.errors {
        all_diagnostics.extend(error.to_diagnostics(&context));
    }

    if !all_diagnostics.is_empty() {
        writeln!(snapshot, "\nDiagnostics").unwrap();
        snapshot.push_str(&format_rustc(&all_diagnostics, &content));
    }

    snapshot
}

fn resolve_class_name(
    engine: &QueryEngine,
    indexed: &indexing::IndexedModule,
    current_file: FileId,
    resolution: (FileId, TypeItemId),
) -> String {
    let (class_file, class_type_id) = resolution;
    if class_file == current_file {
        indexed.items[class_type_id].name.as_deref().unwrap_or("<unknown>").to_string()
    } else {
        engine
            .indexed(class_file)
            .ok()
            .and_then(|idx| idx.items[class_type_id].name.as_deref().map(str::to_string))
            .unwrap_or_else(|| "<imported>".to_string())
    }
}

fn format_instance_head(
    engine: &QueryEngine,
    class_name: &str,
    constraints: &[(checking::core::TypeId, checking::core::TypeId)],
    arguments: &[(checking::core::TypeId, checking::core::TypeId)],
) -> String {
    let mut head = String::new();

    if !constraints.is_empty() {
        let formatted: Vec<_> =
            constraints.iter().map(|(t, _)| pretty::print_global(engine, *t)).collect();
        if formatted.len() == 1 {
            head.push_str(&formatted[0]);
        } else {
            head.push('(');
            head.push_str(&formatted.join(", "));
            head.push(')');
        }
        head.push_str(" => ");
    }

    head.push_str(class_name);
    for (arg_type, arg_kind) in arguments {
        let type_str = pretty::print_global(engine, *arg_type);
        let kind_str = pretty::print_global(engine, *arg_kind);
        head.push_str(&format!(" ({type_str} :: {kind_str})"));
    }

    head
}

fn format_forall_prefix(
    engine: &QueryEngine,
    kind_variables: &[(checking::core::Name, checking::core::TypeId)],
) -> String {
    if kind_variables.is_empty() {
        return String::new();
    }
    let binders: Vec<_> = kind_variables
        .iter()
        .map(|(name, kind)| {
            let kind_str = pretty::print_global(engine, *kind);
            format!("({} :: {kind_str})", name.text)
        })
        .collect();
    format!("forall {}. ", binders.join(" "))
}
