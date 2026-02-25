use std::fmt::Write;

use analyzer::{QueryEngine, locate};
use checking::core::pretty;
use checking2::ExternalQueries;
use checking2::core::pretty as pretty2;
use diagnostics::{DiagnosticsContext, ToDiagnostics, format_rustc};
use files::FileId;
use indexing::{ImportKind, TermItem, TypeItem, TypeItemId, TypeItemKind};
use itertools::Itertools;
use lowering::{
    ExpressionKind, GraphNode, ImplicitTypeVariable, TermVariableResolution, TypeKind,
    TypeVariableResolution,
};
use rowan::ast::AstNode;
use syntax::cst;

macro_rules! pos {
    ($content:expr, $stabilized:expr, $id:expr) => {{
        let cst = $stabilized.ast_ptr($id).unwrap();
        let range = cst.syntax_node_ptr().text_range();
        let p = locate::offset_to_position($content, range.start()).unwrap();
        format!("{}:{}", p.line, p.character)
    }};
}

fn heading(out: &mut String, title: &str) {
    writeln!(out).unwrap();
    writeln!(out, "{title}").unwrap();
}

macro_rules! write_import_items {
    ($out:expr, $title:expr, $iter:expr) => {{
        writeln!($out).unwrap();
        writeln!($out, "{}:", $title).unwrap();
        for (item_name, _, _, kind) in $iter {
            if matches!(kind, ImportKind::Hidden) {
                continue;
            }
            writeln!($out, "  - {item_name} is {kind:?}").unwrap();
        }
    }};
}

pub fn report_resolved(engine: &QueryEngine, id: FileId, name: &str) -> String {
    let resolved = engine.resolved(id).unwrap();

    let mut out = String::default();
    writeln!(out, "module {name}").unwrap();

    heading(&mut out, "Unqualified Imports:");
    for import in resolved.unqualified.values().flatten() {
        write_import_items!(out, "Terms", import.iter_terms());
        write_import_items!(out, "Types", import.iter_types());
        write_import_items!(out, "Classes", import.iter_classes());
    }

    heading(&mut out, "Qualified Imports:");
    for (qualifier, imports) in &resolved.qualified {
        for import in imports {
            write_import_items!(out, format!("{qualifier} Terms"), import.iter_terms());
            write_import_items!(out, format!("{qualifier} Types"), import.iter_types());
            write_import_items!(out, format!("{qualifier} Classes"), import.iter_classes());
        }
    }

    heading(&mut out, "Exported Terms:");
    for (name, _, _) in resolved.exports.iter_terms() {
        writeln!(out, "  - {name}").unwrap();
    }

    heading(&mut out, "Exported Types:");
    for (name, _, _) in resolved.exports.iter_types() {
        writeln!(out, "  - {name}").unwrap();
    }

    heading(&mut out, "Exported Classes:");
    for (name, _, _) in resolved.exports.iter_classes() {
        writeln!(out, "  - {name}").unwrap();
    }

    heading(&mut out, "Local Terms:");
    for (name, _, _) in resolved.locals.iter_terms() {
        writeln!(out, "  - {name}").unwrap();
    }

    heading(&mut out, "Local Types:");
    for (name, _, _) in resolved.locals.iter_types() {
        writeln!(out, "  - {name}").unwrap();
    }

    heading(&mut out, "Local Classes:");
    for (name, _, _) in resolved.locals.iter_classes() {
        writeln!(out, "  - {name}").unwrap();
    }

    heading(&mut out, "Class Members:");
    let indexed = engine.indexed(id).unwrap();
    let mut class_member_entries: Vec<_> = resolved.class.iter().collect();
    class_member_entries.sort_by_key(|(class_id, name, _, _)| (class_id.into_raw(), name.as_str()));
    for (class_id, member_name, member_file, _) in class_member_entries {
        let class_name = resolve_class_name(engine, &indexed, id, (member_file, class_id));
        let locality = if member_file == id { "" } else { " (imported)" };
        writeln!(out, "  - {class_name}.{member_name}{locality}").unwrap();
    }

    heading(&mut out, "Errors:");
    for error in &resolved.errors {
        writeln!(out, "  - {error:?}").unwrap();
    }

    out
}

pub fn report_lowered(engine: &QueryEngine, id: FileId, name: &str) -> String {
    let content = engine.content(id);
    let (parsed, _) = engine.parsed(id).unwrap();

    let stabilized = engine.stabilized(id).unwrap();
    let lowered = engine.lowered(id).unwrap();

    let module = parsed.cst();
    let info = &lowered.info;
    let graph = &lowered.graph;

    let mut out = String::default();
    writeln!(out, "module {name}").unwrap();

    writeln!(out).unwrap();
    writeln!(out, "Expressions:").unwrap();
    writeln!(out).unwrap();
    for (expression_id, _) in info.iter_expression() {
        let Some(kind) = info.get_expression_kind(expression_id) else {
            continue;
        };
        if let ExpressionKind::Variable { resolution, .. } = kind {
            write_term_resolution(
                &content,
                &stabilized,
                &module,
                info,
                &mut out,
                expression_id,
                resolution,
            );
        } else if let ExpressionKind::Record { record } = kind {
            for field in record.iter() {
                if let lowering::ExpressionRecordItem::RecordPun { resolution, .. } = field {
                    write_term_resolution(
                        &content,
                        &stabilized,
                        &module,
                        info,
                        &mut out,
                        expression_id,
                        resolution,
                    );
                }
            }
        } else {
            continue;
        }
    }

    writeln!(out, "\nTypes:\n").unwrap();

    for (type_id, _) in info.iter_type() {
        let Some(TypeKind::Variable { resolution, .. }) = info.get_type_kind(type_id) else {
            continue;
        };

        let cst = stabilized.ast_ptr(type_id).unwrap();
        let node = cst.syntax_node_ptr().to_node(module.syntax());
        let text = node.text().to_string();

        writeln!(out, "{}@{}", text.trim(), pos!(&content, &stabilized, type_id)).unwrap();
        match resolution {
            Some(TypeVariableResolution::Forall(id)) => {
                writeln!(out, "  -> forall@{}", pos!(&content, &stabilized, *id)).unwrap();
            }
            Some(TypeVariableResolution::Implicit(ImplicitTypeVariable { binding, node, id })) => {
                let GraphNode::Implicit { bindings, .. } = &graph[*node] else {
                    writeln!(out, "  did not resolve to constraint variable!").unwrap();
                    continue;
                };
                let (name, type_ids) =
                    bindings.get_index(*id).expect("invariant violated: invalid index");
                if *binding {
                    writeln!(out, "  introduces a constraint variable {name:?}").unwrap();
                } else {
                    writeln!(out, "  -> constraint variable {name:?}").unwrap();
                    for &tid in type_ids {
                        writeln!(out, "    {}", pos!(&content, &stabilized, tid)).unwrap();
                    }
                }
            }
            None => {
                writeln!(out, "  -> nothing").unwrap();
            }
        }
    }

    out
}

pub fn report_checked(engine: &QueryEngine, id: FileId) -> String {
    let indexed = engine.indexed(id).unwrap();
    let checked = engine.checked(id).unwrap();

    let mut out = String::default();

    write_checked_output(&mut out, engine, id, &indexed, &checked);
    write_checked_diagnostics(&mut out, engine, id, &indexed, &checked);

    out
}

pub fn report_checked2(engine: &QueryEngine, id: FileId) -> String {
    let indexed = engine.indexed(id).unwrap();
    let checked = engine.checked2(id).unwrap();

    let mut out = String::default();

    writeln!(out, "Terms").unwrap();
    for (id, TermItem { name, .. }) in indexed.items.iter_terms() {
        let Some(name) = name else { continue };
        let Some(kind) = checked.lookup_term(id) else { continue };
        let signature = pretty2::Pretty::new(engine).signature(name).render(kind);
        writeln!(out, "{signature}").unwrap();
    }

    writeln!(out, "\nTypes").unwrap();
    for (id, TypeItem { name, .. }) in indexed.items.iter_types() {
        let Some(name) = name else { continue };
        let Some(kind) = checked.lookup_type(id) else { continue };
        let signature = pretty2::Pretty::new(engine).signature(name).render(kind);
        writeln!(out, "{signature}").unwrap();
    }

    if !checked.synonyms.is_empty() {
        writeln!(out, "\nSynonyms").unwrap();
    }
    for (id, TypeItem { name, .. }) in indexed.items.iter_types() {
        let Some(name) = name else { continue };
        let Some(definition) = checked.lookup_synonym(id) else { continue };
        let names = definition.parameters.iter().map(|b| (b.name, engine.lookup_smol_str(b.text)));
        let replacement = pretty2::Pretty::new(engine).names(names).render(definition.synonym);
        let binders =
            definition.parameters.iter().map(|b| engine.lookup_smol_str(b.text)).collect_vec();
        let binders_formatted =
            if binders.is_empty() { String::new() } else { format!(" {}", binders.join(" ")) };
        writeln!(out, "type {name}{binders_formatted} = {replacement}").unwrap();
    }

    if !checked.classes.is_empty() {
        writeln!(out, "\nClasses").unwrap();
    }
    for (id, TypeItem { .. }) in indexed.items.iter_types() {
        let Some(class) = checked.lookup_class(id) else { continue };

        let canonical = pretty2::Pretty::new(engine).render(class.canonical);

        let mut superclasses = String::new();
        if !class.superclasses.is_empty() {
            let formatted = class
                .superclasses
                .iter()
                .map(|&superclass| pretty2::Pretty::new(engine).render(superclass))
                .collect_vec();
            superclasses = format!(" <= {}", formatted.join(", "));
        }

        writeln!(out, "class {canonical}{superclasses}").unwrap();
        for &mid in &class.members {
            let Some(member_name) = indexed.items[mid].name.as_deref() else { continue };
            let Some(member_type) = checked.lookup_term(mid) else { continue };
            let signature = pretty2::Pretty::new(engine).signature(member_name).render(member_type);
            writeln!(out, "  {signature}").unwrap();
        }
    }

    if !checked.roles.is_empty() {
        writeln!(out, "\nRoles").unwrap();
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
        writeln!(out, "{name} = [{}]", roles_str.join(", ")).unwrap();
    }

    if !checked.errors.is_empty() {
        writeln!(out, "\nErrors").unwrap();
    }
    for error in &checked.errors {
        writeln!(out, "{error:#?}").unwrap();
    }

    out
}

fn write_term_resolution(
    content: &str,
    stabilized: &stabilizing::StabilizedModule,
    module: &cst::Module,
    info: &lowering::LoweringInfo,
    out: &mut String,
    expression_id: lowering::ExpressionId,
    resolution: &Option<TermVariableResolution>,
) {
    let cst = stabilized.ast_ptr(expression_id).unwrap();
    let node = cst.syntax_node_ptr().to_node(module.syntax());
    let text = node.text().to_string();
    let position = locate::offset_to_position(content, node.text_range().start()).unwrap();

    writeln!(out, "{}@{}:{}", text.trim(), position.line, position.character).unwrap();

    match resolution {
        Some(TermVariableResolution::Binder(id)) => {
            writeln!(out, "  -> binder@{}", pos!(content, stabilized, *id)).unwrap();
        }
        Some(TermVariableResolution::Let(let_binding_id)) => {
            let let_binding = info.get_let_binding_group(*let_binding_id);
            if let Some(sig) = let_binding.signature {
                writeln!(out, "  -> signature@{}", pos!(content, stabilized, sig)).unwrap();
            }
            for eq in let_binding.equations.iter() {
                writeln!(out, "  -> equation@{}", pos!(content, stabilized, *eq)).unwrap();
            }
        }
        Some(TermVariableResolution::RecordPun(id)) => {
            writeln!(out, "  -> record pun@{}", pos!(content, stabilized, *id)).unwrap();
        }
        Some(TermVariableResolution::Reference(..)) => {
            writeln!(out, "  -> top-level").unwrap();
        }
        None => {
            writeln!(out, "  -> nothing").unwrap();
        }
    }
}

fn write_checked_output(
    out: &mut String,
    engine: &QueryEngine,
    id: FileId,
    indexed: &indexing::IndexedModule,
    checked: &checking::CheckedModule,
) {
    writeln!(out, "Terms").unwrap();
    for (id, TermItem { name, .. }) in indexed.items.iter_terms() {
        let Some(n) = name else { continue };
        let Some(t) = checked.lookup_term(id) else { continue };
        let signature = pretty::print_signature_global(engine, n, t);
        writeln!(out, "{signature}").unwrap();
    }

    writeln!(out, "\nTypes").unwrap();
    for (id, TypeItem { name, .. }) in indexed.items.iter_types() {
        let Some(n) = name else { continue };
        let Some(t) = checked.lookup_type(id) else { continue };
        let signature = pretty::print_signature_global(engine, n, t);
        writeln!(out, "{signature}").unwrap();
    }

    if !checked.synonyms.is_empty() {
        writeln!(out, "\nSynonyms").unwrap();
    }
    for (id, TypeItem { name, .. }) in indexed.items.iter_types() {
        let Some(name) = name else { continue };
        let Some(group) = checked.lookup_synonym(id) else { continue };
        let synonym = pretty::print_global(engine, group.synonym_type);
        writeln!(out, "{name} = {synonym}").unwrap();
        writeln!(out, "  Quantified = {}", group.quantified_variables).unwrap();
        writeln!(out, "  Kind = {}", group.kind_variables).unwrap();
        writeln!(out, "  Type = {}", group.type_variables).unwrap();
        writeln!(out).unwrap();
    }

    if !checked.data.is_empty() {
        writeln!(out, "\nData").unwrap();
    }
    for (id, TypeItem { name, kind, .. }) in indexed.items.iter_types() {
        let (TypeItemKind::Data { .. } | TypeItemKind::Newtype { .. }) = kind else {
            continue;
        };
        let Some(name) = name else { continue };
        let Some(data) = checked.lookup_data(id) else { continue };
        writeln!(out, "{name}").unwrap();
        writeln!(out, "  Quantified = {}", data.quantified_variables).unwrap();
        writeln!(out, "  Kind = {}", data.kind_variables).unwrap();
        writeln!(out).unwrap();
    }

    if !checked.roles.is_empty() {
        writeln!(out, "\nRoles").unwrap();
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
        writeln!(out, "{name} = [{}]", roles_str.join(", ")).unwrap();
    }

    if !checked.classes.is_empty() {
        writeln!(out, "\nClasses").unwrap();
    }
    for (type_id, TypeItem { name, kind, .. }) in indexed.items.iter_types() {
        let TypeItemKind::Class { .. } = kind else { continue };
        let Some(name) = name else { continue };
        let Some(class) = checked.lookup_class(type_id) else { continue };

        let mut class_line = String::new();

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

        for (name, &kind) in class.type_variable_names.iter().zip(class.type_variable_kinds.iter())
        {
            let kind_str = pretty::print_global(engine, kind);
            class_line.push_str(&format!(" ({} :: {kind_str})", name.text));
        }

        writeln!(out, "class {class_line}").unwrap();
    }

    if !checked.instances.is_empty() {
        writeln!(out, "\nInstances").unwrap();
    }
    let mut instance_entries: Vec<_> = checked.instances.iter().collect();
    instance_entries.sort_by_key(|(id, _)| *id);
    for (_instance_id, instance) in instance_entries {
        let class_name = resolve_class_name(engine, indexed, id, instance.resolution);
        let head =
            format_instance_head(engine, &class_name, &instance.constraints, &instance.arguments);
        let forall_prefix = format_forall_prefix(engine, &instance.kind_variables);
        writeln!(out, "instance {forall_prefix}{head}").unwrap();
        if let checking::core::InstanceKind::Chain { position, .. } = instance.kind {
            writeln!(out, "  chain: {}", position).unwrap();
        }
    }

    if !checked.derived.is_empty() {
        writeln!(out, "\nDerived").unwrap();
    }
    let mut derived_entries: Vec<_> = checked.derived.iter().collect();
    derived_entries.sort_by_key(|(id, _)| *id);
    for (_derive_id, instance) in derived_entries {
        let class_name = resolve_class_name(engine, indexed, id, instance.resolution);
        let head =
            format_instance_head(engine, &class_name, &instance.constraints, &instance.arguments);
        let forall_prefix = format_forall_prefix(engine, &instance.kind_variables);
        writeln!(out, "derive {forall_prefix}{head}").unwrap();
    }
}

fn write_checked_diagnostics(
    out: &mut String,
    engine: &QueryEngine,
    id: FileId,
    indexed: &indexing::IndexedModule,
    checked: &checking::CheckedModule,
) {
    let content = engine.content(id);
    let (parsed, _) = engine.parsed(id).unwrap();
    let root = parsed.syntax_node();
    let stabilized = engine.stabilized(id).unwrap();
    let lowered = engine.lowered(id).unwrap();
    let resolved = engine.resolved(id).unwrap();

    let context = DiagnosticsContext::new(&content, &root, &stabilized, indexed, &lowered, checked);

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
        writeln!(out, "\nDiagnostics").unwrap();
        out.push_str(&format_rustc(&all_diagnostics, &content));
    }
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
