use std::fmt::Write;

use analyzer::QueryEngine;
use files::FileId;
use indexing::ImportKind;
use lowering::Domain;

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

pub fn report_deferred_resolution(engine: &QueryEngine, id: FileId) -> String {
    let prim = {
        let id = engine.prim_id();
        engine.resolved(id).ok().unwrap()
    };

    let resolved = engine.resolved(id).unwrap();
    let lowered = engine.lowered(id).unwrap();

    let mut buffer = String::default();
    for (id, deferred) in lowered.graph.deferred() {
        let prefix = deferred.qualifier.as_ref().map(|name| name.as_str());
        let Some(name) = &deferred.name else { continue };

        match deferred.domain {
            Domain::Term => {
                let Some((f_id, t_id)) = resolved.lookup_term(&prim, prefix, name) else {
                    continue;
                };
                let (module, _) = engine.parsed(f_id).unwrap();
                let module = module.module_name().unwrap();

                let indexed = engine.indexed(f_id).unwrap();
                let item = &indexed.items[t_id];

                let Some(item) = &item.name else { continue };
                writeln!(buffer, "{id:?} = {module}.{item}").unwrap();
            }
            Domain::Type => {
                let Some((f_id, t_id)) = resolved.lookup_type(&prim, prefix, name) else {
                    continue;
                };
                let (module, _) = engine.parsed(f_id).unwrap();
                let module = module.module_name().unwrap();

                let indexed = engine.indexed(f_id).unwrap();
                let item = &indexed.items[t_id];

                let Some(item) = &item.name else { continue };
                writeln!(buffer, "{id:?} = {module}.{item}").unwrap();
            }
        }
    }

    if buffer.is_empty() {
        writeln!(buffer, "<empty>").unwrap()
    }

    buffer
}
