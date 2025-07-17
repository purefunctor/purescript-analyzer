use std::fmt::Write;

use analyzer::Compiler;
use files::FileId;
use indexing::ImportKind;
use lowering::ResolutionDomain;

pub fn report_resolved(compiler: &Compiler, id: FileId, name: &str) -> String {
    let resolved = compiler.engine.resolved(id).unwrap();

    let mut buffer = String::default();
    writeln!(buffer, "module {}", name).unwrap();

    writeln!(buffer).unwrap();
    writeln!(buffer, "Unqualified Imports:").unwrap();
    for import in &resolved.unqualified {
        writeln!(buffer).unwrap();
        writeln!(buffer, "Terms:").unwrap();
        for (name, _, _, kind) in import.iter_terms() {
            if matches!(kind, ImportKind::Hidden) {
                continue;
            }
            writeln!(buffer, "  - {} is {:?}", name, kind).unwrap();
        }

        writeln!(buffer).unwrap();
        writeln!(buffer, "Types:").unwrap();
        for (name, _, _, kind) in import.iter_types() {
            if matches!(kind, ImportKind::Hidden) {
                continue;
            }
            writeln!(buffer, "  - {} is {:?}", name, kind).unwrap();
        }
    }

    writeln!(buffer).unwrap();
    writeln!(buffer, "Qualified Imports:").unwrap();
    for (name, import) in &resolved.qualified {
        writeln!(buffer).unwrap();
        writeln!(buffer, "{} Terms:", name).unwrap();
        for (name, _, _, kind) in import.iter_terms() {
            if matches!(kind, ImportKind::Hidden) {
                continue;
            }
            writeln!(buffer, "  - {} is {:?}", name, kind).unwrap();
        }

        writeln!(buffer).unwrap();
        writeln!(buffer, "{} Types:", name).unwrap();
        for (name, _, _, kind) in import.iter_types() {
            if matches!(kind, ImportKind::Hidden) {
                continue;
            }
            writeln!(buffer, "  - {} is {:?}", name, kind).unwrap();
        }
    }

    writeln!(buffer).unwrap();
    writeln!(buffer, "Exported Terms:").unwrap();
    for (name, _, _) in resolved.exports.iter_terms() {
        writeln!(buffer, "  - {}", name).unwrap();
    }

    writeln!(buffer).unwrap();
    writeln!(buffer, "Exported Types:").unwrap();
    for (name, _, _) in resolved.exports.iter_types() {
        writeln!(buffer, "  - {}", name).unwrap();
    }

    writeln!(buffer).unwrap();
    writeln!(buffer, "Local Terms:").unwrap();
    for (name, _, _) in resolved.locals.iter_terms() {
        writeln!(buffer, "  - {}", name).unwrap();
    }

    writeln!(buffer).unwrap();
    writeln!(buffer, "Local Types:").unwrap();
    for (name, _, _) in resolved.locals.iter_types() {
        writeln!(buffer, "  - {}", name).unwrap();
    }

    writeln!(buffer).unwrap();
    writeln!(buffer, "Errors:").unwrap();
    for error in &resolved.errors {
        writeln!(buffer, "  - {:?}", error).unwrap();
    }

    buffer
}

pub fn report_deferred_resolution(compiler: &Compiler, id: FileId) -> String {
    let resolved = compiler.engine.resolved(id).unwrap();
    let lowered = compiler.engine.lowered(id).unwrap();

    let mut buffer = String::default();
    for (id, deferred) in lowered.graph.deferred() {
        let prefix = deferred.qualifier.as_ref().map(|name| name.as_str());
        let Some(name) = &deferred.name else { continue };

        match deferred.domain {
            ResolutionDomain::Term => {
                let Some((f_id, t_id)) = resolved.lookup_term(prefix, name) else { continue };
                let (module, _) = compiler.engine.parsed(f_id).unwrap();
                let module = module.module_name().unwrap();

                let indexed = compiler.engine.indexed(f_id).unwrap();
                let item = &indexed.items[t_id];

                let Some(item) = &item.name else { continue };
                writeln!(buffer, "{:?} = {}.{}", id, module, item).unwrap();
            }
            ResolutionDomain::Type => {
                let Some((f_id, t_id)) = resolved.lookup_type(prefix, name) else { continue };
                let (module, _) = compiler.engine.parsed(f_id).unwrap();
                let module = module.module_name().unwrap();

                let indexed = compiler.engine.indexed(f_id).unwrap();
                let item = &indexed.items[t_id];

                let Some(item) = &item.name else { continue };
                writeln!(buffer, "{:?} = {}.{}", id, module, item).unwrap();
            }
        }
    }

    if buffer.is_empty() {
        writeln!(buffer, "<empty>").unwrap()
    }

    buffer
}
