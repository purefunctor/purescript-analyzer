use std::fmt::Write;

use files::FileId;
use indexing::ImportKind;
use lowering::ResolutionDomain;
use resolving::FullResolvedModule;
use server::Compiler;

pub fn report_resolved(name: &str, resolved: &FullResolvedModule) -> String {
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

pub fn report_deferred_resolution(compiler: &mut Compiler, id: FileId) -> String {
    let resolved = compiler.runtime.resolved(id);
    let lowered = compiler.runtime.lowered(id);

    let mut buffer = String::default();
    for (id, deferred) in lowered.graph.deferred() {
        let prefix = deferred.qualifier.as_ref().map(|name| name.as_str());
        let Some(name) = &deferred.name else { continue };

        match deferred.domain {
            ResolutionDomain::Term => {
                let Some((f_id, t_id)) = resolved.lookup_term(prefix, name) else { continue };
                let (module, _) = compiler.runtime.parsed(f_id);
                let module = module.module_name().unwrap();

                let indexed = compiler.runtime.indexed(f_id);
                let item = &indexed.items[t_id];

                let Some(item) = &item.name else { continue };
                writeln!(buffer, "{:?} = {}.{}", id, module, item).unwrap();
            }
            ResolutionDomain::Type => {
                let Some((f_id, t_id)) = resolved.lookup_type(prefix, name) else { continue };
                let (module, _) = compiler.runtime.parsed(f_id);
                let module = module.module_name().unwrap();

                let indexed = compiler.runtime.indexed(f_id);
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
