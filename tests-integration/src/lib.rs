use std::{
    fmt::Write,
    fs,
    path::{Path, PathBuf},
};

use files::FileId;
use glob::glob;
use indexing::ImportKind;
use lowering::ResolutionDomain;
use resolving::FullResolvedModule;
use server::Compiler;
use smol_str::SmolStrBuilder;

fn load_file(compiler: &mut Compiler, path: &Path) {
    let uri = format!("file://{}", path.to_str().unwrap());
    let file = fs::read_to_string(path).unwrap();

    let id = compiler.files.insert(uri, file);
    let content = compiler.files.content(id);

    compiler.runtime.set_content(id, content);
    let (parsed, _) = compiler.runtime.parsed(id);

    let cst = parsed.cst();
    if let Some(cst) = cst.header().and_then(|cst| cst.name()) {
        let mut builder = SmolStrBuilder::default();
        if let Some(token) = cst.qualifier().and_then(|cst| cst.text()) {
            builder.push_str(token.text());
        }
        if let Some(token) = cst.name_token() {
            builder.push_str(token.text());
        }
        let name = builder.finish();
        compiler.runtime.set_module_file(&name, id);
    }
}

fn load_folder(folder: &Path) -> impl Iterator<Item = PathBuf> {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let packages = manifest.join(folder);
    let pattern = format!("{}/**/*.purs", packages.to_str().unwrap());
    glob(&pattern).unwrap().filter_map(Result::ok)
}

pub fn load_compiler(folder: &Path) -> Compiler {
    let mut compiler = Compiler::default();
    load_folder(folder).for_each(|path| {
        load_file(&mut compiler, &path);
    });
    compiler
}

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
