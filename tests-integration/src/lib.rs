use std::{
    fmt::Write,
    fs,
    path::{Path, PathBuf},
};

use building::Runtime;
use files::{FileId, Files};
use glob::glob;
use indexing::ImportKind;
use lowering::ResolutionDomain;
use resolving::FullResolvedModule;
use rustc_hash::FxHashMap;
use smol_str::{SmolStr, SmolStrBuilder};

#[derive(Default)]
pub struct IntegrationTestCompiler {
    pub files: Files,
    pub runtime: Runtime,
    file_module: FxHashMap<FileId, SmolStr>,
}

impl IntegrationTestCompiler {
    fn load_file(&mut self, path: &Path) {
        let uri = format!("file://{}", path.to_str().unwrap());
        let file = fs::read_to_string(path).unwrap();

        let id = self.files.insert(uri, file);
        let content = self.files.content(id);

        self.runtime.set_content(id, content);
        let (parsed, _) = self.runtime.parsed(id);

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
            self.runtime.set_module_file(&name, id);
            self.file_module.insert(id, name);
        }
    }
}

fn load_folder(folder: &Path) -> impl Iterator<Item = PathBuf> {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let packages = manifest.join(folder);
    let pattern = format!("{}/**/*.purs", packages.to_str().unwrap());
    glob(&pattern).unwrap().filter_map(Result::ok)
}

pub fn load_compiler(folder: &Path) -> IntegrationTestCompiler {
    let mut compiler = IntegrationTestCompiler::default();
    load_folder(folder).for_each(|path| {
        compiler.load_file(&path);
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

pub fn report_deferred_resolution(compiler: &mut IntegrationTestCompiler, id: FileId) -> String {
    let resolved = compiler.runtime.resolved(id);
    let lowered = compiler.runtime.lowered(id);

    let mut buffer = String::default();
    for (id, deferred) in lowered.graph.deferred() {
        let prefix = deferred.qualifier.as_ref().map(|name| name.as_str());
        let Some(name) = &deferred.name else { continue };

        match deferred.domain {
            ResolutionDomain::Term => {
                let Some((f_id, t_id)) = resolved.lookup_term(prefix, name) else { continue };
                let module = compiler.file_module.get(&f_id).unwrap();

                let indexed = compiler.runtime.indexed(f_id);
                let item = &indexed.items[t_id];

                let Some(item) = &item.name else { continue };
                writeln!(buffer, "{:?} = {}.{}", id, module, item).unwrap();
            }
            ResolutionDomain::Type => {
                let Some((f_id, t_id)) = resolved.lookup_type(prefix, name) else { continue };
                let module = compiler.file_module.get(&f_id).unwrap();

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
