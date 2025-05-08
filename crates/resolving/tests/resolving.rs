use std::{fmt::Write, fs, path::Path, sync::Arc};

use files::FileId;
use indexing::{FullIndexedModule, ImportKind};
use resolving::{External, FullResolvedModule};
use rowan::ast::AstNode;
use rustc_hash::FxHashMap;
use smol_str::{SmolStr, SmolStrBuilder};
use syntax::cst;

#[derive(Debug, Default)]
struct IntegrationTestExternal {
    files: files::Files,
    names: FxHashMap<SmolStr, FileId>,
}

impl IntegrationTestExternal {
    fn parsed(&mut self, id: FileId) -> Arc<cst::Module> {
        let source = self.files.content(id);
        let lexed = lexing::lex(&source);
        let tokens = lexing::layout(&lexed);
        let (module, _) = parsing::parse(&lexed, &tokens);
        Arc::new(cst::Module::cast(module).unwrap())
    }
}

impl External for IntegrationTestExternal {
    fn indexed(&mut self, id: FileId) -> Arc<FullIndexedModule> {
        let cst = self.parsed(id);
        Arc::new(indexing::index_module(&cst))
    }

    fn resolved(&mut self, id: FileId) -> Arc<FullResolvedModule> {
        Arc::new(resolving::resolve_module(self, id))
    }

    fn file_id(&mut self, name: &str) -> Option<FileId> {
        self.names.get(name).copied()
    }
}

fn create_external(folder: &str) -> IntegrationTestExternal {
    let mut external = IntegrationTestExternal::default();

    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let folder = manifest.join("tests/fixtures").join(folder);

    for entry in folder.read_dir().unwrap() {
        let Ok(entry) = entry else { continue };
        let path = entry.path();

        let k = format!("file://{}", path.to_str().unwrap());
        let v = fs::read_to_string(path).unwrap();

        let id = external.files.insert(k, v);
        let cst = external.parsed(id);

        if let Some(cst) = cst.header().and_then(|cst| cst.name()) {
            let mut builder = SmolStrBuilder::default();
            if let Some(token) = cst.qualifier().and_then(|cst| cst.text()) {
                builder.push_str(token.text());
            }
            if let Some(token) = cst.name_token() {
                builder.push_str(token.text());
            }
            let name = builder.finish();
            external.names.insert(name, id);
        }
    }

    external
}

fn report_result(name: &str, resolved: &FullResolvedModule) -> String {
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

fn test_case(folder: &str, main: &str) -> String {
    let mut external = create_external(folder);
    let id = external.file_id(main).unwrap();
    let resolved = external.resolved(id);
    report_result(main, &resolved)
}

macro_rules! test_case {
    ($folder:tt $(| $module:tt)+) => {
        paste::paste! {
            $(
                #[test]
                fn [<test_ $folder _ $module:snake>]() {
                    let report = test_case(stringify!($folder), stringify!($module));
                    insta::assert_snapshot!(report);
                }
            )+
        }
    };
}

test_case! {
    001_local_resolution
        | Explicit
        | ExplicitSelf
        | Implicit
}

test_case! {
    002_import_resolution
        | ImportExplicit
        | ImportForLocalOnly
        | ImportHiddenConstructor
        | ImportQualifiedExplicit
        | ImportQualifiedHiding
        | ImportQualifiedImplicit
        | ImportUnqualifiedExplicit
        | ImportUnqualifiedHiding
        | ImportUnqualifiedImplicit
        | Library
        | LibraryExplicit
}

test_case! {
    003_import_errors
        | DuplicateLocal
        | DuplicateQualifiedImport
        | InvalidImport
        | LibraryA
        | LibraryB
}
