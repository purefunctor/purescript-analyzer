use std::sync::Arc;

use files::FileId;
use indexing::{FullModuleIndex, ImportExportKind, TermItemId, TypeItemId};
use lowering::FullModuleLower;

pub trait External {
    fn lookup_module_name(&self, name: &str) -> Option<FileId>;

    fn index(&mut self, id: FileId) -> Arc<FullModuleIndex>;

    fn lower(&mut self, id: FileId) -> Arc<FullModuleLower>;
}

pub fn resolve(external: &mut impl External, file_id: FileId) {
    let lower = external.lower(file_id);
    for (_, deferred) in lower.graph.deferred() {
        let Some(name) = &deferred.name else { continue };
        if let Some(_) = &deferred.qualifier {
            todo!("QualifiedResolution");
        } else {
            let mut context = ResolveContext::default();
            match deferred.domain {
                lowering::ResolutionDomain::Term => {
                    resolve_unqualified_term(external, &mut context, false, file_id, name);
                }
                lowering::ResolutionDomain::Type => {
                    resolve_unqualified_type(external, &mut context, false, file_id, name);
                }
            }
            dbg!((context.terms, context.types));
        }
    }
}

#[derive(Debug, Default)]
struct ResolveContext {
    terms: Vec<(FileId, ImportExportKind, TermItemId)>,
    types: Vec<(FileId, ImportExportKind, TypeItemId)>,
}

fn resolve_unqualified_term(
    external: &mut impl External,
    context: &mut ResolveContext,
    is_deep: bool,
    file_id: FileId,
    term_name: &str,
) {
    let index = &external.index(file_id).index;
    if let Some((kind, term_id, _)) = index.lookup_term_item(term_name) {
        context.terms.push((file_id, kind, term_id));
    }
    for (import_id, imported_items) in index.iter_import_items() {
        if is_deep && !imported_items.exported {
            continue;
        };
        let Some(import_name) = index.index_import_name(import_id) else {
            continue;
        };
        let Some(imported_id) = external.lookup_module_name(import_name) else {
            continue;
        };
        resolve_unqualified_term(external, context, true, imported_id, term_name);
    }
}

fn resolve_unqualified_type(
    external: &mut impl External,
    context: &mut ResolveContext,
    is_deep: bool,
    file_id: FileId,
    type_name: &str,
) {
    let index = &external.index(file_id).index;
    if let Some((kind, type_id, _)) = index.lookup_type_item(type_name) {
        context.types.push((file_id, kind, type_id));
    }
    for (import_id, imported_items) in index.iter_import_items() {
        if is_deep && !imported_items.exported {
            continue;
        };
        let Some(import_name) = index.index_import_name(import_id) else {
            continue;
        };
        let Some(imported_id) = external.lookup_module_name(import_name) else {
            continue;
        };
        resolve_unqualified_type(external, context, true, imported_id, type_name);
    }
}
