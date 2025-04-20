use std::collections::hash_map::Entry;

use files::FileId;
use indexing::{
    ExportKind, FullIndexedModule, ImplicitItems, ImportItemId, ImportKind, IndexingImport,
    IndexingImports, TermItemId, TermItemKind, TypeItemId,
};
use rustc_hash::FxHashSet;
use smol_str::SmolStr;

use crate::{
    External, FullResolvedModule, ResolvedExports, ResolvedImport, ResolvedImportsQualified,
    ResolvedImportsUnqualified, ResolvingError,
};

#[derive(Default)]
pub(super) struct State {
    pub(super) unqualified: ResolvedImportsUnqualified,
    pub(super) qualified: ResolvedImportsQualified,
    pub(super) exports: ResolvedExports,
    pub(super) errors: Vec<ResolvingError>,
}

pub(super) fn resolve_module(external: &mut impl External, file: FileId) -> State {
    let indexed = external.indexed(file);

    let mut state = State::default();
    resolve_imports(external, &mut state, &indexed.imports);
    resolve_exports(external, &mut state, &indexed, file);

    state
}

fn resolve_imports(external: &mut impl External, state: &mut State, imports: &IndexingImports) {
    for (&id, import) in imports {
        let Some(name) = &import.name else {
            state.errors.push(ResolvingError::InvalidImportStatement { id });
            continue;
        };

        let import_file_id = external.file_id(name);
        let import_indexed = external.indexed(import_file_id);
        let import_resolved = external.resolved(import_file_id);

        let mut resolved_import = ResolvedImport::new(import_file_id, import.kind, import.exported);
        if let Some(alias) = &import.alias {
            let alias = SmolStr::clone(alias);
            let resolved_import = state.qualified.entry(alias).or_insert_with(|| resolved_import);
            resolve_import(
                &mut state.errors,
                resolved_import,
                &import_indexed,
                &import_resolved,
                import,
            );
        } else {
            resolve_import(
                &mut state.errors,
                &mut resolved_import,
                &import_indexed,
                &import_resolved,
                import,
            );
            state.unqualified.push(resolved_import);
        }
    }
}

fn add_imported_term(
    errors: &mut Vec<ResolvingError>,
    resolved: &mut ResolvedImport,
    name: &SmolStr,
    id: ImportItemId,
    item: (FileId, TermItemId),
) {
    if let Some(existing) = resolved.terms.get(name) {
        if item != *existing {
            errors.push(ResolvingError::InvalidImportItem { id });
        } else {
            errors.push(ResolvingError::DuplicateImportItem { id });
        }
    } else {
        let name = SmolStr::new(name);
        resolved.terms.insert(name, item);
    }
}

fn add_imported_type(
    errors: &mut Vec<ResolvingError>,
    resolved: &mut ResolvedImport,
    name: &str,
    id: ImportItemId,
    item: (FileId, TypeItemId),
) {
    if let Some(existing) = resolved.types.get(name) {
        if item != *existing {
            errors.push(ResolvingError::InvalidImportItem { id });
        } else {
            errors.push(ResolvingError::DuplicateImportItem { id });
        }
    } else {
        let name = SmolStr::new(name);
        resolved.types.insert(name, item);
    }
}

fn resolve_import(
    errors: &mut Vec<ResolvingError>,
    resolved: &mut ResolvedImport,
    import_indexed: &FullIndexedModule,
    import_resolved: &FullResolvedModule,
    indexing_import: &IndexingImport,
) {
    for (name, &id) in &indexing_import.terms {
        if let Some(item) = import_resolved.lookup_term(name) {
            add_imported_term(errors, resolved, name, id, item);
        } else {
            errors.push(ResolvingError::InvalidImportItem { id });
        }
    }

    for (name, &(id, ref implicit)) in &indexing_import.types {
        if let Some(item) = import_resolved.lookup_type(name) {
            add_imported_type(errors, resolved, name, id, item);
        } else {
            errors.push(ResolvingError::InvalidImportItem { id });
        }
        if let Some(implicit) = implicit {
            resolve_implicit(errors, resolved, import_indexed, import_resolved, name, id, implicit);
        }
    }
}

fn resolve_implicit(
    errors: &mut Vec<ResolvingError>,
    resolved: &mut ResolvedImport,
    import_indexed: &FullIndexedModule,
    import_resolved: &FullResolvedModule,
    name: &SmolStr,
    id: ImportItemId,
    implicit: &ImplicitItems,
) {
    match implicit {
        ImplicitItems::Everything => {
            if let Some((_, type_id)) = import_resolved.lookup_type(name) {
                let constructors: FxHashSet<_> =
                    import_indexed.pairs.data_constructors(type_id).collect();
                let constructors =
                    import_resolved.iter_terms().filter(|(_, _, id)| constructors.contains(id));
                for (name, term_file, term_id) in constructors {
                    add_imported_term(errors, resolved, name, id, (term_file, term_id));
                }
            } else {
                errors.push(ResolvingError::InvalidImportItem { id });
            }
        }
        ImplicitItems::Enumerated(names) => {
            for name in names.iter() {
                if let Some(item) = import_resolved.lookup_term(name) {
                    add_imported_term(errors, resolved, name, id, item);
                } else {
                    errors.push(ResolvingError::InvalidImportItem { id });
                }
            }
        }
    }
}

fn resolve_exports(
    external: &mut impl External,
    state: &mut State,
    import_indexed: &FullIndexedModule,
    file: FileId,
) {
    export_module_items(state, import_indexed, file);
    export_module_imports(external, state, import_indexed);
}

fn add_exported_terms<'k>(
    exports: &mut ResolvedExports,
    errors: &mut Vec<ResolvingError>,
    iterator: impl Iterator<Item = (&'k SmolStr, FileId, TermItemId)>,
) {
    let (additional, _) = iterator.size_hint();
    exports.terms.reserve(additional);
    iterator.for_each(move |(name, file, id)| {
        add_exported_term(exports, errors, name, file, id);
    });
}

fn add_exported_term(
    exports: &mut ResolvedExports,
    errors: &mut Vec<ResolvingError>,
    name: &SmolStr,
    file: FileId,
    id: TermItemId,
) {
    let k = SmolStr::clone(name);
    match exports.terms.entry(k) {
        Entry::Occupied(o) => {
            let existing = *o.get();
            let duplicate = (file, id);
            if existing != duplicate {
                errors.push(ResolvingError::ExistingTerm { existing, duplicate });
            }
        }
        Entry::Vacant(v) => {
            v.insert((file, id));
        }
    }
}

fn add_exported_types<'k>(
    exports: &mut ResolvedExports,
    errors: &mut Vec<ResolvingError>,
    iterator: impl Iterator<Item = (&'k SmolStr, FileId, TypeItemId)>,
) {
    let (additional, _) = iterator.size_hint();
    exports.types.reserve(additional);
    iterator.for_each(move |(name, file, id)| {
        add_exported_type(exports, errors, name, file, id);
    });
}

fn add_exported_type(
    exports: &mut ResolvedExports,
    errors: &mut Vec<ResolvingError>,
    name: &SmolStr,
    file: FileId,
    id: TypeItemId,
) {
    let k = SmolStr::clone(name);
    match exports.types.entry(k) {
        Entry::Occupied(o) => {
            let existing = *o.get();
            let duplicate = (file, id);
            if existing != duplicate {
                errors.push(ResolvingError::ExistingType { existing, duplicate });
            }
        }
        Entry::Vacant(v) => {
            v.insert((file, id));
        }
    }
}

fn export_module_items(state: &mut State, import_indexed: &FullIndexedModule, file: FileId) {
    let terms = import_indexed.items.iter_terms().filter_map(|(id, item)| {
        // Instances cannot be to referred directly by their given name yet.
        // They're simply assumed to exist in a global context for coherence.
        if matches!(item.kind, TermItemKind::Instance { .. } | TermItemKind::Derive { .. }) {
            return None;
        }
        if matches!(import_indexed.kind, ExportKind::Explicit) && !item.exported {
            return None;
        }
        let name = item.name.as_ref()?;
        Some((name, file, id))
    });
    let types = import_indexed.items.iter_types().filter_map(|(id, item)| {
        if matches!(import_indexed.kind, ExportKind::Explicit) && !item.exported {
            return None;
        }
        let name = item.name.as_ref()?;
        Some((name, file, id))
    });
    add_exported_terms(&mut state.exports, &mut state.errors, terms);
    add_exported_types(&mut state.exports, &mut state.errors, types);
}

fn export_module_imports(
    external: &mut impl External,
    state: &mut State,
    import_indexed: &FullIndexedModule,
) {
    if matches!(import_indexed.kind, ExportKind::Implicit) {
        return;
    }

    let unqualified = state.unqualified.iter();
    let qualified = state.qualified.values();
    let imports = unqualified.chain(qualified);

    for import in imports {
        match import.kind {
            ImportKind::Implicit => {
                let resolved = external.resolved(import.file);
                let terms = resolved.iter_terms();
                let types = resolved.iter_types();
                add_exported_terms(&mut state.exports, &mut state.errors, terms);
                add_exported_types(&mut state.exports, &mut state.errors, types);
            }
            ImportKind::Explicit => {
                let terms = import.iter_terms();
                let types = import.iter_types();
                add_exported_terms(&mut state.exports, &mut state.errors, terms);
                add_exported_types(&mut state.exports, &mut state.errors, types);
            }
            ImportKind::Hidden => {
                let resolved = external.resolved(import.file);
                let terms = resolved.iter_terms().filter(|(k, _, _)| !import.contains_term(k));
                let types = resolved.iter_types().filter(|(k, _, _)| !import.contains_type(k));
                add_exported_terms(&mut state.exports, &mut state.errors, terms);
                add_exported_types(&mut state.exports, &mut state.errors, types);
            }
        }
    }
}
