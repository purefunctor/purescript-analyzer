use std::collections::hash_map::Entry;

use rustc_hash::{FxHashMap, FxHashSet};
use smol_str::SmolStr;

use files::FileId;
use indexing::{
    FullModuleIndex, ImplicitItems, ImportItemId, ImportItems, ImportKind, Index, TermItemId,
    TypeItemId,
};

use crate::{
    Error, External, FullResolvedModule, QualifiedResolvedImports, ResolvedExports, ResolvedImport,
    UnqualifiedResolvedImports,
};

pub(crate) fn resolve_imports(
    external: &mut impl External,
    errors: &mut Vec<Error>,
    index: &Index,
) -> (UnqualifiedResolvedImports, QualifiedResolvedImports) {
    let mut unqualified = vec![];
    let mut qualified = FxHashMap::default();

    for (id, items) in index.iter_import_items() {
        let Some(name) = &items.name else {
            errors.push(Error::InvalidImportStatement { id });
            continue;
        };

        let import_file_id = external.file_id(name);
        let import_indexed = external.indexed(import_file_id);
        let import_resolved = external.resolved(import_file_id);

        if let Some(alias) = &items.alias {
            let alias = SmolStr::clone(alias);
            let resolved_import = qualified
                .entry(alias)
                .or_insert_with(|| ResolvedImport::new(import_file_id, items.kind, items.exported));
            resolve_import(errors, items, &import_indexed, &import_resolved, resolved_import);
        } else {
            let mut resolved_import =
                ResolvedImport::new(import_file_id, items.kind, items.exported);
            resolve_import(errors, items, &import_indexed, &import_resolved, &mut resolved_import);
            unqualified.push(resolved_import);
        }
    }

    (unqualified, qualified)
}

fn add_resolved_term(
    errors: &mut Vec<Error>,
    resolved_import: &mut ResolvedImport,
    name: &str,
    id: ImportItemId,
    item: (FileId, TermItemId),
) {
    if let Some(existing) = resolved_import.terms.get(name) {
        if item != *existing {
            errors.push(Error::InvalidImportItem { id });
        } else {
            errors.push(Error::DuplicateImportItem { id });
        }
    } else {
        let name = SmolStr::new(name);
        resolved_import.terms.insert(name, item);
    }
}

fn add_resolved_type(
    errors: &mut Vec<Error>,
    resolved_import: &mut ResolvedImport,
    name: &str,
    id: ImportItemId,
    item: (FileId, TypeItemId),
) {
    if let Some(existing) = resolved_import.types.get(name) {
        if item != *existing {
            errors.push(Error::InvalidImportItem { id });
        } else {
            errors.push(Error::DuplicateImportItem { id });
        }
    } else {
        let name = SmolStr::new(name);
        resolved_import.types.insert(name, item);
    }
}

fn resolve_import(
    errors: &mut Vec<Error>,
    import_items: &ImportItems,
    import_indexed: &FullModuleIndex,
    import_resolved: &FullResolvedModule,
    resolved_import: &mut ResolvedImport,
) {
    for (name, &id) in &import_items.terms {
        if let Some(item) = import_resolved.lookup_term(name) {
            add_resolved_term(errors, resolved_import, name, id, item);
        } else {
            errors.push(Error::InvalidImportItem { id });
        }
    }

    for (name, &(id, ref implicit)) in &import_items.types {
        if let Some(item) = import_resolved.lookup_type(name) {
            add_resolved_type(errors, resolved_import, name, id, item);
        } else {
            errors.push(Error::InvalidImportItem { id });
        }

        if let Some(implicit) = implicit {
            resolve_implicit(
                errors,
                import_indexed,
                import_resolved,
                resolved_import,
                name,
                id,
                implicit,
            );
        }
    }
}

fn resolve_implicit(
    errors: &mut Vec<Error>,
    import_indexed: &FullModuleIndex,
    import_resolved: &FullResolvedModule,
    resolved_import: &mut ResolvedImport,
    name: &str,
    id: ImportItemId,
    implicit: &ImplicitItems,
) {
    match implicit {
        ImplicitItems::Everything => {
            if let Some((_, type_id)) = import_resolved.lookup_type(name) {
                let constructors: FxHashSet<_> =
                    import_indexed.relational.constructors_of(type_id).collect();
                let constructors =
                    import_resolved.iter_terms().filter(|(_, _, id)| constructors.contains(id));
                for (name, term_file, term_id) in constructors {
                    add_resolved_term(errors, resolved_import, name, id, (term_file, term_id));
                }
            } else {
                errors.push(Error::InvalidImportItem { id });
            }
        }
        ImplicitItems::Enumerated(names) => {
            for name in names.iter() {
                if let Some(item) = import_resolved.lookup_term(name) {
                    add_resolved_term(errors, resolved_import, name, id, item);
                } else {
                    errors.push(Error::InvalidImportItem { id });
                }
            }
        }
    }
}

pub(crate) fn resolve_exports<'a>(
    external: &mut impl External,
    errors: &mut Vec<Error>,
    index: &Index,
    imports: impl Iterator<Item = &'a ResolvedImport>,
    file: FileId,
) -> ResolvedExports {
    let mut exports = ResolvedExports::default();
    match index.export_kind {
        indexing::ExportKind::Implicit => {
            implicit_module_exports(errors, &mut exports, index, file);
        }
        indexing::ExportKind::Explicit => {
            explicit_module_exports(external, errors, &mut exports, index, imports, file, false);
        }
        indexing::ExportKind::ExplicitSelf => {
            explicit_module_exports(external, errors, &mut exports, index, imports, file, true);
        }
    }
    exports
}

fn add_terms<'a>(
    exports: &mut ResolvedExports,
    errors: &mut Vec<Error>,
    iterator: impl Iterator<Item = (&'a str, FileId, TermItemId)>,
) {
    let (additional, _) = iterator.size_hint();
    exports.terms.reserve(additional);
    iterator.for_each(move |(name, file, id)| {
        add_term(exports, errors, name, file, id);
    });
}

fn add_types<'a>(
    exports: &mut ResolvedExports,
    errors: &mut Vec<Error>,
    iterator: impl Iterator<Item = (&'a str, FileId, TypeItemId)>,
) {
    let (additional, _) = iterator.size_hint();
    exports.types.reserve(additional);
    iterator.for_each(move |(name, file, id)| {
        add_type(exports, errors, name, file, id);
    });
}

fn add_term(
    exports: &mut ResolvedExports,
    errors: &mut Vec<Error>,
    name: &str,
    file: FileId,
    id: TermItemId,
) {
    let k = SmolStr::from(name);
    match exports.terms.entry(k) {
        Entry::Occupied(o) => {
            let existing = *o.get();
            let duplicate = (file, id);
            if existing != duplicate {
                errors.push(Error::ExistingTerm { existing, duplicate });
            }
        }
        Entry::Vacant(v) => {
            v.insert((file, id));
        }
    }
}

fn add_type(
    exports: &mut ResolvedExports,
    errors: &mut Vec<Error>,
    name: &str,
    file: FileId,
    id: TypeItemId,
) {
    let k = SmolStr::from(name);
    match exports.types.entry(k) {
        Entry::Occupied(o) => {
            let existing = *o.get();
            let duplicate = (file, id);
            if existing != duplicate {
                errors.push(Error::ExistingType { existing, duplicate });
            }
        }
        Entry::Vacant(v) => {
            v.insert((file, id));
        }
    }
}

fn implicit_module_exports(
    errors: &mut Vec<Error>,
    exports: &mut ResolvedExports,
    index: &Index,
    file: FileId,
) {
    let terms = index.iter_term_nominal().map(|(name, id)| (name, file, id));
    let types = index.iter_type_nominal().map(|(name, id)| (name, file, id));
    add_terms(exports, errors, terms);
    add_types(exports, errors, types);
}

fn explicit_module_exports<'a>(
    external: &mut impl External,
    errors: &mut Vec<Error>,
    exports: &mut ResolvedExports,
    index: &Index,
    imports: impl Iterator<Item = &'a ResolvedImport>,
    file: FileId,
    including_self: bool,
) {
    if including_self {
        implicit_module_exports(errors, exports, index, file);
    } else {
        let terms = index.iter_exported_terms().map(|(name, id)| (name, file, id));
        let types = index.iter_exported_types().map(|(name, id)| (name, file, id));
        add_terms(exports, errors, terms);
        add_types(exports, errors, types);
    }

    for import in imports {
        match import.kind {
            ImportKind::Implicit => {
                let resolved = external.resolved(import.file);
                let terms = resolved.iter_terms();
                let types = resolved.iter_types();
                add_terms(exports, errors, terms);
                add_types(exports, errors, types);
            }
            ImportKind::Explicit => {
                let terms = import.iter_terms();
                let types = import.iter_types();
                add_terms(exports, errors, terms);
                add_types(exports, errors, types);
            }
            ImportKind::Hidden => {
                let resolved = external.resolved(import.file);
                let terms = resolved.iter_terms().filter(|(k, _, _)| !import.contains_term(k));
                let types = resolved.iter_types().filter(|(k, _, _)| !import.contains_type(k));
                add_terms(exports, errors, terms);
                add_types(exports, errors, types);
            }
        }
    }
}
