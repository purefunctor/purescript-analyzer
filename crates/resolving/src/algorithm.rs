use std::collections::hash_map::Entry;

use smol_str::SmolStr;

use files::FileId;
use indexing::{ImplicitItems, ImportItems, ImportKind, Index, TermItemId, TypeItemId};

use crate::{Error, External, ResolvedExports, ResolvedImport, TermMap, TypeMap};

pub(crate) fn resolve_imports(
    external: &mut impl External,
    errors: &mut Vec<Error>,
    index: &Index,
) -> Vec<ResolvedImport> {
    let mut imports = vec![];
    for (id, items) in index.iter_import_items() {
        if let Some(items) = resolve_import(external, errors, items) {
            imports.push(items);
        } else {
            errors.push(Error::InvalidImportStatement { id });
        }
    }
    imports
}

fn resolve_import(
    external: &mut impl External,
    errors: &mut Vec<Error>,
    items: &ImportItems,
) -> Option<ResolvedImport> {
    let Some(name) = &items.name else {
        return None;
    };

    let import = external.file_id(name);
    let index = external.index(import);
    let resolved = external.resolved(import);

    let mut terms = TermMap::default();
    let mut types = TypeMap::default();

    for (name, &id) in &items.terms {
        if let Some((term_file, term_id)) = resolved.lookup_term(name) {
            let name = SmolStr::clone(name);
            terms.insert(name, (term_file, term_id));
        } else {
            errors.push(Error::InvalidImportItem { id });
        }
    }

    for (name, &(id, ref implicit)) in &items.types {
        if let Some((type_file, type_id)) = resolved.lookup_type(name) {
            let name = SmolStr::clone(name);
            types.insert(name, (type_file, type_id));
        } else {
            errors.push(Error::InvalidImportItem { id });
        }
        match implicit {
            Some(ImplicitItems::Everything) => {
                if let Some((_, type_id)) = resolved.lookup_type(name) {
                    for term_id in index.relational.constructors_of(type_id) {
                        if let Some((name, term_file, term_id)) =
                            resolved.iter_terms().find(|(_, _, id)| *id == term_id)
                        {
                            let name = SmolStr::new(name);
                            terms.insert(name, (term_file, term_id));
                        }
                    }
                } else {
                    errors.push(Error::InvalidImportItem { id });
                }
            }
            Some(ImplicitItems::Enumerated(names)) => {
                for name in names.iter() {
                    if let Some((term_file, term_id)) = resolved.lookup_term(name) {
                        let name = SmolStr::clone(name);
                        terms.insert(name, (term_file, term_id));
                    } else {
                        errors.push(Error::InvalidImportItem { id });
                    }
                }
            }
            None => {}
        }
    }

    Some(ResolvedImport { file: import, kind: items.kind, exported: items.exported, terms, types })
}

pub(crate) fn resolve_exports(
    external: &mut impl External,
    errors: &mut Vec<Error>,
    index: &Index,
    file: FileId,
    imports: &[ResolvedImport],
) -> ResolvedExports {
    let mut exports = ResolvedExports::default();
    match index.export_kind {
        indexing::ExportKind::Implicit => {
            implicit_module_exports(errors, &mut exports, index, file);
        }
        indexing::ExportKind::Explicit => {
            explicit_module_exports(external, errors, &mut exports, index, file, imports, false);
        }
        indexing::ExportKind::ExplicitSelf => {
            explicit_module_exports(external, errors, &mut exports, index, file, imports, true);
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

fn explicit_module_exports(
    external: &mut impl External,
    errors: &mut Vec<Error>,
    exports: &mut ResolvedExports,
    index: &Index,
    file: FileId,
    imports: &[ResolvedImport],
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
