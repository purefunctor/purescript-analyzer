use building_types::QueryResult;
use files::FileId;
use indexing::{
    ExportKind, ImplicitItems, ImportId, ImportKind, IndexedModule, IndexingImport, TermItemId,
    TermItemKind, TypeItemId,
};
use smol_str::SmolStr;

use crate::{
    ExportSource, ExternalQueries, ResolvedExports, ResolvedImport, ResolvedImportsQualified,
    ResolvedImportsUnqualified, ResolvedLocals, ResolvingError,
};

#[derive(Default)]
pub(super) struct State {
    pub(super) unqualified: ResolvedImportsUnqualified,
    pub(super) qualified: ResolvedImportsQualified,
    pub(super) exports: ResolvedExports,
    pub(super) locals: ResolvedLocals,
    pub(super) errors: Vec<ResolvingError>,
}

pub(super) fn resolve_module(queries: &impl ExternalQueries, file: FileId) -> QueryResult<State> {
    let indexed = queries.indexed(file)?;

    let mut state = State::default();
    resolve_imports(queries, &mut state, &indexed)?;
    resolve_exports(&mut state, &indexed, file);

    Ok(state)
}

fn resolve_imports(
    queries: &impl ExternalQueries,
    state: &mut State,
    indexed: &IndexedModule,
) -> QueryResult<()> {
    for (&indexing_import_id, indexing_import) in &indexed.imports {
        let Some(name) = &indexing_import.name else {
            state.errors.push(ResolvingError::InvalidImportStatement { id: indexing_import_id });
            continue;
        };

        let Some(import_file_id) = queries.module_file(name) else {
            state.errors.push(ResolvingError::InvalidImportStatement { id: indexing_import_id });
            continue;
        };

        let mut resolved_import = ResolvedImport::new(
            indexing_import_id,
            import_file_id,
            indexing_import.kind,
            indexing_import.exported,
        );

        if let Some(alias) = &indexing_import.alias {
            let alias = SmolStr::clone(alias);
            let resolved_import = state.qualified.entry(alias).or_insert(resolved_import);
            resolve_import(
                queries,
                &mut state.errors,
                resolved_import,
                indexing_import_id,
                indexing_import,
                import_file_id,
            )?;
        } else {
            let name = SmolStr::clone(name);
            resolve_import(
                queries,
                &mut state.errors,
                &mut resolved_import,
                indexing_import_id,
                indexing_import,
                import_file_id,
            )?;
            state.unqualified.entry(name).or_default().push(resolved_import);
        }
    }

    Ok(())
}

fn resolve_import(
    queries: &impl ExternalQueries,
    errors: &mut Vec<ResolvingError>,
    resolved: &mut ResolvedImport,
    indexing_import_id: ImportId,
    indexing_import: &IndexingImport,
    import_file_id: FileId,
) -> QueryResult<()> {
    let kind = match indexing_import.kind {
        ImportKind::Implicit => ImportKind::Implicit,
        ImportKind::Explicit => ImportKind::Hidden,
        ImportKind::Hidden => ImportKind::Implicit,
    };

    let import_resolved = queries.resolved(import_file_id)?;

    let terms = import_resolved.exports.iter_terms().map(|(name, file, id)| (name, file, id, kind));
    let types = import_resolved.exports.iter_types().map(|(name, file, id)| (name, file, id, kind));

    add_imported_terms(errors, resolved, indexing_import_id, terms);
    add_imported_types(errors, resolved, indexing_import_id, types);

    if matches!(indexing_import.kind, ImportKind::Implicit) {
        return Ok(());
    }

    for (name, &id) in &indexing_import.terms {
        if let Some((_, _, kind)) = resolved.terms.get_mut(name) {
            *kind = indexing_import.kind;
        } else {
            errors.push(ResolvingError::InvalidImportItem { id });
        }
    }

    for (name, &(id, ref implicit)) in &indexing_import.types {
        if let Some((file, id, kind)) = resolved.types.get_mut(name) {
            *kind = indexing_import.kind;
            let Some(implicit) = implicit else { continue };
            let item = (*file, *id, implicit);
            resolve_implicit(queries, resolved, indexing_import, item)?;
        } else {
            errors.push(ResolvingError::InvalidImportItem { id });
        };
    }

    Ok(())
}

fn resolve_implicit(
    queries: &impl ExternalQueries,
    resolved: &mut ResolvedImport,
    indexing_import: &IndexingImport,
    item: (FileId, TypeItemId, &ImplicitItems),
) -> QueryResult<()> {
    let (f_id, t_id, implicit) = item;
    let import_indexed = queries.indexed(f_id)?;
    match implicit {
        ImplicitItems::Everything => {
            for term_id in import_indexed.pairs.data_constructors(t_id) {
                let item = &import_indexed.items[term_id];
                if matches!(import_indexed.kind, ExportKind::Explicit) && !item.exported {
                    continue;
                }
                let Some(name) = &item.name else {
                    continue;
                };
                if let Some((_, _, term_kind)) = resolved.terms.get_mut(name) {
                    *term_kind = indexing_import.kind;
                }
            }
        }
        ImplicitItems::Enumerated(names) => {
            for name in names {
                if let Some((_, _, term_kind)) = resolved.terms.get_mut(name) {
                    *term_kind = indexing_import.kind;
                }
            }
        }
    }
    Ok(())
}

fn add_imported_terms<'a>(
    errors: &mut Vec<ResolvingError>,
    resolved: &mut ResolvedImport,
    indexing_import_id: ImportId,
    terms: impl Iterator<Item = (&'a SmolStr, FileId, TermItemId, ImportKind)>,
) {
    let (additional, _) = terms.size_hint();
    resolved.terms.reserve(additional);
    terms.for_each(|(name, file, id, kind)| {
        add_imported_term(errors, resolved, indexing_import_id, name, file, id, kind);
    });
}

fn add_imported_types<'a>(
    errors: &mut Vec<ResolvingError>,
    resolved: &mut ResolvedImport,
    indexing_import_id: ImportId,
    terms: impl Iterator<Item = (&'a SmolStr, FileId, TypeItemId, ImportKind)>,
) {
    let (additional, _) = terms.size_hint();
    resolved.terms.reserve(additional);
    terms.for_each(|(name, file, id, kind)| {
        add_imported_type(errors, resolved, indexing_import_id, name, file, id, kind);
    });
}

fn add_imported_term(
    errors: &mut Vec<ResolvingError>,
    resolved: &mut ResolvedImport,
    indexing_import_id: ImportId,
    name: &SmolStr,
    file: FileId,
    id: TermItemId,
    kind: ImportKind,
) {
    if let Some((existing_file, existing_term, _)) = resolved.terms.get(name) {
        let duplicate = (file, id, indexing_import_id);
        let existing = (*existing_file, *existing_term, resolved.id);
        if duplicate != existing {
            errors.push(ResolvingError::TermImportConflict { existing, duplicate });
        }
    } else {
        let name = SmolStr::clone(name);
        resolved.terms.insert(name, (file, id, kind));
    }
}

fn add_imported_type(
    errors: &mut Vec<ResolvingError>,
    resolved: &mut ResolvedImport,
    indexing_import_id: ImportId,
    name: &SmolStr,
    file: FileId,
    id: TypeItemId,
    kind: ImportKind,
) {
    if let Some((existing_file, existing_term, _)) = resolved.types.get(name) {
        let duplicate = (file, id, indexing_import_id);
        let existing = (*existing_file, *existing_term, resolved.id);
        if duplicate != existing {
            errors.push(ResolvingError::TypeImportConflict { existing, duplicate });
        }
    } else {
        let name = SmolStr::clone(name);
        resolved.types.insert(name, (file, id, kind));
    }
}

fn resolve_exports(state: &mut State, indexed: &IndexedModule, file: FileId) {
    export_module_items(state, indexed, file);
    export_module_imports(state, indexed);
}

fn add_local_terms<'k>(
    items: &mut ResolvedLocals,
    errors: &mut Vec<ResolvingError>,
    iterator: impl Iterator<Item = (&'k SmolStr, FileId, TermItemId)>,
) {
    let (additional, _) = iterator.size_hint();
    items.terms.reserve(additional);
    iterator.for_each(move |(name, file, id)| {
        add_local_term(items, errors, name, file, id);
    });
}

fn add_local_term(
    items: &mut ResolvedLocals,
    errors: &mut Vec<ResolvingError>,
    name: &SmolStr,
    file: FileId,
    id: TermItemId,
) {
    if let Some(&existing) = items.terms.get(name) {
        let duplicate = (file, id);
        if existing != duplicate {
            errors.push(ResolvingError::ExistingTerm { existing, duplicate });
        }
    } else {
        let name = SmolStr::clone(name);
        items.terms.insert(name, (file, id));
    }
}

fn add_local_types<'k>(
    items: &mut ResolvedLocals,
    errors: &mut Vec<ResolvingError>,
    iterator: impl Iterator<Item = (&'k SmolStr, FileId, TypeItemId)>,
) {
    let (additional, _) = iterator.size_hint();
    items.types.reserve(additional);
    iterator.for_each(move |(name, file, id)| {
        add_local_type(items, errors, name, file, id);
    });
}

fn add_local_type(
    items: &mut ResolvedLocals,
    errors: &mut Vec<ResolvingError>,
    name: &SmolStr,
    file: FileId,
    id: TypeItemId,
) {
    if let Some(&existing) = items.types.get(name) {
        let duplicate = (file, id);
        if existing != duplicate {
            errors.push(ResolvingError::ExistingType { existing, duplicate });
        }
    } else {
        let name = SmolStr::clone(name);
        items.types.insert(name, (file, id));
    }
}

fn export_module_items(state: &mut State, indexed: &IndexedModule, file: FileId) {
    let local_terms = indexed.items.iter_terms().filter_map(|(id, item)| {
        let name = item.name.as_ref()?;
        Some((name, file, id))
    });

    let local_types = indexed.items.iter_types().filter_map(|(id, item)| {
        let name = item.name.as_ref()?;
        Some((name, file, id))
    });

    add_local_terms(&mut state.locals, &mut state.errors, local_terms);
    add_local_types(&mut state.locals, &mut state.errors, local_types);

    let exported_terms = indexed.items.iter_terms().filter_map(|(id, item)| {
        // Instances cannot be to referred directly by their given name yet.
        // They're simply assumed to exist in a global context for coherence.
        if matches!(item.kind, TermItemKind::Instance { .. } | TermItemKind::Derive { .. }) {
            return None;
        }
        if matches!(indexed.kind, ExportKind::Explicit) && !item.exported {
            return None;
        }
        let name = item.name.as_ref()?;
        Some((name, file, id, ExportSource::Local))
    });

    let exported_types = indexed.items.iter_types().filter_map(|(id, item)| {
        if matches!(indexed.kind, ExportKind::Explicit) && !item.exported {
            return None;
        }
        let name = item.name.as_ref()?;
        Some((name, file, id, ExportSource::Local))
    });

    add_export_terms(&mut state.exports, &mut state.errors, exported_terms);
    add_export_types(&mut state.exports, &mut state.errors, exported_types);
}

fn export_module_imports(state: &mut State, indexed: &IndexedModule) {
    if matches!(indexed.kind, ExportKind::Implicit) {
        return;
    }

    let unqualified = state.unqualified.values().flatten();
    let qualified = state.qualified.values();
    let imports = unqualified.chain(qualified);

    for import in imports {
        if !import.exported {
            continue;
        }
        let source = ExportSource::Import(import.id);
        let terms = import.iter_terms().filter_map(|(k, f, i, d)| {
            if matches!(d, ImportKind::Implicit | ImportKind::Explicit) {
                Some((k, f, i, source))
            } else {
                None
            }
        });
        let types = import.iter_types().filter_map(|(k, f, i, d)| {
            if matches!(d, ImportKind::Implicit | ImportKind::Explicit) {
                Some((k, f, i, source))
            } else {
                None
            }
        });
        add_export_terms(&mut state.exports, &mut state.errors, terms);
        add_export_types(&mut state.exports, &mut state.errors, types);
    }
}

fn add_export_terms<'k>(
    items: &mut ResolvedExports,
    errors: &mut Vec<ResolvingError>,
    iterator: impl Iterator<Item = (&'k SmolStr, FileId, TermItemId, ExportSource)>,
) {
    let (additional, _) = iterator.size_hint();
    items.terms.reserve(additional);
    iterator.for_each(move |(name, file, id, source)| {
        add_export_term(items, errors, name, file, id, source);
    });
}

fn add_export_term(
    items: &mut ResolvedExports,
    errors: &mut Vec<ResolvingError>,
    name: &SmolStr,
    file: FileId,
    id: TermItemId,
    source: ExportSource,
) {
    if let Some(&existing) = items.terms.get(name) {
        let duplicate = (file, id, source);
        if existing != duplicate {
            errors.push(ResolvingError::TermExportConflict { existing, duplicate });
        }
    } else {
        let name = SmolStr::clone(name);
        items.terms.insert(name, (file, id, source));
    }
}

fn add_export_types<'k>(
    items: &mut ResolvedExports,
    errors: &mut Vec<ResolvingError>,
    iterator: impl Iterator<Item = (&'k SmolStr, FileId, TypeItemId, ExportSource)>,
) {
    let (additional, _) = iterator.size_hint();
    items.types.reserve(additional);
    iterator.for_each(move |(name, file, id, source)| {
        add_export_type(items, errors, name, file, id, source);
    });
}

fn add_export_type(
    items: &mut ResolvedExports,
    errors: &mut Vec<ResolvingError>,
    name: &SmolStr,
    file: FileId,
    id: TypeItemId,
    source: ExportSource,
) {
    if let Some(&existing) = items.types.get(name) {
        let duplicate = (file, id, source);
        if existing != duplicate {
            errors.push(ResolvingError::TypeExportConflict { existing, duplicate });
        }
    } else {
        let name = SmolStr::clone(name);
        items.types.insert(name, (file, id, source));
    }
}
