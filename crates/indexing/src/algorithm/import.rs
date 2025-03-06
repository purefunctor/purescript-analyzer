use rustc_hash::FxHashMap;
use smol_str::{SmolStr, SmolStrBuilder};
use syntax::cst;

use crate::{ImportItemId, ImportedItems, ImportedTerms, ImportedTypes, IndexError, TypeItems};

use super::State;

pub(super) fn index(state: &mut State, cst: &cst::ImportStatement) {
    let id = state.source.allocate_import_statement(cst);

    if let Some(imports) = cst.import_list() {
        let mut terms = FxHashMap::default();
        let mut types = FxHashMap::default();
        for import in imports.children() {
            index_import(state, &mut terms, &mut types, &import);
        }
        let imported_items = ImportedItems { terms, types };
        state.index.insert_imported_items(id, imported_items);
    }

    if let Some(alias) = extract_alias(cst) {
        state.index.insert_import_alias(alias, id);
    }
}

/// Qualifier -> Alias -> ImportId -> FxHashMap<SmolStr, ImportItemId>
/// Qualifier -> Alias -> FxHashMap<(SmolStr, ImportId), ImportItemId>
/// FxHashMap<ImportId, FxHashMap<SmolStr, ImportItemId>>
///
/// What about constructors? How do we verify them?
/// What if instead of consulting the import list first we consulted
/// the export list, since it has an interface for searching items
/// nominally. Then, we should be able to get the type id of the
/// constructor, which we can check against the import list to verify
/// if the import item actually imports the thing
///
/// For an import statement, we want an interface to verify that an item is imported
/// Likewise, for an import statement, we want an interface to verify that an implicit
/// item is actually imported as well. There's no direct way to do this aside from consulting
/// the export list first and then checking the import list. So, what scheme makes sense?
///
/// During resolution for qualified names, we obtain the import id for that specific alias.
/// Which would give us the module name and subsequently the file id for that given module,
/// and finally the index.
///
/// Given the name, we get the term/type item and check if it's exported. Finally, we verify that
/// it was actually imported in the import list, emitting an error if it wasn't.
///
/// During resolution for unqualified names, we iterate through all unqualified imports in the
/// current module. Given current unqualified module, we obtain the file id from the module name,
/// and then get the index for that module.
///
/// Given the name, we get the term/type item and check if it's exported, Finally, we verify that
/// it was actually imported in the import list, emitting an error if it wasn't.
///
/// Note that for this algorithm in particular, we mention resolving module names to file ids,
/// which would come from an external source, particularly the component in our build system that
/// handles files. For now, what we could do instead is to use an interface to fake this, similar
/// to what we're doing with InlineStorage in the checking algorithm.
///
/// I think that for this workflow of checking the export items first before the import statement,
/// (ImportId, SmolStr) -> ImportItemId, should suffice for the implementation.
///
/// The tricky part is handling constructors? The export list would tell us that the constructor
/// was exported just fine, because we treat it as a term item. But to verify if it was actually
/// imported, we need to get the name of the data type that holds it, and only then can we verify
/// in the import list if it was imported or not.
///
/// What if we iterated through all the types in the import list first, then marked any
/// constructors that we know were imported. For example, if I had the import list:
///
/// import Data.Maybe (Maybe(..), Invalid(..))
///
/// I would know that Maybe, Just, and Nothing is in scope, but not Invalid.
/// Then, I could simply introduce these names into a flat mapping and verify off of that.
/// This requires an intermediary structure, but reduces the need for linear search against the
/// SmolStr => TypeItemId mapping
///
/// What if we didn't have an import list? Then it gets a little tricky because we can't just build
/// the flat mapping for a given module, because it might be too big? It'll also be too redundant
/// given the export list.
///
/// We effectively get the following matrix for name resolution
///
/// Unqualified
/// Qualified
///
/// No Import List
/// Has Import List
/// Has Exclude List
///
/// Let's focus on qualified first.
///
/// 1. No import list, simply consult the associated Index for the imported module, checking if the
///    item was actually exported
/// 2. Has import list, iterate through the import list to discover implicit items, check if the
///    item was exported, and if it was imported
/// 3. Has exclude list, iterate through the import list to discover implicit items, check if the
///    item was exported, and if it's not imported
///
/// We can fold the other matrix to be:
///
/// No Import List
/// Has Import List + Excluding
///
/// We simply take 2 and 3 and apply a not condition based on whether or not the excluding flag is
/// on.
///
/// We may do the same for unqualified imports, but would it be too slow? Especially if we had to
/// discover implicit items for each name that needs to be resolved. We could always discover
/// implicit items in advance of course, which means only having to iterate over the Indexes of
/// unqualified imports per name being resolved. Is it slow? Honestly yes. Could be potentially do
/// better? Yes, if we're willing to use commit a bit more memory, which I think should be fine
/// because it's an ephemeral structure anyways.
///
/// Cool, that said, what structure should we choose for lowering import items? We know that
/// FxHashMap<SmolStr, ImportItemId> is what we need, and we obviously need two separate domains
/// for them.

fn index_import(
    state: &mut State,
    terms: &mut ImportedTerms,
    types: &mut ImportedTypes,
    cst: &cst::ImportItem,
) {
    let id = state.source.allocate_import(cst);
    match cst {
        cst::ImportItem::ImportValue(v) => {
            let Some(token) = v.name_token() else { return };
            let name = token.text();
            index_term_import(state, terms, name, id);
        }
        cst::ImportItem::ImportClass(c) => {
            let Some(token) = c.name_token() else { return };
            let name = token.text();
            index_type_import(state, types, name, id, None);
        }
        cst::ImportItem::ImportType(t) => {
            let Some(token) = t.name_token() else { return };
            let name = token.text();
            index_type_import(state, types, name, id, t.type_items());
        }
        cst::ImportItem::ImportOperator(o) => {
            let Some(token) = o.name_token() else { return };
            let name = token.text();
            index_term_import(state, terms, name, id);
        }
        cst::ImportItem::ImportTypeOperator(o) => {
            let Some(token) = o.name_token() else { return };
            let name = token.text();
            index_type_import(state, types, name, id, None);
        }
    }
}

fn index_term_import(state: &mut State, terms: &mut ImportedTerms, name: &str, id: ImportItemId) {
    if let Some(&existing) = terms.get(name) {
        state.error.push(IndexError::DuplicateImport { id, existing });
    } else {
        let name = SmolStr::from(name);
        terms.insert(name, id);
    }
}

fn index_type_import(
    state: &mut State,
    types: &mut ImportedTypes,
    name: &str,
    id: ImportItemId,
    items: Option<cst::TypeItems>,
) {
    let items = items.map(index_type_items);
    if let Some((existing, _)) = types.get(name) {
        let existing = *existing;
        state.error.push(IndexError::DuplicateImport { id, existing });
    } else {
        let name = SmolStr::from(name);
        types.insert(name, (id, items));
    }
}

fn index_type_items(items: cst::TypeItems) -> TypeItems {
    match items {
        cst::TypeItems::TypeItemsAll(_) => TypeItems::All,
        cst::TypeItems::TypeItemsList(cst) => {
            TypeItems::List(cst.name_tokens().map(|token| SmolStr::from(token.text())).collect())
        }
    }
}

fn extract_alias(cst: &cst::ImportStatement) -> Option<SmolStr> {
    let cst = cst.import_alias()?;
    let cst = cst.module_name()?;

    let mut buffer = SmolStrBuilder::default();
    if let Some(token) = cst.qualifier().and_then(|cst| cst.text()) {
        buffer.push_str(token.text());
    }

    let token = cst.name_token()?;
    buffer.push_str(token.text());

    Some(buffer.finish())
}
