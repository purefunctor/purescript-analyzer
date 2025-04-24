use std::collections::hash_map::Entry;

use la_arena::Idx;
use rustc_hash::FxHashMap;
use smol_str::{SmolStr, SmolStrBuilder};
use syntax::{SyntaxToken, cst};

use crate::{
    ExistingKind, ExportKind, ImplicitItems, ImportKind, IndexingError, IndexingImport,
    IndexingImports, IndexingItems, IndexingPairs, ItemKind, TermItem, TermItemId, TermItemKind,
    TypeItem, TypeItemId, TypeItemKind, source::*,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Current {
    Term(TermItemId),
    Type(TypeItemId),
}

#[derive(Debug, Default, PartialEq, Eq)]
pub(super) struct State {
    name: Option<SmolStr>,
    current: Option<Current>,
    pub(super) kind: ExportKind,
    pub(super) items: IndexingItems,
    pub(super) imports: IndexingImports,
    pub(super) pairs: IndexingPairs,
    pub(super) source: IndexingSource,
    pub(super) errors: Vec<IndexingError>,
}

impl State {
    fn new(name: Option<SmolStr>) -> State {
        State { name, ..Default::default() }
    }

    fn active_term(&mut self, name: &Option<SmolStr>) -> Option<(TermItemId, &mut TermItem)> {
        let Current::Term(id) = self.current? else {
            return None;
        };
        let item = &mut self.items.terms[id];
        if &item.name != name {
            return None;
        }
        Some((id, &mut self.items.terms[id]))
    }

    fn active_type(&mut self, name: &Option<SmolStr>) -> Option<(TypeItemId, &mut TypeItem)> {
        let Current::Type(id) = self.current? else {
            return None;
        };
        let item = &mut self.items.types[id];
        if &item.name != name {
            return None;
        }
        Some((id, &mut self.items.types[id]))
    }

    fn alloc_term(&mut self, item: TermItem) -> TermItemId {
        let id = self.items.terms.alloc(item);
        self.current = Some(Current::Term(id));
        id
    }

    fn alloc_type(&mut self, item: TypeItem) -> TypeItemId {
        let id = self.items.types.alloc(item);
        self.current = Some(Current::Type(id));
        id
    }
}

pub(super) fn index_module(cst: &cst::Module) -> State {
    let name = cst.header().and_then(|cst| {
        let cst = cst.name()?;
        extract_module_name(&cst)
    });

    let mut state = State::new(name);

    if let Some(statements) = cst.statements() {
        for declaration in statements.children() {
            index_declaration(&mut state, &declaration);
        }
    }

    validate_items(&mut state);

    if let Some(imports) = cst.imports() {
        for import in imports.children() {
            index_import(&mut state, &import);
        }
    }

    if let Some(header) = cst.header() {
        if let Some(exports) = header.exports() {
            index_exports(&mut state, &exports);
        }
    }

    state
}

fn index_declaration(state: &mut State, cst: &cst::Declaration) {
    match cst {
        cst::Declaration::ValueSignature(cst) => {
            let id = state.source.allocate_value_signature(cst);
            index_value_signature(state, id, cst);
        }
        cst::Declaration::ValueEquation(cst) => {
            let id = state.source.allocate_value_equation(cst);
            index_value_equation(state, id, cst);
        }
        cst::Declaration::InfixDeclaration(cst) => {
            let id = state.source.allocate_infix(cst);
            index_infix(state, id, cst);
        }
        cst::Declaration::TypeRoleDeclaration(cst) => {
            let id = state.source.allocate_type_role(cst);
            index_type_role(state, id, cst);
        }
        cst::Declaration::InstanceChain(cst) => {
            let c_id = state.source.allocate_chain(cst);
            for cst in cst.instance_declarations() {
                let i_id = state.source.allocate_instance(&cst);
                index_instance(state, i_id, &cst);
                state.pairs.instance_chain.push((c_id, i_id));
                if let Some(cst) = cst.instance_statements() {
                    for cst in cst.children() {
                        let m_id = state.source.allocate_instance_member(&cst);
                        state.pairs.instance_members.push((i_id, m_id));
                    }
                }
            }
        }
        cst::Declaration::TypeSynonymSignature(cst) => {
            let id = state.source.allocate_type_signature(cst);
            let token = cst.name_token();
            index_type_signature(
                state,
                id,
                token,
                |name, id| TypeItem {
                    name,
                    kind: TypeItemKind::Synonym { signature: Some(id), equation: None },
                    exported: false,
                },
                ItemKind::SynonymSignature,
                |item| {
                    if let TypeItemKind::Synonym { signature, .. } = &mut item.kind {
                        Some(signature)
                    } else {
                        None
                    }
                },
            );
        }
        cst::Declaration::TypeSynonymEquation(cst) => {
            let id = state.source.allocate_type_equation(cst);
            let token = cst.name_token();
            let _ = index_type_declaration(
                state,
                id,
                token,
                |name, id| TypeItem {
                    name,
                    kind: TypeItemKind::Synonym { signature: None, equation: Some(id) },
                    exported: false,
                },
                ItemKind::SynonymEquation,
                |item| {
                    if let TypeItemKind::Synonym { equation, .. } = &mut item.kind {
                        Some(equation)
                    } else {
                        None
                    }
                },
            );
        }
        cst::Declaration::ClassSignature(cst) => {
            let id = state.source.allocate_class_signature(cst);
            let token = cst.name_token();
            index_type_signature(
                state,
                id,
                token,
                |name, id| TypeItem {
                    name,
                    kind: TypeItemKind::Class { signature: Some(id), declaration: None },
                    exported: false,
                },
                ItemKind::ClassSignature,
                |item| {
                    if let TypeItemKind::Class { signature, .. } = &mut item.kind {
                        Some(signature)
                    } else {
                        None
                    }
                },
            );
        }
        cst::Declaration::ClassDeclaration(cst) => {
            let id = state.source.allocate_class_declaration(cst);
            let token = cst.class_head().and_then(|cst| cst.name_token());
            let type_id = index_type_declaration(
                state,
                id,
                token,
                |name, id| TypeItem {
                    name,
                    kind: TypeItemKind::Class { signature: None, declaration: Some(id) },
                    exported: false,
                },
                ItemKind::ClassDeclaration,
                |item| {
                    if let TypeItemKind::Class { declaration, .. } = &mut item.kind {
                        Some(declaration)
                    } else {
                        None
                    }
                },
            );
            if let Some(cst) = cst.class_statements() {
                for cst in cst.children() {
                    let id = state.source.allocate_class_member(&cst);
                    let term_id = index_class_member(state, id, &cst);
                    state.pairs.class_members.push((type_id, term_id));
                }
            }
        }
        cst::Declaration::ForeignImportDataDeclaration(cst) => {
            let id = state.source.allocate_foreign_data(cst);
            index_foreign_data(state, id, cst);
        }
        cst::Declaration::ForeignImportValueDeclaration(cst) => {
            let id = state.source.allocate_foreign_value(cst);
            index_foreign_value(state, id, cst);
        }
        cst::Declaration::NewtypeSignature(cst) => {
            let id = state.source.allocate_newtype_signature(cst);
            let token = cst.name_token();
            index_type_signature(
                state,
                id,
                token,
                |name, id| TypeItem {
                    name,
                    kind: TypeItemKind::Newtype { signature: Some(id), equation: None, role: None },
                    exported: false,
                },
                ItemKind::NewtypeSignature,
                |item| {
                    if let TypeItemKind::Newtype { signature, .. } = &mut item.kind {
                        Some(signature)
                    } else {
                        None
                    }
                },
            );
        }
        cst::Declaration::NewtypeEquation(cst) => {
            let id = state.source.allocate_newtype_equation(cst);
            let token = cst.name_token();
            let type_id = index_type_declaration(
                state,
                id,
                token,
                |name, id| TypeItem {
                    name,
                    kind: TypeItemKind::Newtype { signature: None, equation: Some(id), role: None },
                    exported: false,
                },
                ItemKind::NewtypeEquation,
                |item| {
                    if let TypeItemKind::Newtype { equation, .. } = &mut item.kind {
                        Some(equation)
                    } else {
                        None
                    }
                },
            );
            for cst in cst.data_constructors() {
                let id = state.source.allocate_data_constructor(&cst);
                let term_id = index_data_constructor(state, id, &cst);
                state.pairs.data_constructors.push((type_id, term_id));
            }
        }
        cst::Declaration::DataSignature(cst) => {
            let id = state.source.allocate_data_signature(cst);
            let token = cst.name_token();
            index_type_signature(
                state,
                id,
                token,
                |name, id| TypeItem {
                    name,
                    kind: TypeItemKind::Data { signature: Some(id), equation: None, role: None },
                    exported: false,
                },
                ItemKind::DataSignature,
                |item| {
                    if let TypeItemKind::Data { signature, .. } = &mut item.kind {
                        Some(signature)
                    } else {
                        None
                    }
                },
            );
        }
        cst::Declaration::DataEquation(cst) => {
            let id = state.source.allocate_data_equation(cst);
            let token = cst.name_token();
            let type_id = index_type_declaration(
                state,
                id,
                token,
                |name, id| TypeItem {
                    name,
                    kind: TypeItemKind::Data { signature: None, equation: Some(id), role: None },
                    exported: false,
                },
                ItemKind::DataEquation,
                |item| {
                    if let TypeItemKind::Data { equation, .. } = &mut item.kind {
                        Some(equation)
                    } else {
                        None
                    }
                },
            );
            for cst in cst.data_constructors() {
                let id = state.source.allocate_data_constructor(&cst);
                let term_id = index_data_constructor(state, id, &cst);
                state.pairs.data_constructors.push((type_id, term_id));
            }
        }
        cst::Declaration::DeriveDeclaration(cst) => {
            let id = state.source.allocate_derive(cst);
            index_derive(state, id, cst);
        }
    }
}

fn index_value_signature(state: &mut State, id: ValueSignatureId, cst: &cst::ValueSignature) {
    let name = cst.name_token().map(|t| {
        let text = t.text();
        SmolStr::from(text)
    });

    let Some((active_id, active)) = state.active_term(&name) else {
        state.alloc_term(TermItem {
            name,
            kind: TermItemKind::Value { signature: Some(id), equations: vec![] },
            exported: false,
        });
        return;
    };

    if let TermItemKind::Value { signature, .. } = &mut active.kind {
        if signature.is_some() {
            let kind = ItemKind::ValueSignature(id);
            let existing = ExistingKind::Term(active_id);
            state.errors.push(IndexingError::DuplicateItem { kind, existing });
        } else {
            *signature = Some(id);
        }
    } else {
        let kind = ItemKind::ValueSignature(id);
        let existing = ExistingKind::Term(active_id);
        state.errors.push(IndexingError::MismatchedItem { kind, existing });
    }
}

fn index_value_equation(state: &mut State, id: ValueEquationId, cst: &cst::ValueEquation) {
    let name = cst.name_token().map(|t| {
        let text = t.text();
        SmolStr::from(text)
    });

    let Some((active_id, active)) = state.active_term(&name) else {
        state.alloc_term(TermItem {
            name,
            kind: TermItemKind::Value { signature: None, equations: vec![id] },
            exported: false,
        });
        return;
    };

    if let TermItemKind::Value { equations, .. } = &mut active.kind {
        equations.push(id);
    } else {
        let kind = ItemKind::ValueEquation(id);
        let existing = ExistingKind::Term(active_id);
        state.errors.push(IndexingError::MismatchedItem { kind, existing });
    }
}

fn index_infix(state: &mut State, id: InfixId, cst: &cst::InfixDeclaration) {
    let type_token = cst.type_token();
    let operator_token = cst.operator_token();

    let name = operator_token.map(|t| {
        let text = t.text();
        SmolStr::from(text)
    });

    if type_token.is_some() {
        let item = TypeItem { name, kind: TypeItemKind::Operator { id }, exported: false };
        state.alloc_type(item);
    } else {
        let item = TermItem { name, kind: TermItemKind::Operator { id }, exported: false };
        state.alloc_term(item);
    }
}

fn index_type_role(state: &mut State, id: TypeRoleId, cst: &cst::TypeRoleDeclaration) {
    let name = cst.name_token().map(|t| {
        let text = t.text();
        SmolStr::from(text)
    });

    let Some((active_id, active)) = state.active_type(&name) else {
        return state.errors.push(IndexingError::InvalidRole { id, existing: None });
    };

    if let TypeItemKind::Data { role, .. } | TypeItemKind::Newtype { role, .. } = &mut active.kind {
        if role.is_some() {
            state.errors.push(IndexingError::InvalidRole { id, existing: Some(active_id) });
        } else {
            *role = Some(id)
        }
    } else {
        state.errors.push(IndexingError::InvalidRole { id, existing: None });
    }
}

type Item<T> = fn(Option<SmolStr>, Idx<T>) -> TypeItem;
type Extract<T> = fn(&mut TypeItem) -> Option<&mut Option<Idx<T>>>;
type MakeItemKind<T> = fn(Idx<T>) -> ItemKind;

fn index_type_signature<T>(
    state: &mut State,
    id: Idx<T>,
    token: Option<SyntaxToken>,
    item: Item<T>,
    kind: MakeItemKind<T>,
    extract: Extract<T>,
) -> TypeItemId {
    let name = token.map(|t| {
        let text = t.text();
        SmolStr::from(text)
    });

    let Some((active_id, active)) = state.active_type(&name) else {
        return state.alloc_type(item(name, id));
    };

    let Some(signature) = extract(active) else {
        let kind = kind(id);
        let existing = ExistingKind::Type(active_id);
        state.errors.push(IndexingError::MismatchedItem { kind, existing });
        return active_id;
    };

    if signature.is_some() {
        let kind = kind(id);
        let existing = ExistingKind::Type(active_id);
        state.errors.push(IndexingError::DuplicateItem { kind, existing });
    } else {
        *signature = Some(id)
    }

    active_id
}

fn index_type_declaration<T>(
    state: &mut State,
    id: Idx<T>,
    token: Option<SyntaxToken>,
    item: Item<T>,
    kind: MakeItemKind<T>,
    extract: Extract<T>,
) -> TypeItemId {
    let name = token.map(|t| {
        let text = t.text();
        SmolStr::from(text)
    });

    let Some((active_id, active)) = state.active_type(&name) else {
        return state.alloc_type(item(name, id));
    };

    let Some(equation) = extract(active) else {
        let kind = kind(id);
        let existing = ExistingKind::Type(active_id);
        state.errors.push(IndexingError::MismatchedItem { kind, existing });
        return active_id;
    };

    if equation.is_some() {
        let kind = kind(id);
        let existing = ExistingKind::Type(active_id);
        state.errors.push(IndexingError::DuplicateItem { kind, existing });
    } else {
        *equation = Some(id)
    }

    active_id
}

fn index_data_constructor(
    state: &mut State,
    id: DataConstructorId,
    cst: &cst::DataConstructor,
) -> TermItemId {
    let name = cst.name_token().map(|t| {
        let text = t.text();
        SmolStr::from(text)
    });
    state.alloc_term(TermItem { name, kind: TermItemKind::Constructor { id }, exported: false })
}

fn index_class_member(
    state: &mut State,
    id: ClassMemberId,
    cst: &cst::ClassMemberStatement,
) -> TermItemId {
    let name = cst.name_token().map(|t| {
        let text = t.text();
        SmolStr::from(text)
    });
    state.alloc_term(TermItem { name, kind: TermItemKind::ClassMember { id }, exported: false })
}

fn index_foreign_data(
    state: &mut State,
    id: ForeignDataId,
    cst: &cst::ForeignImportDataDeclaration,
) {
    let name = cst.name_token().map(|t| {
        let text = t.text();
        SmolStr::from(text)
    });
    state.alloc_type(TypeItem { name, kind: TypeItemKind::Foreign { id }, exported: false });
}

fn index_foreign_value(
    state: &mut State,
    id: ForeignValueId,
    cst: &cst::ForeignImportValueDeclaration,
) {
    let name = cst.name_token().map(|t| {
        let text = t.text();
        SmolStr::from(text)
    });
    state.alloc_term(TermItem { name, kind: TermItemKind::Foreign { id }, exported: false });
}

fn index_instance(state: &mut State, id: InstanceId, cst: &cst::InstanceDeclaration) {
    let name = cst.instance_name().and_then(|n| {
        let token = n.name_token()?;
        let text = token.text();
        Some(SmolStr::from(text))
    });
    state.alloc_term(TermItem { name, kind: TermItemKind::Instance { id }, exported: true });
}

fn index_derive(state: &mut State, id: DeriveId, cst: &cst::DeriveDeclaration) {
    let name = cst.instance_name().and_then(|n| {
        let token = n.name_token()?;
        let text = token.text();
        Some(SmolStr::from(text))
    });
    state.alloc_term(TermItem { name, kind: TermItemKind::Derive { id }, exported: true });
}

fn validate_items(state: &mut State) {
    let mut terms = FxHashMap::default();
    for (id, item) in state.items.terms.iter() {
        let Some(name) = &item.name else { continue };
        match terms.entry(name) {
            Entry::Occupied(o) => {
                let kind = ItemKind::Term(id);
                let id = *o.get();
                let existing = ExistingKind::Term(id);
                state.errors.push(IndexingError::DuplicateItem { kind, existing });
            }
            Entry::Vacant(v) => {
                v.insert(id);
            }
        }
    }

    let mut types = FxHashMap::default();
    for (id, item) in state.items.types.iter() {
        let Some(name) = &item.name else { continue };
        match types.entry(name) {
            Entry::Occupied(o) => {
                let kind = ItemKind::Type(id);
                let id = *o.get();
                let existing = ExistingKind::Type(id);
                state.errors.push(IndexingError::DuplicateItem { kind, existing });
            }
            Entry::Vacant(v) => {
                v.insert(id);
            }
        }
    }
}

// Imports

fn index_import(state: &mut State, cst: &cst::ImportStatement) {
    let id = state.source.allocate_import_statement(cst);

    let name = extract_name(cst);
    let alias = extract_alias(cst);

    let mut import = IndexingImport::new(name, alias);

    if let Some(cst) = cst.import_list() {
        if cst.hiding().is_some() {
            import.kind = ImportKind::Hidden;
        } else {
            import.kind = ImportKind::Explicit;
        }
        for cst in cst.children() {
            index_import_items(state, &mut import, &cst);
        }
    }

    state.imports.insert(id, import);
}

fn index_import_items(state: &mut State, import: &mut IndexingImport, cst: &cst::ImportItem) {
    let id = state.source.allocate_import(cst);
    match cst {
        cst::ImportItem::ImportValue(v) => {
            let Some(token) = v.name_token() else { return };
            let name = token.text();
            index_term_import(state, import, name, id);
        }
        cst::ImportItem::ImportClass(c) => {
            let Some(token) = c.name_token() else { return };
            let name = token.text();
            index_type_import(state, import, name, id, None);
        }
        cst::ImportItem::ImportType(t) => {
            let Some(token) = t.name_token() else { return };
            let name = token.text();
            index_type_import(state, import, name, id, t.type_items());
        }
        cst::ImportItem::ImportOperator(o) => {
            let Some(token) = o.name_token() else { return };
            let name = token.text();
            index_term_import(state, import, name, id);
        }
        cst::ImportItem::ImportTypeOperator(o) => {
            let Some(token) = o.name_token() else { return };
            let name = token.text();
            index_type_import(state, import, name, id, None);
        }
    }
}

fn index_term_import(state: &mut State, import: &mut IndexingImport, name: &str, id: ImportItemId) {
    if let Some(&existing) = import.terms.get(name) {
        state.errors.push(IndexingError::DuplicateImport { id, existing });
    } else {
        let name = SmolStr::from(name);
        import.terms.insert(name, id);
    }
}

fn index_type_import(
    state: &mut State,
    import: &mut IndexingImport,
    name: &str,
    id: ImportItemId,
    items: Option<cst::TypeItems>,
) {
    let items = items.map(|items| index_type_items(state, import, id, items));
    if let Some((existing, _)) = import.types.get(name) {
        let existing = *existing;
        state.errors.push(IndexingError::DuplicateImport { id, existing });
    } else {
        let name = SmolStr::from(name);
        import.types.insert(name, (id, items));
    }
}

fn index_type_items(
    state: &mut State,
    import: &mut IndexingImport,
    id: ImportItemId,
    items: cst::TypeItems,
) -> ImplicitItems {
    match items {
        cst::TypeItems::TypeItemsAll(_) => ImplicitItems::Everything,
        cst::TypeItems::TypeItemsList(cst) => {
            let enumerated = cst.name_tokens().map(|token| {
                let name = token.text();
                index_term_import(state, import, name, id);
                SmolStr::from(name)
            });
            let enumerated = enumerated.collect();
            ImplicitItems::Enumerated(enumerated)
        }
    }
}

fn extract_module_name(cst: &cst::ModuleName) -> Option<SmolStr> {
    let mut buffer = SmolStrBuilder::default();
    if let Some(token) = cst.qualifier().and_then(|cst| cst.text()) {
        buffer.push_str(token.text());
    }

    let token = cst.name_token()?;
    buffer.push_str(token.text());

    Some(buffer.finish())
}

fn extract_name(cst: &cst::ImportStatement) -> Option<SmolStr> {
    let cst = cst.module_name()?;
    extract_module_name(&cst)
}

fn extract_alias(cst: &cst::ImportStatement) -> Option<SmolStr> {
    let cst = cst.import_alias()?;
    let cst = cst.module_name()?;
    extract_module_name(&cst)
}

// Exports

fn index_exports(state: &mut State, cst: &cst::ExportList) {
    let mut terms = FxHashMap::default();
    let mut types = FxHashMap::default();

    let mut add_term = |e: &mut Vec<IndexingError>, n: &str, id: ExportItemId| {
        let name = SmolStr::from(n);
        match terms.entry(name) {
            Entry::Occupied(o) => {
                let existing = *o.get();
                e.push(IndexingError::DuplicateExport { id, existing });
            }
            Entry::Vacant(v) => {
                v.insert(id);
            }
        }
    };

    let mut add_type =
        |e: &mut Vec<IndexingError>, n: &str, id: ExportItemId, cst: Option<cst::TypeItems>| {
            let name = SmolStr::from(n);
            match types.entry(name) {
                Entry::Occupied(o) => {
                    let (existing, _) = *o.get();
                    e.push(IndexingError::DuplicateExport { id, existing });
                }
                Entry::Vacant(v) => {
                    let items = cst.map(|cst| match cst {
                        cst::TypeItems::TypeItemsAll(_) => ImplicitItems::Everything,
                        cst::TypeItems::TypeItemsList(cst) => {
                            let enumerated = cst.name_tokens().map(|token| {
                                let name = token.text();
                                SmolStr::from(name)
                            });
                            let enumerated = enumerated.collect();
                            ImplicitItems::Enumerated(enumerated)
                        }
                    });
                    v.insert((id, items));
                }
            }
        };

    for cst in cst.children() {
        let id = state.source.allocate_export(&cst);
        match cst {
            cst::ExportItem::ExportValue(cst) => {
                let Some(name) = cst.name_token() else { continue };
                let name = name.text();
                add_term(&mut state.errors, name, id);
            }
            cst::ExportItem::ExportClass(cst) => {
                let Some(name) = cst.name_token() else { continue };
                let name = name.text();
                add_type(&mut state.errors, name, id, None);
            }
            cst::ExportItem::ExportType(cst) => {
                let Some(name) = cst.name_token() else { continue };
                let name = name.text();
                let items = cst.type_items();
                add_type(&mut state.errors, name, id, items);
            }
            cst::ExportItem::ExportOperator(cst) => {
                let Some(name) = cst.name_token() else { continue };
                let name = name.text();
                let name = operator_name(name);
                add_term(&mut state.errors, name, id);
            }
            cst::ExportItem::ExportTypeOperator(cst) => {
                let Some(name) = cst.name_token() else { continue };
                let name = name.text();
                let name = operator_name(name);
                add_type(&mut state.errors, name, id, None);
            }
            cst::ExportItem::ExportModule(cst) => {
                index_module_export(state, &cst);
            }
        }
    }

    for (type_id, item) in state.items.types.iter_mut() {
        let Some(name) = &item.name else { continue };
        if let Some((id, implicit)) = types.get(name) {
            item.exported = true;
            if let Some(implicit) = implicit {
                match implicit {
                    ImplicitItems::Everything => {
                        for term_id in state.pairs.data_constructors(type_id) {
                            state.items.terms[term_id].exported = true;
                        }
                    }
                    ImplicitItems::Enumerated(names) => {
                        for name in names {
                            add_term(&mut state.errors, name, *id);
                        }
                    }
                }
            }
            for term_id in state.pairs.class_members(type_id) {
                state.items.terms[term_id].exported = true;
            }
        }
    }

    for (_, item) in state.items.terms.iter_mut() {
        let Some(name) = &item.name else { continue };
        item.exported = item.exported || terms.contains_key(name);
    }
}

fn operator_name(name: &str) -> &str {
    name.trim_start_matches("(").trim_end_matches(")")
}

fn index_module_export(state: &mut State, cst: &cst::ExportModule) {
    if let Some(n) = extracted_exported_module(cst) {
        if state.name.as_ref() == Some(&n) {
            state.kind = ExportKind::ExplicitSelf;
        } else {
            // PureScript supports the following export forms:
            //
            // 1. Using the alias:
            //
            // ```purescript
            // module Main (module Maybe) where
            //
            // import Data.Maybe as Maybe
            // ```
            //
            // 2. Using the name:
            //
            // ```purescript
            // module Main (module Data.Maybe) where
            //
            // import Data.Maybe (isJust)
            // ```
            //
            // Modules can only be exported using its full name if it's not aliased.
            // As a result, the following export form is invalid:
            //
            // ```purescript
            // module Main (module Data.Maybe) where
            //
            // import Data.Maybe as Maybe
            // ```
            for (_, items) in state.imports.iter_mut() {
                let alias = items.alias.as_deref();
                let name = items.name.as_deref();

                let using_alias = alias == Some(&n);
                let using_name = alias.is_none() && name == Some(&n);

                if using_alias || using_name {
                    items.exported = true;
                }
            }
        }
    }
}

fn extracted_exported_module(cst: &cst::ExportModule) -> Option<SmolStr> {
    let module_name = cst.module_name()?;

    let mut buffer = SmolStrBuilder::default();
    if let Some(qualifier) = module_name.qualifier() {
        if let Some(token) = qualifier.text() {
            buffer.push_str(token.text());
        }
    }

    let token = module_name.name_token()?;
    buffer.push_str(token.text());

    Some(buffer.finish())
}
