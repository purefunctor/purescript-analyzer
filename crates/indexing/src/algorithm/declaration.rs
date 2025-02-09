use la_arena::Idx;
use smol_str::SmolStr;
use syntax::{cst, SyntaxToken};

use crate::{
    ClassMemberId, DataConstructorId, DeriveId, ExistingKind, ForeignDataId, ForeignValueId,
    IndexError, InfixId, InstanceId, ItemKind, TermItem, TermItemId, TypeItem, TypeItemId,
    TypeRoleId, ValueEquationId, ValueSignatureId,
};

use super::State;

pub(super) fn index(state: &mut State, declaration: &cst::Declaration) {
    match declaration {
        cst::Declaration::ValueSignature(s) => {
            let id = state.source.allocate_value_signature(s);
            index_value_signature(state, id, s);
        }
        cst::Declaration::ValueEquation(e) => {
            let id = state.source.allocate_value_equation(e);
            index_value_equation(state, id, e);
        }
        cst::Declaration::InfixDeclaration(i) => {
            let id = state.source.allocate_infix(i);
            index_infix(state, id, i);
        }
        cst::Declaration::TypeRoleDeclaration(r) => {
            let id = state.source.allocate_type_role(r);
            index_type_role(state, id, r);
        }
        cst::Declaration::InstanceChain(c) => {
            let c_id = state.source.allocate_chain(c);
            for i in c.instance_declarations() {
                let i_id = state.source.allocate_instance(&i);
                index_instance(state, i_id, &i);
                state.relational.insert_chain_relation(c_id, i_id);
                if let Some(s) = i.instance_statements() {
                    for m in s.children() {
                        let m_id = state.source.allocate_instance_member(&m);
                        state.relational.insert_instance_relation(i_id, m_id);
                    }
                }
            }
        }
        cst::Declaration::TypeSynonymSignature(s) => {
            let id = state.source.allocate_type_signature(s);
            let token = s.name_token();
            index_type_signature(
                state,
                id,
                token,
                |id| TypeItem::Synonym { signature: Some(id), equation: None },
                ItemKind::SynonymSignature,
                |item| {
                    if let TypeItem::Synonym { signature, .. } = item {
                        Some(signature)
                    } else {
                        None
                    }
                },
            );
        }
        cst::Declaration::TypeSynonymEquation(e) => {
            let id = state.source.allocate_type_equation(e);
            let token = e.name_token();
            index_type_declaration(
                state,
                id,
                token,
                |id| TypeItem::Synonym { signature: None, equation: Some(id) },
                ItemKind::SynonymEquation,
                |item| {
                    if let TypeItem::Synonym { equation, .. } = item {
                        Some(equation)
                    } else {
                        None
                    }
                },
            );
        }
        cst::Declaration::ClassSignature(s) => {
            let id = state.source.allocate_class_signature(s);
            let token = s.name_token();
            index_type_signature(
                state,
                id,
                token,
                |id| TypeItem::Class { signature: Some(id), declaration: None },
                ItemKind::ClassSignature,
                |item| {
                    if let TypeItem::Class { signature, .. } = item {
                        Some(signature)
                    } else {
                        None
                    }
                },
            );
        }
        cst::Declaration::ClassDeclaration(d) => {
            let id = state.source.allocate_class_declaration(d);
            let token = d.class_head().and_then(|h| h.name_token());

            let type_id = index_type_declaration(
                state,
                id,
                token,
                |id| TypeItem::Class { signature: None, declaration: Some(id) },
                ItemKind::ClassDeclaration,
                |item| {
                    if let TypeItem::Class { declaration, .. } = item {
                        Some(declaration)
                    } else {
                        None
                    }
                },
            );

            if let Some(s) = d.class_statements() {
                for c in s.children() {
                    let id = state.source.allocate_class_member(&c);
                    let term_id = index_class_member(state, id, &c);
                    state.relational.insert_class_relation(type_id, term_id);
                }
            }
        }
        cst::Declaration::ForeignImportDataDeclaration(d) => {
            let id = state.source.allocate_foreign_data(d);
            index_foreign_data(state, id, d);
        }
        cst::Declaration::ForeignImportValueDeclaration(v) => {
            let id = state.source.allocate_foreign_value(v);
            index_foreign_value(state, id, v);
        }
        cst::Declaration::NewtypeSignature(s) => {
            let id = state.source.allocate_newtype_signature(s);
            let token = s.name_token();
            index_type_signature(
                state,
                id,
                token,
                |id| TypeItem::Newtype { signature: Some(id), equation: None, role: None },
                ItemKind::NewtypeSignature,
                |item| {
                    if let TypeItem::Newtype { signature, .. } = item {
                        Some(signature)
                    } else {
                        None
                    }
                },
            );
        }
        cst::Declaration::NewtypeEquation(e) => {
            let id = state.source.allocate_newtype_equation(e);
            let token = e.name_token();

            let type_id = index_type_declaration(
                state,
                id,
                token,
                |id| TypeItem::Newtype { signature: None, equation: Some(id), role: None },
                ItemKind::NewtypeEquation,
                |item| {
                    if let TypeItem::Newtype { equation, .. } = item {
                        Some(equation)
                    } else {
                        None
                    }
                },
            );

            for c in e.data_constructors() {
                let id = state.source.allocate_data_constructor(&c);
                let term_id = index_data_constructor(state, id, &c);
                state.relational.insert_data_relation(type_id, term_id);
            }
        }
        cst::Declaration::DataSignature(s) => {
            let id = state.source.allocate_data_signature(s);
            let token = s.name_token();
            index_type_signature(
                state,
                id,
                token,
                |id| TypeItem::Data { signature: Some(id), equation: None, role: None },
                ItemKind::DataSignature,
                |item| {
                    if let TypeItem::Data { signature, .. } = item {
                        Some(signature)
                    } else {
                        None
                    }
                },
            );
        }
        cst::Declaration::DataEquation(e) => {
            let id = state.source.allocate_data_equation(e);
            let token = e.name_token();

            let item_id = index_type_declaration(
                state,
                id,
                token,
                |id| TypeItem::Data { signature: None, equation: Some(id), role: None },
                ItemKind::DataEquation,
                |item| {
                    if let TypeItem::Data { equation, .. } = item {
                        Some(equation)
                    } else {
                        None
                    }
                },
            );

            for c in e.data_constructors() {
                let id = state.source.allocate_data_constructor(&c);
                let term_id = index_data_constructor(state, id, &c);
                state.relational.insert_data_relation(item_id, term_id);
            }
        }
        cst::Declaration::DeriveDeclaration(d) => {
            let id = state.source.allocate_derive(d);
            index_derive(state, id, d);
        }
    }
}

type Item<T> = fn(Idx<T>) -> TypeItem;
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

    let Some(name) = name else {
        return state.index.insert_type_item(None, item(id));
    };

    let Some((item, existing)) = state.index.type_item_mut(&name) else {
        return state.index.insert_type_item(Some(name), item(id));
    };

    let Some(signature) = extract(item) else {
        state.error.push(IndexError::MismatchedItem {
            kind: kind(id),
            existing: ExistingKind::Type(existing),
        });
        return existing;
    };

    if signature.is_some() {
        state.error.push(IndexError::DuplicateItem {
            kind: kind(id),
            existing: ExistingKind::Type(existing),
        });
    } else {
        *signature = Some(id)
    }

    existing
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

    let Some(name) = name else {
        return state.index.insert_type_item(None, item(id));
    };

    let Some((item, existing)) = state.index.type_item_mut(&name) else {
        return state.index.insert_type_item(Some(name), item(id));
    };

    let Some(equation) = extract(item) else {
        state.error.push(IndexError::MismatchedItem {
            kind: kind(id),
            existing: ExistingKind::Type(existing),
        });
        return existing;
    };

    if equation.is_some() {
        state.error.push(IndexError::DuplicateItem {
            kind: kind(id),
            existing: ExistingKind::Type(existing),
        });
    } else {
        *equation = Some(id)
    }

    existing
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

    let item = TypeItem::Foreign { id };

    let Some(name) = name else {
        state.index.insert_type_item(None, item);
        return;
    };

    let Some((_, existing)) = state.index.type_item_mut(&name) else {
        state.index.insert_type_item(Some(name), item);
        return;
    };

    let kind = ItemKind::ForeignData(id);
    let existing = ExistingKind::Type(existing);
    state.error.push(IndexError::DuplicateItem { kind, existing });
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

    let item = TermItem::Foreign { id };

    let Some(name) = name else {
        state.index.insert_term_item(None, item);
        return;
    };

    let Some((_, existing)) = state.index.term_item_mut(&name) else {
        state.index.insert_term_item(Some(name), item);
        return;
    };

    let kind = ItemKind::ForeignValue(id);
    let existing = ExistingKind::Term(existing);
    state.error.push(IndexError::DuplicateItem { kind, existing });
}

fn index_value_signature(state: &mut State, id: ValueSignatureId, cst: &cst::ValueSignature) {
    let name = cst.name_token().map(|t| {
        let text = t.text();
        SmolStr::from(text)
    });

    let item = TermItem::Value { signature: Some(id), equations: vec![] };

    let Some(name) = name else {
        state.index.insert_term_item(None, item);
        return;
    };

    let Some((item, existing)) = state.index.term_item_mut(&name) else {
        state.index.insert_term_item(Some(name), item);
        return;
    };

    let kind = ItemKind::ValueSignature(id);
    let existing = ExistingKind::Term(existing);
    let TermItem::Value { signature, .. } = item else {
        return state.error.push(IndexError::MismatchedItem { kind, existing });
    };

    if signature.is_some() {
        state.error.push(IndexError::DuplicateItem { kind, existing });
    } else {
        *signature = Some(id);
    }
}

fn index_value_equation(state: &mut State, id: ValueEquationId, cst: &cst::ValueEquation) {
    let name = cst.name_token().map(|t| {
        let text = t.text();
        SmolStr::from(text)
    });

    let item = TermItem::Value { signature: None, equations: vec![id] };

    let Some(name) = name else {
        state.index.insert_term_item(None, item);
        return;
    };

    let Some((item, existing)) = state.index.term_item_mut(&name) else {
        state.index.insert_term_item(Some(name), item);
        return;
    };

    let kind = ItemKind::ValueEquation(id);
    let existing = ExistingKind::Term(existing);
    let TermItem::Value { equations, .. } = item else {
        return state.error.push(IndexError::MismatchedItem { kind, existing });
    };

    equations.push(id);
}

fn index_infix(state: &mut State, id: InfixId, infix: &cst::InfixDeclaration) {
    let type_token = infix.type_token();
    let operator_token = infix.operator_token();

    let name = operator_token.map(|t| {
        let text = t.text();
        SmolStr::from(text)
    });

    let type_item = TypeItem::Operator { id };
    let term_item = TermItem::Operator { id };

    let Some(name) = name else {
        if type_token.is_some() {
            state.index.insert_type_item(None, type_item);
            return;
        } else {
            state.index.insert_term_item(None, term_item);
            return;
        }
    };

    if type_token.is_some() {
        if let Some((_, existing)) = state.index.type_item_mut(&name) {
            let kind = ItemKind::Operator(id);
            let existing = ExistingKind::Type(existing);
            state.error.push(IndexError::DuplicateItem { kind, existing });
        } else {
            state.index.insert_type_item(Some(name), type_item);
        }
    } else if let Some((_, existing)) = state.index.term_item_mut(&name) {
        let kind = ItemKind::Operator(id);
        let existing = ExistingKind::Term(existing);
        state.error.push(IndexError::DuplicateItem { kind, existing });
    } else {
        state.index.insert_term_item(Some(name), term_item);
    }
}

fn index_type_role(state: &mut State, id: TypeRoleId, cst: &cst::TypeRoleDeclaration) {
    let name = cst.name_token().map(|t| {
        let text = t.text();
        SmolStr::from(text)
    });

    let Some(name) = name else { return };
    let Some((item, existing)) = state.index.type_item_mut(&name) else {
        let existing = None;
        return state.error.push(IndexError::InvalidRole { id, existing });
    };

    if let TypeItem::Data { role, .. } | TypeItem::Newtype { role, .. } = item {
        if role.is_some() {
            let existing = Some(existing);
            state.error.push(IndexError::InvalidRole { id, existing });
        } else {
            *role = Some(id);
        }
    } else {
        let existing = None;
        state.error.push(IndexError::InvalidRole { id, existing });
    }
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

    let item = TermItem::Constructor { id };

    let Some(name) = name else {
        return state.index.insert_term_item(None, item);
    };

    let Some((_, existing)) = state.index.term_item_mut(&name) else {
        return state.index.insert_term_item(Some(name), item);
    };

    state.error.push(IndexError::DuplicateItem {
        kind: ItemKind::Constructor(id),
        existing: ExistingKind::Term(existing),
    });

    existing
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

    let item = TermItem::ClassMember { id };

    let Some(name) = name else {
        return state.index.insert_term_item(None, item);
    };

    let Some((_, existing)) = state.index.term_item_mut(&name) else {
        return state.index.insert_term_item(Some(name), item);
    };

    state.error.push(IndexError::DuplicateItem {
        kind: ItemKind::ClassMember(id),
        existing: ExistingKind::Term(existing),
    });

    existing
}

fn index_instance(state: &mut State, id: InstanceId, cst: &cst::InstanceDeclaration) {
    let name = cst.instance_name().and_then(|n| {
        let token = n.name_token()?;
        let text = token.text();
        Some(SmolStr::from(text))
    });
    state.index.insert_term_item(name, TermItem::Instance { id });
}

fn index_derive(state: &mut State, id: DeriveId, cst: &cst::DeriveDeclaration) {
    let name = cst.instance_name().and_then(|n| {
        let token = n.name_token()?;
        let text = token.text();
        Some(SmolStr::from(text))
    });
    state.index.insert_term_item(name, TermItem::Derive { id });
}
