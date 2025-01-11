use smol_str::SmolStr;
use syntax::cst;

use crate::{
    ClassMemberId, ConstructorId, Duplicate, ExprItem, IndexingError, InstanceId, NominalIndex,
    RelationalIndex, SourceMap, TypeGroupId, TypeItem, ValueGroupId,
};

#[derive(Default)]
pub(super) struct IndexState {
    pub(super) source_map: SourceMap,
    pub(super) nominal: NominalIndex,
    pub(super) relational: RelationalIndex,
    pub(super) errors: Vec<IndexingError>,
}

pub(super) fn index_module(state: &mut IndexState, module: &cst::Module) {
    let Some(statements) = module.statements() else { return };
    for statement in statements.children() {
        index_declaration(state, statement);
    }
}

fn index_declaration(state: &mut IndexState, declaration: cst::Declaration) {
    match declaration {
        cst::Declaration::ValueSignature(s) => index_value_signature(state, s),
        cst::Declaration::ValueEquation(e) => index_value_equation(state, e),
        cst::Declaration::ClassSignature(s) => index_class_signature(state, s),
        cst::Declaration::ClassDeclaration(d) => index_class_declaration(state, d),
        cst::Declaration::InstanceChain(c) => index_instance_chain(state, c),
        cst::Declaration::TypeSynonymSignature(s) => index_synonym_signature(state, s),
        cst::Declaration::TypeSynonymEquation(e) => index_synonym_equation(state, e),
        cst::Declaration::NewtypeSignature(s) => index_newtype_signature(state, s),
        cst::Declaration::NewtypeEquation(e) => index_newtype_equation(state, e),
        cst::Declaration::DataSignature(s) => index_data_signature(state, s),
        cst::Declaration::DataEquation(e) => index_data_equation(state, e),
        _ => (),
    }
}

fn index_value_signature(state: &mut IndexState, signature: cst::ValueSignature) {
    let name_token = signature.name_token();

    let declaration = cst::Declaration::ValueSignature(signature);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    if let Some((item, item_id)) = state.nominal.expr_get_mut(name) {
        if let ExprItem::Value(group) = item {
            if let &[declaration, ..] = &group.equations[..] {
                let signature = declaration_id;
                state.errors.push(IndexingError::LateSignature { declaration, signature });
            }
            if group.signature.is_some() {
                let duplicate = Duplicate::Declaration(declaration_id);
                state.errors.push(IndexingError::DuplicateExprItem { item_id, duplicate });
            } else {
                group.signature = Some(declaration_id);
            }
        } else {
            let duplicate = Duplicate::Declaration(declaration_id);
            state.errors.push(IndexingError::DuplicateExprItem { item_id, duplicate });
        }
    } else {
        let name: SmolStr = name.into();
        let group = ValueGroupId { signature: Some(declaration_id), equations: vec![] };
        state.nominal.insert_expr(name, ExprItem::Value(group));
    }
}

fn index_value_equation(state: &mut IndexState, equation: cst::ValueEquation) {
    let name_token = equation.name_token();

    let declaration = cst::Declaration::ValueEquation(equation);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    if let Some((item, item_id)) = state.nominal.expr_get_mut(name) {
        if let ExprItem::Value(group) = item {
            group.equations.push(declaration_id);
        } else {
            let duplicate = Duplicate::Declaration(declaration_id);
            state.errors.push(IndexingError::DuplicateExprItem { item_id, duplicate });
        }
    } else {
        let name: SmolStr = name.into();
        let group = ValueGroupId { signature: None, equations: vec![declaration_id] };
        state.nominal.insert_expr(name, ExprItem::Value(group));
    }
}

fn index_class_signature(state: &mut IndexState, signature: cst::ClassSignature) {
    let name_token = signature.name_token();

    let declaration = cst::Declaration::ClassSignature(signature);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    if let Some((item, item_id)) = state.nominal.type_get_mut(name) {
        if let TypeItem::Class(group) = item {
            if let Some(declaration) = group.declaration {
                let signature = declaration_id;
                state.errors.push(IndexingError::LateSignature { declaration, signature })
            }
            if group.signature.is_some() {
                let duplicate = Duplicate::Declaration(declaration_id);
                state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
            } else {
                group.signature = Some(declaration_id);
            }
        } else {
            let duplicate = Duplicate::Declaration(declaration_id);
            state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
        }
    } else {
        let name: SmolStr = name.into();
        let group = TypeGroupId { signature: Some(declaration_id), declaration: None };
        state.nominal.insert_type(name, TypeItem::Class(group));
    }
}

fn index_class_declaration(state: &mut IndexState, declaration: cst::ClassDeclaration) {
    let class_head = declaration.class_head();
    let class_statements = declaration.class_statements();

    let declaration = cst::Declaration::ClassDeclaration(declaration);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let mut class_item_id = None;
    if let Some(name_token) = class_head.and_then(|c| c.name_token()) {
        let name = name_token.text();
        if let Some((item, item_id)) = state.nominal.type_get_mut(name) {
            if let TypeItem::Class(group) = item {
                if group.declaration.is_some() {
                    let duplicate = Duplicate::Declaration(declaration_id);
                    state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
                } else {
                    group.declaration = Some(declaration_id);
                    class_item_id = Some(item_id);
                }
            } else {
                let duplicate = Duplicate::Declaration(declaration_id);
                state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
            }
        } else {
            let name: SmolStr = name.into();
            let group = TypeGroupId { signature: None, declaration: Some(declaration_id) };
            class_item_id = Some(state.nominal.insert_type(name, TypeItem::Class(group)));
        }
    }

    let Some(class_statements) = class_statements else { return };

    for statement in class_statements.children() {
        let member_id = index_class_member(state, statement);
        if let Some(class_item_id) = class_item_id {
            state.relational.method_of.push((class_item_id, member_id));
        }
    }
}

fn index_class_member(state: &mut IndexState, member: cst::ClassMemberStatement) -> ClassMemberId {
    let name_token = member.name_token();
    let member_id = state.source_map.insert_class_member(&member);

    let Some(name_token) = name_token else {
        return member_id;
    };

    let name = name_token.text();

    if let Some((_, item_id)) = state.nominal.expr_get_mut(name) {
        let duplicate = Duplicate::ClassMember(member_id);
        state.errors.push(IndexingError::DuplicateExprItem { item_id, duplicate });
    } else {
        let name: SmolStr = name.into();
        let item = ExprItem::Method(member_id);
        state.nominal.insert_expr(name, item);
    }

    member_id
}

fn index_instance_chain(state: &mut IndexState, chain: cst::InstanceChain) {
    let instance_declarations = chain.instance_declarations();

    let declaration = cst::Declaration::InstanceChain(chain);
    let _ = state.source_map.insert_declaration(&declaration);

    for instance_declaration in instance_declarations {
        index_instance_declaration(state, instance_declaration);
    }
}

fn index_instance_declaration(
    state: &mut IndexState,
    instance: cst::InstanceDeclaration,
) -> InstanceId {
    let instance_name = instance.instance_name();
    let instance_id = state.source_map.insert_instance(&instance);

    if let Some(name_token) = instance_name.and_then(|i| i.name_token()) {
        let name = name_token.text();
        if let Some((_, item_id)) = state.nominal.expr_get_mut(name) {
            let duplicate = Duplicate::Instance(instance_id);
            state.errors.push(IndexingError::DuplicateExprItem { item_id, duplicate });
        } else {
            let name: SmolStr = name.into();
            let item = ExprItem::Instance(instance_id);
            state.nominal.insert_expr(name, item);
        }
    }

    instance_id
}

fn index_synonym_signature(state: &mut IndexState, signature: cst::TypeSynonymSignature) {
    let name_token = signature.name_token();

    let declaration = cst::Declaration::TypeSynonymSignature(signature);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    if let Some((item, item_id)) = state.nominal.type_get_mut(name) {
        if let TypeItem::Synonym(group) = item {
            if let Some(declaration) = group.declaration {
                let signature = declaration_id;
                state.errors.push(IndexingError::LateSignature { declaration, signature })
            }
            if group.signature.is_some() {
                let duplicate = Duplicate::Declaration(declaration_id);
                state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
            } else {
                group.signature = Some(declaration_id);
            }
        } else {
            let duplicate = Duplicate::Declaration(declaration_id);
            state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
        }
    } else {
        let name: SmolStr = name.into();
        let group = TypeGroupId { signature: Some(declaration_id), declaration: None };
        state.nominal.insert_type(name, TypeItem::Synonym(group));
    }
}

fn index_synonym_equation(state: &mut IndexState, equation: cst::TypeSynonymEquation) {
    let name_token = equation.name_token();

    let declaration = cst::Declaration::TypeSynonymEquation(equation);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    if let Some((item, item_id)) = state.nominal.type_get_mut(name) {
        if let TypeItem::Synonym(group) = item {
            if group.declaration.is_some() {
                let duplicate = Duplicate::Declaration(declaration_id);
                state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
            } else {
                group.declaration = Some(declaration_id);
            }
        } else {
            let duplicate = Duplicate::Declaration(declaration_id);
            state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
        }
    } else {
        let name: SmolStr = name.into();
        let group = TypeGroupId { signature: None, declaration: Some(declaration_id) };
        state.nominal.insert_type(name, TypeItem::Synonym(group));
    }
}

fn index_newtype_signature(state: &mut IndexState, signature: cst::NewtypeSignature) {
    let name_token = signature.name_token();

    let declaration = cst::Declaration::NewtypeSignature(signature);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    if let Some((item, item_id)) = state.nominal.type_get_mut(name) {
        if let TypeItem::Newtype(group) = item {
            if let Some(declaration) = group.declaration {
                let signature = declaration_id;
                state.errors.push(IndexingError::LateSignature { declaration, signature })
            }
            if group.signature.is_some() {
                let duplicate = Duplicate::Declaration(declaration_id);
                state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
            } else {
                group.signature = Some(declaration_id);
            }
        } else {
            let duplicate = Duplicate::Declaration(declaration_id);
            state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
        }
    } else {
        let name: SmolStr = name.into();
        let group = TypeGroupId { signature: Some(declaration_id), declaration: None };
        state.nominal.insert_type(name, TypeItem::Newtype(group));
    }
}

fn index_newtype_equation(state: &mut IndexState, equation: cst::NewtypeEquation) {
    let name_token = equation.name_token();
    let data_constructors = equation.data_constructors();

    let declaration = cst::Declaration::NewtypeEquation(equation);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    let mut equation_item_id = None;
    if let Some((item, item_id)) = state.nominal.type_get_mut(name) {
        if let TypeItem::Newtype(group) = item {
            if group.declaration.is_some() {
                let duplicate = Duplicate::Declaration(declaration_id);
                state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
            } else {
                group.declaration = Some(declaration_id);
                equation_item_id = Some(item_id);
            }
        } else {
            let duplicate = Duplicate::Declaration(declaration_id);
            state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
        }
    } else {
        let name: SmolStr = name.into();
        let group = TypeGroupId { signature: None, declaration: Some(declaration_id) };
        equation_item_id = Some(state.nominal.insert_type(name, TypeItem::Newtype(group)));
    };

    for data_constructor in data_constructors {
        let constructor_id = index_data_constructor(state, data_constructor);
        if let Some(equation_item_id) = equation_item_id {
            state.relational.constructor_of.push((equation_item_id, constructor_id));
        }
    }
}

fn index_data_constructor(
    state: &mut IndexState,
    constructor: cst::DataConstructor,
) -> ConstructorId {
    let name_token = constructor.name_token();
    let constructor_id = state.source_map.insert_constructor(&constructor);

    let Some(name_token) = name_token else {
        return constructor_id;
    };

    let name = name_token.text();

    if let Some((_, item_id)) = state.nominal.expr_get_mut(name) {
        let duplicate = Duplicate::Constructor(constructor_id);
        state.errors.push(IndexingError::DuplicateExprItem { item_id, duplicate });
    } else {
        let name: SmolStr = name.into();
        let item = ExprItem::Constructor(constructor_id);
        state.nominal.insert_expr(name, item);
    }

    constructor_id
}

fn index_data_signature(state: &mut IndexState, signature: cst::DataSignature) {
    let name_token = signature.name_token();

    let declaration = cst::Declaration::DataSignature(signature);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    if let Some((item, item_id)) = state.nominal.type_get_mut(name) {
        if let TypeItem::Data(group) = item {
            if let Some(declaration) = group.declaration {
                let signature = declaration_id;
                state.errors.push(IndexingError::LateSignature { declaration, signature })
            }
            if group.signature.is_some() {
                let duplicate = Duplicate::Declaration(declaration_id);
                state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
            } else {
                group.signature = Some(declaration_id);
            }
        } else {
            let duplicate = Duplicate::Declaration(declaration_id);
            state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
        }
    } else {
        let name: SmolStr = name.into();
        let group = TypeGroupId { signature: Some(declaration_id), declaration: None };
        state.nominal.insert_type(name, TypeItem::Data(group));
    }
}

fn index_data_equation(state: &mut IndexState, equation: cst::DataEquation) {
    let name_token = equation.name_token();
    let data_constructors = equation.data_constructors();

    let declaration = cst::Declaration::DataEquation(equation);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    let mut equation_item_id = None;
    if let Some((item, item_id)) = state.nominal.type_get_mut(name) {
        if let TypeItem::Data(group) = item {
            if group.declaration.is_some() {
                let duplicate = Duplicate::Declaration(declaration_id);
                state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
            } else {
                group.declaration = Some(declaration_id);
                equation_item_id = Some(item_id);
            }
        } else {
            let duplicate = Duplicate::Declaration(declaration_id);
            state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
        }
    } else {
        let name: SmolStr = name.into();
        let group = TypeGroupId { signature: None, declaration: Some(declaration_id) };
        equation_item_id = Some(state.nominal.insert_type(name, TypeItem::Data(group)));
    };

    for data_constructor in data_constructors {
        let constructor_id = index_data_constructor(state, data_constructor);
        if let Some(equation_item_id) = equation_item_id {
            state.relational.constructor_of.push((equation_item_id, constructor_id));
        }
    }
}
