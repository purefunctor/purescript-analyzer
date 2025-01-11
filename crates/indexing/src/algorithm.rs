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
        cst::Declaration::InfixDeclaration(i) => index_infix(state, i),
        cst::Declaration::TypeRoleDeclaration(r) => index_type_role(state, r),
        cst::Declaration::ClassSignature(s) => index_class_signature(state, s),
        cst::Declaration::ClassDeclaration(d) => index_class_declaration(state, d),
        cst::Declaration::InstanceChain(c) => index_instance_chain(state, c),
        cst::Declaration::TypeSynonymSignature(s) => index_synonym_signature(state, s),
        cst::Declaration::TypeSynonymEquation(e) => index_synonym_equation(state, e),
        cst::Declaration::ForeignImportDataDeclaration(f) => index_foreign_data(state, f),
        cst::Declaration::ForeignImportValueDeclaration(f) => index_foreign_value(state, f),
        cst::Declaration::NewtypeSignature(s) => index_newtype_signature(state, s),
        cst::Declaration::NewtypeEquation(e) => index_newtype_equation(state, e),
        cst::Declaration::DataSignature(s) => index_data_signature(state, s),
        cst::Declaration::DataEquation(e) => index_data_equation(state, e),
        cst::Declaration::DeriveDeclaration(d) => index_derive_declaration(state, d),
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

fn index_infix(state: &mut IndexState, infix: cst::InfixDeclaration) {
    let type_token = infix.type_token();
    let operator_token = infix.operator_token();

    let declaration = cst::Declaration::InfixDeclaration(infix);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(operator_token) = operator_token else { return };
    let operator = operator_token.text();

    if type_token.is_none() {
        if let Some((_, item_id)) = state.nominal.expr_get_mut(operator) {
            let duplicate = Duplicate::Declaration(declaration_id);
            state.errors.push(IndexingError::DuplicateExprItem { item_id, duplicate });
        } else {
            let operator: SmolStr = operator.into();
            state.nominal.insert_expr(operator, ExprItem::Operator(declaration_id));
        }
    } else {
        if let Some((_, item_id)) = state.nominal.type_get_mut(operator) {
            let duplicate = Duplicate::Declaration(declaration_id);
            state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
        } else {
            let operator: SmolStr = operator.into();
            state.nominal.insert_type(operator, TypeItem::Operator(declaration_id));
        }
    }
}

fn index_type_role(state: &mut IndexState, role: cst::TypeRoleDeclaration) {
    let name_token = role.name_token();

    let declaration = cst::Declaration::TypeRoleDeclaration(role);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    if let Some((item, item_id)) = state.nominal.type_get_mut(name) {
        match item {
            TypeItem::Class(_) | TypeItem::Synonym(_) | TypeItem::Operator(_) => {
                state.errors.push(IndexingError::InvalidRoleDeclaration {
                    item_id: Some(item_id),
                    declaration: declaration_id,
                    early: false,
                });
            }
            TypeItem::Data(group) | TypeItem::Newtype(group) | TypeItem::Foreign(group) => {
                if group.declaration.is_none() {
                    state.errors.push(IndexingError::InvalidRoleDeclaration {
                        item_id: Some(item_id),
                        declaration: declaration_id,
                        early: true,
                    });
                }
                if group.role.is_some() {
                    let duplicate = Duplicate::Declaration(declaration_id);
                    state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
                } else {
                    group.role = Some(declaration_id);
                }
            }
        };
    } else {
        state.errors.push(IndexingError::InvalidRoleDeclaration {
            item_id: None,
            declaration: declaration_id,
            early: true,
        });
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
        let group = TypeGroupId::from_signature(declaration_id);
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
            let group = TypeGroupId::from_declaration(declaration_id);
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
    let chain_id = state.source_map.insert_declaration(&declaration);

    for instance_declaration in instance_declarations {
        let instance_id = index_instance_declaration(state, instance_declaration);
        state.relational.instance_of.push((chain_id, instance_id));
    }
}

fn index_instance_declaration(
    state: &mut IndexState,
    instance: cst::InstanceDeclaration,
) -> InstanceId {
    let instance_name = instance.instance_name();
    let instance_statements = instance.instance_statements();

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

    let Some(instance_statements) = instance_statements else { return instance_id };
    for instance_statement in instance_statements.children() {
        let instance_member_id = state.source_map.insert_instance_member(&instance_statement);
        state.relational.instance_member_of.push((instance_id, instance_member_id));
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
        let group = TypeGroupId::from_signature(declaration_id);
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
        let group = TypeGroupId::from_declaration(declaration_id);
        state.nominal.insert_type(name, TypeItem::Synonym(group));
    }
}

fn index_foreign_data(state: &mut IndexState, foreign: cst::ForeignImportDataDeclaration) {
    let name_token = foreign.name_token();

    let declaration = cst::Declaration::ForeignImportDataDeclaration(foreign);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    if let Some((_, item_id)) = state.nominal.type_get_mut(name) {
        let duplicate = Duplicate::Declaration(declaration_id);
        state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
    } else {
        let name: SmolStr = name.into();
        let group = TypeGroupId::from_declaration(declaration_id);
        state.nominal.insert_type(name, TypeItem::Foreign(group));
    }
}

fn index_foreign_value(state: &mut IndexState, foreign: cst::ForeignImportValueDeclaration) {
    let name_token = foreign.name_token();

    let declaration = cst::Declaration::ForeignImportValueDeclaration(foreign);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    if let Some((_, item_id)) = state.nominal.expr_get_mut(name) {
        let duplicate = Duplicate::Declaration(declaration_id);
        state.errors.push(IndexingError::DuplicateExprItem { item_id, duplicate });
    } else {
        let name: SmolStr = name.into();
        state.nominal.insert_expr(name, ExprItem::Foreign(declaration_id));
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
        let group = TypeGroupId::from_signature(declaration_id);
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
        let group = TypeGroupId::from_declaration(declaration_id);
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
        let group = TypeGroupId::from_signature(declaration_id);
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
        let group = TypeGroupId::from_declaration(declaration_id);
        equation_item_id = Some(state.nominal.insert_type(name, TypeItem::Data(group)));
    };

    for data_constructor in data_constructors {
        let constructor_id = index_data_constructor(state, data_constructor);
        if let Some(equation_item_id) = equation_item_id {
            state.relational.constructor_of.push((equation_item_id, constructor_id));
        }
    }
}

fn index_derive_declaration(state: &mut IndexState, derive: cst::DeriveDeclaration) {
    let instance_name = derive.instance_name();

    let declaration = cst::Declaration::DeriveDeclaration(derive);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    if let Some(name_token) = instance_name.and_then(|i| i.name_token()) {
        let name = name_token.text();
        if let Some((_, item_id)) = state.nominal.expr_get_mut(name) {
            let duplicate = Duplicate::Declaration(declaration_id);
            state.errors.push(IndexingError::DuplicateExprItem { item_id, duplicate });
        } else {
            let name: SmolStr = name.into();
            state.nominal.insert_expr(name, ExprItem::Derive(declaration_id));
        }
    }
}
