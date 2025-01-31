use rowan::ast::AstChildren;
use smol_str::{SmolStr, SmolStrBuilder};
use syntax::cst;

use crate::{
    ClassMemberId, Duplicate, ExprItem, ExprItemId, IndexingError, IndexingResult, InstanceId,
    NominalIndex, RelationalIndex, SourceMap, TypeGroupId, TypeItem, TypeItemId, ValueGroupId,
};

#[derive(Default)]
struct State {
    source_map: SourceMap,
    nominal: NominalIndex,
    relational: RelationalIndex,
    errors: Vec<IndexingError>,
}

pub(super) fn index_module(module: &cst::Module) -> (IndexingResult, Vec<IndexingError>) {
    let mut state = State::default();

    if let Some(imports) = module.imports() {
        for import in imports.children() {
            index_import(&mut state, import);
        }
    }

    if let Some(statements) = module.statements() {
        for statement in statements.children() {
            index_statement(&mut state, statement);
        }
    }

    if let Some(header) = module.header() {
        if let Some(exports) = header.exports() {
            for export in exports.children() {
                index_export(&mut state, export);
            }
        }
    }

    let State { source_map, nominal, relational, errors } = state;
    let result = IndexingResult { source_map, nominal, relational };

    (result, errors)
}

fn index_export(state: &mut State, export: cst::ExportItem) {
    let export_id = state.source_map.insert_export(&export);

    let index_expr = |state: &mut State, name: &str| {
        let Some((item, _)) = state.nominal.expr_get_mut(name) else { return };
        if let Some(existing) = item.export_id {
            let duplicate = export_id;
            state.errors.push(IndexingError::DuplicateExport { existing, duplicate });
        } else {
            item.export_id = Some(export_id);
        }
    };

    let index_type = |state: &mut State, name: &str| {
        let Some((item, _)) = state.nominal.type_get_mut(name) else { return };
        if let Some(existing) = item.export_id {
            let duplicate = export_id;
            state.errors.push(IndexingError::DuplicateExport { existing, duplicate });
        } else {
            item.export_id = Some(export_id);
        }
    };

    let index_type_items_all = |state: &mut State, name: &str| {
        let Some((_, type_id)) = state.nominal.type_get_mut(name) else { return };

        let Some((_, TypeItem::Data(_) | TypeItem::Newtype(_), _)) =
            state.nominal.index_type_item(type_id)
        else {
            return state.errors.push(IndexingError::InvalidTypeItemExport { export_id, type_id });
        };

        for item_id in state.relational.constructors_of(type_id) {
            let Some((item, _)) = state.nominal.expr_index_mut(item_id) else { continue };
            if let Some(existing) = item.export_id {
                let duplicate = export_id;
                state.errors.push(IndexingError::DuplicateExport { existing, duplicate });
            } else {
                item.export_id = Some(export_id);
            }
        }
    };

    let index_type_items_list = |state: &mut State, name: &str, t: cst::TypeItemsList| {
        let Some((_, type_id)) = state.nominal.type_get_mut(name) else { return };

        let Some((_, TypeItem::Data(_) | TypeItem::Newtype(_), _)) =
            state.nominal.index_type_item(type_id)
        else {
            return state.errors.push(IndexingError::InvalidTypeItemExport { export_id, type_id });
        };

        for token in t.name_tokens() {
            let name = token.text();
            index_expr(state, &name);
        }
    };

    fn operator_name(name: &str) -> Option<&str> {
        let mut chars = name.chars();
        if chars.next() != Some('(') {
            return None;
        }
        if chars.next_back() != Some(')') {
            return None;
        }
        Some(chars.as_str())
    }

    match export {
        cst::ExportItem::ExportValue(v) => {
            let Some(token) = v.name_token() else { return };

            let name = token.text();
            index_expr(state, &name);
        }
        cst::ExportItem::ExportClass(c) => {
            let Some(token) = c.name_token() else { return };

            let name = token.text();
            index_type(state, &name);
        }
        cst::ExportItem::ExportType(t) => {
            let Some(token) = t.name_token() else { return };

            let name = token.text();
            index_type(state, &name);

            let Some(items) = t.type_items() else { return };
            match items {
                cst::TypeItems::TypeItemsAll(_) => {
                    let name = token.text();
                    index_type_items_all(state, name);
                }
                cst::TypeItems::TypeItemsList(t) => {
                    let name = token.text();
                    index_type_items_list(state, name, t);
                }
            }
        }
        cst::ExportItem::ExportOperator(o) => {
            let Some(token) = o.name_token() else { return };

            let name = token.text();
            let Some(name) = operator_name(name) else { return };

            index_expr(state, &name);
        }
        cst::ExportItem::ExportTypeOperator(o) => {
            let Some(token) = o.name_token() else { return };

            let name = token.text();
            let Some(name) = operator_name(name) else { return };

            index_type(state, &name);
        }
        cst::ExportItem::ExportModule(m) => {
            let Some(module_name) = m.module_name() else { return };

            let mut buffer = SmolStrBuilder::default();
            if let Some(qualifier) = module_name.qualifier() {
                if let Some(token) = qualifier.text() {
                    buffer.push_str(token.text());
                }
            }

            let Some(token) = module_name.name_token() else { return };
            buffer.push_str(token.text());

            let name = buffer.finish();
            if let Some((item, _)) = state.nominal.qualified_get_mut(&name) {
                if let Some(existing) = item.export_id {
                    let duplicate = export_id;
                    state.errors.push(IndexingError::DuplicateExport { existing, duplicate });
                } else {
                    item.export_id = Some(export_id);
                }
            } else {
                state.errors.push(IndexingError::InvalidExport { export_id });
            }
        }
    }
}

fn index_import(state: &mut State, import: cst::ImportStatement) {
    let import_id = state.source_map.insert_import(&import);

    let Some(import_alias) = import.import_alias() else { return };
    let Some(module_name) = import_alias.module_name() else { return };

    let mut buffer = SmolStrBuilder::default();
    if let Some(qualifier) = module_name.qualifier() {
        if let Some(token) = qualifier.text() {
            buffer.push_str(token.text());
        }
    }

    let Some(token) = module_name.name_token() else { return };
    buffer.push_str(token.text());

    let name = buffer.finish();
    state.nominal.insert_qualified(name, import_id);
}

fn index_statement(state: &mut State, declaration: cst::Declaration) {
    match declaration {
        cst::Declaration::ValueSignature(s) => {
            index_value_signature(state, s);
        }
        cst::Declaration::ValueEquation(e) => {
            index_value_equation(state, e);
        }
        cst::Declaration::InfixDeclaration(i) => {
            index_infix(state, i);
        }
        cst::Declaration::TypeRoleDeclaration(r) => {
            index_type_role(state, r);
        }
        cst::Declaration::InstanceChain(c) => {
            index_instance_chain(state, c);
        }
        cst::Declaration::TypeSynonymSignature(s) => {
            index_type_signature(state, TypeSignature::Synonym(s));
        }
        cst::Declaration::TypeSynonymEquation(e) => {
            index_type_declaration(state, TypeDeclaration::Synonym(e));
        }
        cst::Declaration::ClassSignature(s) => {
            index_type_signature(state, TypeSignature::Class(s));
        }
        cst::Declaration::ClassDeclaration(d) => {
            let statements = d.class_statements();
            let item_id = index_type_declaration(state, TypeDeclaration::Class(d));
            index_class_statements(state, item_id, statements);
        }
        cst::Declaration::ForeignImportDataDeclaration(f) => {
            index_foreign_data(state, f);
        }
        cst::Declaration::ForeignImportValueDeclaration(f) => {
            index_foreign_value(state, f);
        }
        cst::Declaration::NewtypeSignature(s) => {
            index_type_signature(state, TypeSignature::Newtype(s));
        }
        cst::Declaration::NewtypeEquation(e) => {
            let constructors = e.data_constructors();
            let item_id = index_type_declaration(state, TypeDeclaration::Newtype(e));
            index_data_constructors(state, item_id, constructors);
        }
        cst::Declaration::DataSignature(s) => {
            index_type_signature(state, TypeSignature::Data(s));
        }
        cst::Declaration::DataEquation(e) => {
            let constructors = e.data_constructors();
            let item_id = index_type_declaration(state, TypeDeclaration::Data(e));
            index_data_constructors(state, item_id, constructors);
        }
        cst::Declaration::DeriveDeclaration(d) => {
            index_derive_declaration(state, d);
        }
    }
}

fn index_value_signature(state: &mut State, signature: cst::ValueSignature) {
    let name_token = signature.name_token();

    let declaration = cst::Declaration::ValueSignature(signature);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    if let Some((item, item_id)) = state.nominal.expr_get_mut(name) {
        if let ExprItem::Value(group) = &mut item.value {
            if let Some(signature) = group.signature {
                state.errors.push(IndexingError::DuplicateSignature {
                    signature,
                    duplicate: declaration_id,
                });
            } else {
                group.signature = Some(declaration_id);
            }
        } else {
            let duplicate = Duplicate::Declaration(declaration_id);
            state.errors.push(IndexingError::DuplicateExprItem { item_id, duplicate });
        }
    } else {
        let name: SmolStr = name.into();
        let group = ValueGroupId::from_signature(declaration_id);
        state.nominal.insert_expr(name, ExprItem::Value(group));
    }
}

fn index_value_equation(state: &mut State, equation: cst::ValueEquation) {
    let name_token = equation.name_token();

    let declaration = cst::Declaration::ValueEquation(equation);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    if let Some((item, item_id)) = state.nominal.expr_get_mut(name) {
        if let ExprItem::Value(group) = &mut item.value {
            group.equations.push(declaration_id);
        } else {
            let duplicate = Duplicate::Declaration(declaration_id);
            state.errors.push(IndexingError::DuplicateExprItem { item_id, duplicate });
        }
    } else {
        let name: SmolStr = name.into();
        let group = ValueGroupId::from_equation(declaration_id);
        state.nominal.insert_expr(name, ExprItem::Value(group));
    }
}

impl TypeItem {
    fn role_group(&mut self) -> Option<&mut TypeGroupId> {
        match self {
            TypeItem::Data(g) => Some(g),
            TypeItem::Newtype(g) => Some(g),
            TypeItem::Foreign(g) => Some(g),
            _ => None,
        }
    }

    fn class_group(&mut self) -> Option<&mut TypeGroupId> {
        if let TypeItem::Class(g) = self {
            Some(g)
        } else {
            None
        }
    }

    fn class_item(g: TypeGroupId) -> TypeItem {
        TypeItem::Class(g)
    }

    fn data_group(&mut self) -> Option<&mut TypeGroupId> {
        if let TypeItem::Data(g) = self {
            Some(g)
        } else {
            None
        }
    }

    fn data_item(g: TypeGroupId) -> TypeItem {
        TypeItem::Data(g)
    }

    fn newtype_group(&mut self) -> Option<&mut TypeGroupId> {
        if let TypeItem::Newtype(g) = self {
            Some(g)
        } else {
            None
        }
    }

    fn newtype_item(g: TypeGroupId) -> TypeItem {
        TypeItem::Newtype(g)
    }

    fn synonym_group(&mut self) -> Option<&mut TypeGroupId> {
        if let TypeItem::Synonym(g) = self {
            Some(g)
        } else {
            None
        }
    }

    fn synonym_item(g: TypeGroupId) -> TypeItem {
        TypeItem::Synonym(g)
    }
}

enum TypeSignature {
    Class(cst::ClassSignature),
    Data(cst::DataSignature),
    Newtype(cst::NewtypeSignature),
    Synonym(cst::TypeSynonymSignature),
}

fn index_type_signature(state: &mut State, signature: TypeSignature) {
    type TypeItemGroupFn = fn(&mut TypeItem) -> Option<&mut TypeGroupId>;
    type MakeTypeItemFn = fn(TypeGroupId) -> TypeItem;

    let (name_token, declaration, item_fn, make_fn) = match signature {
        TypeSignature::Class(s) => (
            s.name_token(),
            cst::Declaration::ClassSignature(s),
            TypeItem::class_group as TypeItemGroupFn,
            TypeItem::class_item as MakeTypeItemFn,
        ),
        TypeSignature::Data(s) => (
            s.name_token(),
            cst::Declaration::DataSignature(s),
            TypeItem::data_group as TypeItemGroupFn,
            TypeItem::data_item as MakeTypeItemFn,
        ),
        TypeSignature::Newtype(s) => (
            s.name_token(),
            cst::Declaration::NewtypeSignature(s),
            TypeItem::newtype_group as TypeItemGroupFn,
            TypeItem::newtype_item as MakeTypeItemFn,
        ),
        TypeSignature::Synonym(s) => (
            s.name_token(),
            cst::Declaration::TypeSynonymSignature(s),
            TypeItem::synonym_group as TypeItemGroupFn,
            TypeItem::synonym_item as MakeTypeItemFn,
        ),
    };

    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    if let Some((item, item_id)) = state.nominal.type_get_mut(name) {
        if let Some(group) = item_fn(&mut item.value) {
            if let Some(signature) = group.signature {
                state.errors.push(IndexingError::DuplicateSignature {
                    signature,
                    duplicate: declaration_id,
                });
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
        state.nominal.insert_type(name, make_fn(group));
    }
}

enum TypeDeclaration {
    Class(cst::ClassDeclaration),
    Data(cst::DataEquation),
    Newtype(cst::NewtypeEquation),
    Synonym(cst::TypeSynonymEquation),
}

fn index_type_declaration(state: &mut State, declaration: TypeDeclaration) -> Option<TypeItemId> {
    type TypeItemGroupFn = fn(&mut TypeItem) -> Option<&mut TypeGroupId>;
    type MakeTypeItemFn = fn(TypeGroupId) -> TypeItem;

    let (name_token, declaration, item_fn, make_fn) = match declaration {
        TypeDeclaration::Class(d) => (
            d.class_head().and_then(|h| h.name_token()),
            cst::Declaration::ClassDeclaration(d),
            TypeItem::class_group as TypeItemGroupFn,
            TypeItem::class_item as MakeTypeItemFn,
        ),
        TypeDeclaration::Data(e) => (
            e.name_token(),
            cst::Declaration::DataEquation(e),
            TypeItem::data_group as TypeItemGroupFn,
            TypeItem::data_item as MakeTypeItemFn,
        ),
        TypeDeclaration::Newtype(e) => (
            e.name_token(),
            cst::Declaration::NewtypeEquation(e),
            TypeItem::newtype_group as TypeItemGroupFn,
            TypeItem::newtype_item as MakeTypeItemFn,
        ),
        TypeDeclaration::Synonym(e) => (
            e.name_token(),
            cst::Declaration::TypeSynonymEquation(e),
            TypeItem::synonym_group as TypeItemGroupFn,
            TypeItem::synonym_item as MakeTypeItemFn,
        ),
    };

    let declaration_id = state.source_map.insert_declaration(&declaration);
    let name_token = name_token?;

    let name = name_token.text();
    let type_item_id;

    if let Some((item, item_id)) = state.nominal.type_get_mut(name) {
        type_item_id = Some(item_id);
        if let Some(group) = item_fn(&mut item.value) {
            if let Some(declaration) = group.declaration {
                state.errors.push(IndexingError::DuplicateDeclaration {
                    declaration,
                    duplicate: declaration_id,
                });
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
        let item_id = state.nominal.insert_type(name, make_fn(group));
        type_item_id = Some(item_id);
    }

    type_item_id
}

fn index_type_role(state: &mut State, role: cst::TypeRoleDeclaration) {
    let name_token = role.name_token();

    let declaration = cst::Declaration::TypeRoleDeclaration(role);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    if let Some((item, _)) = state.nominal.type_get_mut(name) {
        if let Some(group) = item.value.role_group() {
            if group.declaration.is_none() {
                state.errors.push(IndexingError::EmptyRole { role: declaration_id });
            }
            if let Some(role) = group.role {
                state.errors.push(IndexingError::DuplicateDeclaration {
                    declaration: role,
                    duplicate: declaration_id,
                });
            } else {
                group.role = Some(declaration_id);
            }
        } else {
            state.errors.push(IndexingError::InvalidRole { role: declaration_id });
        }
    } else {
        state.errors.push(IndexingError::EmptyRole { role: declaration_id });
    }
}

fn index_class_member(state: &mut State, member: cst::ClassMemberStatement) -> ClassMemberId {
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
        let item = ExprItem::ClassMember(member_id);
        state.nominal.insert_expr(name, item);
    }

    member_id
}

fn index_class_statements(
    state: &mut State,
    item_id: Option<TypeItemId>,
    statements: Option<cst::ClassStatements>,
) {
    let Some(statements) = statements else { return };
    for statement in statements.children() {
        let member_id = index_class_member(state, statement);
        if let Some(item_id) = item_id {
            state.relational.class_member_of.push((item_id, member_id));
        }
    }
}

fn index_data_constructor(
    state: &mut State,
    constructor: cst::DataConstructor,
) -> Option<ExprItemId> {
    let name_token = constructor.name_token()?;
    let constructor_id = state.source_map.insert_constructor(&constructor);

    let name = name_token.text();
    if let Some((_, item_id)) = state.nominal.expr_get_mut(name) {
        let duplicate = Duplicate::Constructor(constructor_id);
        state.errors.push(IndexingError::DuplicateExprItem { item_id, duplicate });
        Some(item_id)
    } else {
        let name: SmolStr = name.into();
        let item = ExprItem::Constructor(constructor_id);
        Some(state.nominal.insert_expr(name, item))
    }
}

fn index_data_constructors(
    state: &mut State,
    type_id: Option<TypeItemId>,
    constructors: AstChildren<cst::DataConstructor>,
) {
    for constructor in constructors {
        let Some(item_id) = index_data_constructor(state, constructor) else { continue };
        let Some(type_id) = type_id else { continue };
        state.relational.constructor_of.push((type_id, item_id));
    }
}

fn index_infix(state: &mut State, infix: cst::InfixDeclaration) {
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
    } else if let Some((_, item_id)) = state.nominal.type_get_mut(operator) {
        let duplicate = Duplicate::Declaration(declaration_id);
        state.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
    } else {
        let operator: SmolStr = operator.into();
        state.nominal.insert_type(operator, TypeItem::Operator(declaration_id));
    }
}

fn index_foreign_data(state: &mut State, foreign: cst::ForeignImportDataDeclaration) {
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

fn index_foreign_value(state: &mut State, foreign: cst::ForeignImportValueDeclaration) {
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

fn index_derive_declaration(state: &mut State, derive: cst::DeriveDeclaration) {
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

fn index_instance_chain(state: &mut State, chain: cst::InstanceChain) {
    let instance_declarations = chain.instance_declarations();

    let declaration = cst::Declaration::InstanceChain(chain);
    let chain_id = state.source_map.insert_declaration(&declaration);

    for instance in instance_declarations {
        let instance_id = index_instance_declaration(state, instance);
        state.relational.instance_of.push((chain_id, instance_id));
    }
}

fn index_instance_declaration(state: &mut State, instance: cst::InstanceDeclaration) -> InstanceId {
    let instance_name = instance.instance_name();
    let instance_statements = instance.instance_statements();

    let instance_id = state.source_map.insert_instance(&instance);

    let Some(name_token) = instance_name.and_then(|n| n.name_token()) else {
        return instance_id;
    };

    let name = name_token.text();
    if let Some((_, item_id)) = state.nominal.expr_get_mut(name) {
        let duplicate = Duplicate::Instance(instance_id);
        state.errors.push(IndexingError::DuplicateExprItem { item_id, duplicate });
    } else {
        let name: SmolStr = name.into();
        state.nominal.insert_expr(name, ExprItem::Instance(instance_id));
    }

    let Some(instance_statements) = instance_statements else {
        return instance_id;
    };

    // We don't need to check for well-formedness during traversal because
    // instance members are not nominally indexed. The only conflicts that
    // can occur here are duplicate or late signatures, which do not affect
    // the semantics of these statements. Instead, we only check for these
    // errors when we need them for error-reporting, at the end of indexing.
    for statement in instance_statements.children() {
        let statement_id = state.source_map.insert_instance_member(&statement);
        state.relational.instance_member_of.push((instance_id, statement_id));
    }

    instance_id
}
