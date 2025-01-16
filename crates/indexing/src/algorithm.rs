use std::mem;

use smol_str::SmolStr;
use syntax::cst::{self, Type};

use crate::{
    ClassMemberId, DeclarationId, Duplicate, ExprItem, FullIndexingResult, IndexingError,
    NominalIndex, RelationalIndex, SourceMap, TypeGroupId, TypeItem, ValueGroupId,
};

impl ValueGroupId {
    fn check_for_errors(&self, errors: &mut Vec<IndexingError>) {
        debug_assert!(
            self.signature.is_some() || !self.equations.is_empty(),
            "Invalid ValueGroupId"
        );
        let Some(signature) = self.signature else { return };
        if self.equations.is_empty() {
            return errors.push(IndexingError::EmptySignature { signature });
        }
        // Invariant: IDs are allocated in order of appearance in the source code.
        // Therefore, equations that have smaller IDs than the signature are invalid.
        for declaration in self.equations.iter().filter(|&equation| signature > *equation).copied()
        {
            errors.push(IndexingError::EarlyDeclaration { declaration, signature });
        }
    }
}

impl TypeGroupId {
    fn check_for_errors(&self, errors: &mut Vec<IndexingError>) {
        debug_assert!(
            self.signature.is_some() || self.declaration.is_some() || self.role.is_some(),
            "Invalid TypeGroupId"
        );
        if let Some(signature) = self.signature {
            if let Some(declaration) = self.declaration {
                if signature > declaration {
                    errors.push(IndexingError::EarlyDeclaration { declaration, signature });
                }
            } else {
                errors.push(IndexingError::EmptySignature { signature });
            }
        }
        if let Some(role) = self.role {
            if let Some(declaration) = self.declaration {
                if role < declaration {
                    errors.push(IndexingError::EarlyRole { role });
                }
            } else {
                errors.push(IndexingError::EmptyRole { role });
            }
        }
    }
}

// region: State

#[derive(Default)]
struct State {
    current_group: Option<(SmolStr, CurrentGroup)>,
    source_map: SourceMap,
    nominal: NominalIndex,
    relational: RelationalIndex,
    errors: Vec<IndexingError>,
}

impl From<State> for FullIndexingResult {
    fn from(mut state: State) -> FullIndexingResult {
        if let Some((name, group)) = mem::take(&mut state.current_group) {
            state.insert_group(name, group);
        }
        FullIndexingResult {
            source_map: state.source_map,
            nominal: state.nominal,
            relational: state.relational,
            errors: state.errors,
        }
    }
}

enum CurrentGroup {
    Type { kind: TypeGroupKind, group: TypeGroupId },
    Value { group: ValueGroupId },
}

#[derive(PartialEq, Eq)]
enum TypeGroupKind {
    Class,
    Data,
    Newtype,
    Synonym,
}

impl State {
    fn insert_group(&mut self, name: SmolStr, group: CurrentGroup) {
        match group {
            CurrentGroup::Type { kind, group } => {
                group.check_for_errors(&mut self.errors);
                let group = match kind {
                    TypeGroupKind::Class => TypeItem::Class(group),
                    TypeGroupKind::Data => TypeItem::Data(group),
                    TypeGroupKind::Newtype => TypeItem::Newtype(group),
                    TypeGroupKind::Synonym => TypeItem::Synonym(group),
                };
                self.nominal.insert_type(name, group);
            }
            CurrentGroup::Value { group } => {
                group.check_for_errors(&mut self.errors);
                self.nominal.insert_expr(name, ExprItem::Value(group));
            }
        }
    }

    fn check_duplicate_expr(&mut self, name: &str, duplicate: Duplicate) -> bool {
        let Some((item_id, _)) = self.nominal.lookup_expr_item(name) else { return false };
        self.errors.push(IndexingError::DuplicateExprItem { item_id, duplicate });
        true
    }

    fn check_duplicate_type(&mut self, name: &str, duplicate: Duplicate) -> bool {
        let Some((item_id, _)) = self.nominal.lookup_type_item(name) else { return false };
        self.errors.push(IndexingError::DuplicateTypeItem { item_id, duplicate });
        true
    }

    fn with_value_group(&mut self, name: &str, f: impl FnOnce(&mut State, &mut ValueGroupId)) {
        // If the current group is a value group with the same name, take it;
        // Otherwise, create a new value group and insert the previous group.
        let (name, mut group) = match mem::take(&mut self.current_group) {
            Some((n, CurrentGroup::Value { group: g })) if n == name => (n, g),
            current_group => {
                if let Some((n, g)) = current_group {
                    self.insert_group(n, g);
                }
                (name.into(), ValueGroupId::default())
            }
        };
        // The logic above allows us to write logic over &mut ValueGroupId,
        // rather than having to match over the current Option<CurrentGroup>.
        f(self, &mut group);
        // Finally, we either return the existing group that we took or
        // introduce the new value group that we created to the state.
        self.current_group = Some((name, CurrentGroup::Value { group }));
    }

    fn with_type_group(
        &mut self,
        name: &str,
        kind: TypeGroupKind,
        f: impl FnOnce(&mut State, &mut TypeGroupId),
    ) {
        let (name, mut group) = match mem::take(&mut self.current_group) {
            Some((n, CurrentGroup::Type { kind: k, group: g })) if n == name && k == kind => (n, g),
            current_group => {
                if let Some((n, g)) = current_group {
                    self.insert_group(n, g);
                }
                (name.into(), TypeGroupId::default())
            }
        };
        f(self, &mut group);
        self.current_group = Some((name, CurrentGroup::Type { kind, group }));
    }

    fn value_signature(&mut self, name: &str, id: DeclarationId) {
        if self.check_duplicate_expr(name, Duplicate::Declaration(id)) {
            return;
        }
        self.with_value_group(name, |state, group| {
            if let Some(signature) = group.signature {
                state.errors.push(IndexingError::DuplicateSignature { signature });
            } else {
                group.signature = Some(id);
            }
        });
    }

    fn value_equation(&mut self, name: &str, id: DeclarationId) {
        if self.check_duplicate_expr(name, Duplicate::Declaration(id)) {
            return;
        }
        self.with_value_group(name, |_, group| {
            group.equations.push(id);
        });
    }

    fn synonym_signature(&mut self, name: &str, id: DeclarationId) {
        if self.check_duplicate_type(name, Duplicate::Declaration(id)) {
            return;
        }
        self.with_type_group(name, TypeGroupKind::Synonym, |state, group| {
            if let Some(signature) = group.signature {
                state.errors.push(IndexingError::DuplicateSignature { signature });
            } else {
                group.signature = Some(id);
            }
        });
    }

    fn synonym_equation(&mut self, name: &str, id: DeclarationId) {
        if self.check_duplicate_type(name, Duplicate::Declaration(id)) {
            return;
        }
        self.with_type_group(name, TypeGroupKind::Synonym, |state, group| {
            if let Some(declaration) = group.declaration {
                state.errors.push(IndexingError::DuplicateDeclaration { declaration });
            } else {
                group.declaration = Some(id);
            }
        });
    }

    fn class_signature(&mut self, name: &str, id: DeclarationId) {
        if self.check_duplicate_type(name, Duplicate::Declaration(id)) {
            return;
        }
        self.with_type_group(name, TypeGroupKind::Class, |state, group| {
            if let Some(signature) = group.signature {
                state.errors.push(IndexingError::DuplicateSignature { signature });
            } else {
                group.signature = Some(id);
            }
        });
    }

    fn class_declaration(&mut self, name: &str, id: DeclarationId) {
        if self.check_duplicate_type(name, Duplicate::Declaration(id)) {
            return;
        }
        self.with_type_group(name, TypeGroupKind::Class, |state, group| {
            if let Some(declaration) = group.declaration {
                state.errors.push(IndexingError::DuplicateDeclaration { declaration });
            } else {
                group.declaration = Some(id);
            }
        });
    }

    fn class_member(&mut self, name: &str, id: ClassMemberId) {
        if self.check_duplicate_expr(name, Duplicate::ClassMember(id)) {
            return;
        }
        let name = name.into();
        let item = ExprItem::Method(id);
        self.nominal.insert_expr(name, item);
    }
}

// endregion: State

// region: Traversals

pub(super) fn index_module(module: &cst::Module) -> FullIndexingResult {
    let mut state = State::default();
    if let Some(statements) = module.statements() {
        for statement in statements.children() {
            index_statement(&mut state, statement);
        }
    }
    FullIndexingResult::from(state)
}

fn index_statement(state: &mut State, declaration: cst::Declaration) {
    match declaration {
        cst::Declaration::ValueSignature(s) => index_value_signature(state, s),
        cst::Declaration::ValueEquation(e) => index_value_equation(state, e),
        cst::Declaration::InfixDeclaration(i) => todo!(),
        cst::Declaration::TypeRoleDeclaration(r) => todo!(),
        cst::Declaration::InstanceChain(i) => todo!(),
        cst::Declaration::TypeSynonymSignature(s) => index_synonym_signature(state, s),
        cst::Declaration::TypeSynonymEquation(e) => index_synonym_equation(state, e),
        cst::Declaration::ClassSignature(s) => index_class_signature(state, s),
        cst::Declaration::ClassDeclaration(d) => index_class_declaration(state, d),
        cst::Declaration::ForeignImportDataDeclaration(d) => todo!(),
        cst::Declaration::ForeignImportValueDeclaration(v) => todo!(),
        cst::Declaration::NewtypeSignature(s) => todo!(),
        cst::Declaration::NewtypeEquation(e) => todo!(),
        cst::Declaration::DataSignature(s) => todo!(),
        cst::Declaration::DataEquation(e) => todo!(),
        cst::Declaration::DeriveDeclaration(d) => todo!(),
    }
}

fn index_value_signature(state: &mut State, signature: cst::ValueSignature) {
    let name_token = signature.name_token();

    let declaration = cst::Declaration::ValueSignature(signature);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    state.value_signature(name, declaration_id);
}

fn index_value_equation(state: &mut State, equation: cst::ValueEquation) {
    let name_token = equation.name_token();

    let declaration = cst::Declaration::ValueEquation(equation);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    state.value_equation(name, declaration_id);
}

fn index_synonym_signature(state: &mut State, signature: cst::TypeSynonymSignature) {
    let name_token = signature.name_token();

    let declaration = cst::Declaration::TypeSynonymSignature(signature);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    state.synonym_signature(name, declaration_id);
}

fn index_synonym_equation(state: &mut State, equation: cst::TypeSynonymEquation) {
    let name_token = equation.name_token();

    let declaration = cst::Declaration::TypeSynonymEquation(equation);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    state.synonym_equation(name, declaration_id);
}

fn index_class_signature(state: &mut State, signature: cst::ClassSignature) {
    let name_token = signature.name_token();

    let declaration = cst::Declaration::ClassSignature(signature);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    state.class_signature(name, declaration_id);
}

fn index_class_declaration(state: &mut State, declaration: cst::ClassDeclaration) {
    let class_head = declaration.class_head();
    let class_statements = declaration.class_statements();

    let declaration = cst::Declaration::ClassDeclaration(declaration);
    let declaration_id = state.source_map.insert_declaration(&declaration);

    if let Some(class_head) = class_head {
        if let Some(name_token) = class_head.name_token() {
            let name = name_token.text();
            state.class_declaration(name, declaration_id);
        }
    }

    if let Some(class_statements) = class_statements {
        for class_member in class_statements.children() {
            index_class_member(state, class_member);
        }
    }
}

fn index_class_member(state: &mut State, member: cst::ClassMemberStatement) {
    let name_token = member.name_token();
    let member_id = state.source_map.insert_class_member(&member);

    let Some(name_token) = name_token else { return };
    let name = name_token.text();

    state.class_member(name, member_id);
}

// endregion: Traversals
