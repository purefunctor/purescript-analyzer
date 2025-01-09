use rowan::ast::AstNode;
use smol_str::SmolStr;
use syntax::{cst, SyntaxNode};

use crate::{
    ClassGroup, DeclarationId, DeclarationPtr, FullIndexingResult, IndexingError,
    InstanceStatementGroup, SynonymGroup, ValueGroup,
};

impl FullIndexingResult {
    fn allocate_declaration(&mut self, node: cst::Declaration) -> DeclarationId {
        let pointer = DeclarationPtr::new(&node);
        let index = self.arena.alloc(node);

        let insert = self.pointer.insert_full(pointer);
        debug_assert_eq!(insert, (index.into_raw().into_u32() as usize, true));

        index
    }
}

pub(super) fn index_module(module_map: &mut FullIndexingResult, node: SyntaxNode) {
    let Some(module) = cst::Module::cast(node) else {
        return;
    };
    let Some(statements) = module.statements() else {
        return;
    };
    for declaration in statements.children() {
        index_declaration(module_map, declaration);
    }
}

fn index_declaration(module_map: &mut FullIndexingResult, declaration: cst::Declaration) {
    match declaration {
        cst::Declaration::ValueSignature(s) => index_value(module_map, IndexValue::Signature(s)),
        cst::Declaration::ValueEquation(e) => index_value(module_map, IndexValue::Equation(e)),
        cst::Declaration::InstanceChain(c) => index_instance_chain(module_map, c),
        cst::Declaration::TypeSynonymSignature(s) => index_synonym_signature(module_map, s),
        cst::Declaration::TypeSynonymEquation(e) => index_synonym_equation(module_map, e),
        cst::Declaration::ClassSignature(s) => index_class_signature(module_map, s),
        cst::Declaration::ClassDeclaration(e) => index_class_declaration(module_map, e),
        _ => (),
    }
}

enum IndexValue {
    Signature(cst::ValueSignature),
    Equation(cst::ValueEquation),
}

fn index_value(module_map: &mut FullIndexingResult, signature_or_equation: IndexValue) {
    let (name_token, declaration, is_signature) = match signature_or_equation {
        IndexValue::Signature(s) => (s.name_token(), cst::Declaration::ValueSignature(s), true),
        IndexValue::Equation(e) => (e.name_token(), cst::Declaration::ValueEquation(e), false),
    };

    let index = module_map.allocate_declaration(declaration);

    let Some(name_token) = name_token else {
        return;
    };

    let name = name_token.text();
    match module_map.value.get_mut(name) {
        Some(group) => {
            if is_signature {
                // Signature is declared after equation.
                if let &[declaration, ..] = &group.equations[..] {
                    let error = IndexingError::SignatureIsLate { declaration, signature: index };
                    module_map.errors.push(error);
                }
                // Signature is declared twice.
                if let Some(existing) = group.signature {
                    let error = IndexingError::SignatureConflict { existing, duplicate: index };
                    module_map.errors.push(error);
                } else {
                    group.signature = Some(index);
                }
            } else {
                group.equations.push(index);
            }
        }
        None => {
            let signature = if is_signature { Some(index) } else { None };
            let equations = if is_signature { vec![] } else { vec![index] };

            let name: SmolStr = name.into();
            let group = ValueGroup { signature, equations };

            module_map.value.insert(name, group);
        }
    }
}

fn index_instance_chain(module_map: &mut FullIndexingResult, instance_chain: cst::InstanceChain) {
    let instance_declarations = instance_chain.instance_declarations();

    let declaration = cst::Declaration::InstanceChain(instance_chain);
    let chain_index = module_map.allocate_declaration(declaration);

    for instance_declaration in instance_declarations {
        let declaration_index = index_instance_declaration(module_map, instance_declaration);
        module_map.instance.instance_graph.add_edge(chain_index, declaration_index, ());
    }
}

fn index_instance_declaration(
    module_map: &mut FullIndexingResult,
    instance_declaration: cst::InstanceDeclaration,
) -> DeclarationId {
    let name = instance_declaration.instance_name();
    let head = instance_declaration.instance_head();
    let statements = instance_declaration.instance_statements();

    let declaration = cst::Declaration::InstanceDeclaration(instance_declaration);
    let declaration_index = module_map.allocate_declaration(declaration);

    if let Some(name) = name.and_then(|n| n.name_token()) {
        let name = name.text().into();
        match module_map.instance.by_name.get_mut(name) {
            Some(existing) => {
                let error = IndexingError::DeclarationConflict {
                    existing: *existing,
                    duplicate: declaration_index,
                };
                module_map.errors.push(error);
            }
            None => {
                let name: SmolStr = name.into();
                module_map.instance.by_name.insert(name, declaration_index);
            }
        }
    }

    if let Some(head) = head.and_then(|h| h.type_name_token()) {
        let head: SmolStr = head.text().into();
        module_map.instance.by_type.entry(head).or_default().push(declaration_index);
    }

    if let Some(statements) = statements {
        let mut statement_group = InstanceStatementGroup { signature: None, equations: vec![] };
        for statement in statements.children() {
            let statement_index =
                index_instance_statement(module_map, &mut statement_group, statement);
            module_map.instance.statement_graph.add_edge(declaration_index, statement_index, ());
        }
        module_map.instance.statement_group.insert(declaration_index, statement_group);
    }

    declaration_index
}

fn index_instance_statement(
    module_map: &mut FullIndexingResult,
    statement_group: &mut InstanceStatementGroup,
    instance_statement: cst::InstanceDeclarationStatement,
) -> DeclarationId {
    let (declaration, is_signature) = match instance_statement {
        cst::InstanceDeclarationStatement::InstanceSignatureStatement(s) => {
            (cst::Declaration::InstanceSignatureStatement(s), true)
        }
        cst::InstanceDeclarationStatement::InstanceEquationStatement(e) => {
            (cst::Declaration::InstanceEquationStatement(e), false)
        }
    };

    let statement_index = module_map.allocate_declaration(declaration);

    if is_signature {
        // Signature is declared after equation.
        if let &[declaration, ..] = &statement_group.equations[..] {
            let error = IndexingError::SignatureIsLate { declaration, signature: statement_index };
            module_map.errors.push(error);
        }
        // Signature is declared twice.
        if let Some(existing) = statement_group.signature {
            let error = IndexingError::SignatureConflict { existing, duplicate: statement_index };
            module_map.errors.push(error);
        } else {
            statement_group.signature = Some(statement_index);
        }
    } else {
        statement_group.equations.push(statement_index);
    }

    statement_index
}

fn index_synonym_signature(
    module_map: &mut FullIndexingResult,
    signature: cst::TypeSynonymSignature,
) {
    let name_token = signature.name_token();

    let declaration = cst::Declaration::TypeSynonymSignature(signature);
    let signature_index = module_map.allocate_declaration(declaration);

    let Some(name_token) = name_token else {
        return;
    };

    let name = name_token.text();
    match module_map.synonym.get_mut(name) {
        Some(group) => {
            // Signature is declared after equation.
            if let Some(declaration) = group.equation {
                let error =
                    IndexingError::SignatureIsLate { declaration, signature: signature_index };
                module_map.errors.push(error);
            }
            // Signature is declared twice.
            if let Some(existing) = group.signature {
                let error =
                    IndexingError::SignatureConflict { existing, duplicate: signature_index };
                module_map.errors.push(error);
            } else {
                group.signature = Some(signature_index);
            }
        }
        None => {
            let name: SmolStr = name.into();
            let group = SynonymGroup { signature: Some(signature_index), equation: None };
            module_map.synonym.insert(name, group);
        }
    }
}

fn index_synonym_equation(module_map: &mut FullIndexingResult, equation: cst::TypeSynonymEquation) {
    let name_token = equation.name_token();

    let declaration = cst::Declaration::TypeSynonymEquation(equation);
    let equation_index = module_map.allocate_declaration(declaration);

    let Some(name_token) = name_token else {
        return;
    };

    let name = name_token.text();
    match module_map.synonym.get_mut(name) {
        Some(group) => {
            // Equation is declared twice.
            if let Some(existing) = group.equation {
                let error =
                    IndexingError::DeclarationConflict { existing, duplicate: equation_index };
                module_map.errors.push(error);
            } else {
                group.equation = Some(equation_index);
            }
        }
        None => {
            let name: SmolStr = name.into();
            let group = SynonymGroup { signature: None, equation: Some(equation_index) };
            module_map.synonym.insert(name, group);
        }
    }
}

fn index_class_signature(module_map: &mut FullIndexingResult, signature: cst::ClassSignature) {
    let name_token = signature.name_token();

    let declaration = cst::Declaration::ClassSignature(signature);
    let index = module_map.allocate_declaration(declaration);

    let Some(name_token) = name_token else {
        return;
    };

    let name = name_token.text();
    match module_map.class.by_type.get_mut(name) {
        Some(group) => {
            // Signature is declared after declaration.
            if let Some(declaration) = group.declaration {
                let error = IndexingError::SignatureIsLate { declaration, signature: index };
                module_map.errors.push(error);
            }
            // Signature is declared twice.
            if let Some(existing) = group.signature {
                let error = IndexingError::SignatureConflict { existing, duplicate: index };
                module_map.errors.push(error);
            } else {
                group.signature = Some(index);
            }
        }
        None => {
            let name: SmolStr = name.into();
            let group = ClassGroup { signature: Some(index), declaration: None };
            module_map.class.by_type.insert(name, group);
        }
    }
}

fn index_class_declaration(
    module_map: &mut FullIndexingResult,
    declaration: cst::ClassDeclaration,
) {
    let head = declaration.class_head();
    let statements = declaration.class_statements();

    let declaration = cst::Declaration::ClassDeclaration(declaration);
    let declaration_index = module_map.allocate_declaration(declaration);

    if let Some(name) = head.and_then(|h| h.name_token()) {
        let name = name.text();
        match module_map.class.by_type.get_mut(name) {
            Some(group) => {
                if let Some(existing) = group.declaration {
                    let error = IndexingError::DeclarationConflict {
                        existing,
                        duplicate: declaration_index,
                    };
                    module_map.errors.push(error);
                } else {
                    group.declaration = Some(declaration_index);
                }
            }
            None => {
                let name: SmolStr = name.into();
                let group = ClassGroup { signature: None, declaration: Some(declaration_index) };
                module_map.class.by_type.insert(name, group);
            }
        }
    }

    if let Some(statements) = statements {
        for statement in statements.children() {
            let statement_index = index_class_statement(module_map, statement);
            module_map.class.statement_graph.add_edge(declaration_index, statement_index, ());
        }
    }
}

fn index_class_statement(
    module_map: &mut FullIndexingResult,
    class_statement: cst::ClassDeclarationStatement,
) -> DeclarationId {
    let (name_token, class_statement) = match class_statement {
        cst::ClassDeclarationStatement::ClassMemberStatement(s) => (s.name_token(), s),
    };

    let declaration = cst::Declaration::ClassMemberStatement(class_statement);
    let statement_index = module_map.allocate_declaration(declaration);

    let Some(name_token) = name_token else {
        return statement_index;
    };

    let name = name_token.text();
    match module_map.class.by_member.get_mut(name) {
        Some(existing) => {
            let error = IndexingError::DeclarationConflict {
                existing: *existing,
                duplicate: statement_index,
            };
            module_map.errors.push(error);
        }
        None => {
            let name: SmolStr = name.into();
            module_map.class.by_member.insert(name, statement_index);
        }
    }

    statement_index
}
