use rowan::ast::{AstNode, SyntaxNodePtr};
use smol_str::SmolStr;
use syntax::{cst, SyntaxNode};

use crate::{DeclarationGroup, IndexingError, FullIndexingResult};

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

    let Some(name_token) = name_token else {
        return;
    };

    let pointer = SyntaxNodePtr::new(declaration.syntax());
    let index = module_map.arena.alloc(declaration);
    module_map.pointer.insert(index, pointer);

    let name = name_token.text();
    match module_map.value.get_mut(name) {
        Some(group) => {
            if is_signature {
                // Signature is declared after equation.
                if let &[equation, ..] = &group.declarations[..] {
                    let error = IndexingError::SignatureIsLate { equation, signature: index };
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
                group.declarations.push(index);
            }
        }
        None => {
            let signature = if is_signature { Some(index) } else { None };
            let declarations = if is_signature { vec![] } else { vec![index] };

            let name: SmolStr = name.into();
            let group = DeclarationGroup { signature, declarations };

            module_map.value.insert(name, group);
        }
    }
}
