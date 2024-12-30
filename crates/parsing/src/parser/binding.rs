use syntax::{SyntaxKind, TokenSet};

use super::{binders, expressions, generic, Parser};

const LET_BINDING_START: TokenSet = TokenSet::new(&[SyntaxKind::LOWER])
    .union(expressions::EXPRESSION_START)
    .union(binders::BINDER_ATOM_START);

pub(super) fn let_binding_statements(p: &mut Parser) {
    let mut m = p.start();
    p.expect(SyntaxKind::LAYOUT_START);
    while p.at_in(LET_BINDING_START) && !p.at_eof() {
        p.alternative([let_binding_signature_or_equation, let_binding_pattern]);
        while !p.at(SyntaxKind::LAYOUT_SEPARATOR) && !p.at(SyntaxKind::LAYOUT_END) && !p.at_eof() {
            p.error_recover("Invalid token");
        }
        if !p.at(SyntaxKind::LAYOUT_END) {
            p.expect(SyntaxKind::LAYOUT_SEPARATOR);
        }
    }
    p.expect(SyntaxKind::LAYOUT_END);
    m.end(p, SyntaxKind::LetBindingStatements);
}

fn let_binding_signature_or_equation(p: &mut Parser) {
    generic::signature_or_equation(
        p,
        SyntaxKind::LetBindingSignature,
        SyntaxKind::LetBindingEquation,
    );
}

fn let_binding_pattern(p: &mut Parser) {
    let mut m = p.start();
    binders::binder_atom(p);
    p.expect(SyntaxKind::EQUAL);
    expressions::expression(p);
    m.end(p, SyntaxKind::LetBindingPattern);
}
