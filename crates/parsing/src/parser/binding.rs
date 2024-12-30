use syntax::SyntaxKind;

use super::{generic, names, Parser};

pub(super) fn bindings(p: &mut Parser) {
    let mut m = p.start();
    p.expect(SyntaxKind::LAYOUT_START);
    while p.at_in(names::LOWER_NON_RESERVED) && !p.at_eof() {
        generic::signature_or_equation(
            p,
            SyntaxKind::LetBindingSignature,
            SyntaxKind::LetBindingEquation,
        );
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
