use syntax::{SyntaxKind, TokenSet};

use super::Parser;

pub(super) const LOWER_NON_RESERVED: TokenSet =
    TokenSet::new(&[SyntaxKind::LOWER, SyntaxKind::AS, SyntaxKind::HIDING]);

pub(super) const OPERATOR_NON_RESERVED: TokenSet = TokenSet::new(&[
    SyntaxKind::OPERATOR,
    SyntaxKind::COLON,
    SyntaxKind::MINUS,
    SyntaxKind::DOUBLE_PERIOD,
    SyntaxKind::LEFT_THICK_ARROW,
]);

pub(super) fn module_name(p: &mut Parser) {
    let mut m = p.start();

    if p.at(SyntaxKind::PREFIX) {
        p.consume();
    }
    p.expect(SyntaxKind::UPPER);

    m.end(p, SyntaxKind::ModuleName);
}
