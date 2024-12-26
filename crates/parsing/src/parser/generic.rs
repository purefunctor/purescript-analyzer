use syntax::SyntaxKind;

use super::{names::LOWER_NON_RESERVED, Parser};

pub(super) fn record_item(p: &mut Parser, k: impl Fn(&mut Parser)) {
    let mut m = p.start();

    p.expect_in(LOWER_NON_RESERVED, SyntaxKind::LOWER, "Expected LOWER_NON_RESERVED");
    if p.at(SyntaxKind::COMMA) || p.at(SyntaxKind::RIGHT_CURLY) {
        return m.end(p, SyntaxKind::RecordPun);
    }

    p.expect(SyntaxKind::COLON);
    k(p);
    m.end(p, SyntaxKind::RecordField);
}
