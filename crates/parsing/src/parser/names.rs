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

pub(super) const OPERATOR_NAME: TokenSet =
    TokenSet::new(&[SyntaxKind::OPERATOR_NAME, SyntaxKind::DOUBLE_PERIOD_OPERATOR_NAME]);

pub(super) fn module_name(p: &mut Parser) {
    let mut m = p.start();

    if p.at(SyntaxKind::PREFIX) {
        p.consume();
    }
    p.expect(SyntaxKind::UPPER);

    m.end(p, SyntaxKind::ModuleName);
}

pub(super) const RESERVED_KEYWORD: TokenSet = TokenSet::new(&[
    SyntaxKind::MODULE,
    SyntaxKind::WHERE,
    SyntaxKind::IMPORT,
    SyntaxKind::ADO,
    SyntaxKind::DO,
    SyntaxKind::IF,
    SyntaxKind::THEN,
    SyntaxKind::ELSE,
    SyntaxKind::LET,
    SyntaxKind::IN,
    SyntaxKind::CASE,
    SyntaxKind::OF,
    SyntaxKind::DATA,
    SyntaxKind::NEWTYPE,
    SyntaxKind::FORALL,
    SyntaxKind::TYPE,
    SyntaxKind::CLASS,
    SyntaxKind::INSTANCE,
    SyntaxKind::DERIVE,
    SyntaxKind::FOREIGN,
    SyntaxKind::INFIXL,
    SyntaxKind::INFIXR,
    SyntaxKind::INFIX,
]);

pub(super) const RECORD_LABEL: TokenSet =
    TokenSet::new(&[SyntaxKind::STRING, SyntaxKind::RAW_STRING])
        .union(LOWER_NON_RESERVED)
        .union(RESERVED_KEYWORD);

pub(super) fn label(p: &mut Parser) {
    let mut m = p.start();

    // Unlike parsing for `LOWER_NON_RESERVED`, which consumes tokens as
    // the `LOWER` token, we use a `LabelName` node to represent labels.
    if p.at(SyntaxKind::STRING) || p.at(SyntaxKind::RAW_STRING) || p.at_in(RECORD_LABEL) {
        p.consume();
    } else {
        p.error("Expected STRING, RAW_STRING or RECORD_LABEL");
    };

    m.end(p, SyntaxKind::LabelName);
}
