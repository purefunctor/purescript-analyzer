use syntax::{SyntaxKind, TokenSet};

use super::Parser;

pub(super) const LOWER: TokenSet =
    TokenSet::new(&[SyntaxKind::LOWER, SyntaxKind::AS, SyntaxKind::HIDING, SyntaxKind::ROLE])
        .union(ROLE);

pub(super) const ROLE: TokenSet =
    TokenSet::new(&[SyntaxKind::NOMINAL, SyntaxKind::PHANTOM, SyntaxKind::REPRESENTATIONAL]);

pub(super) const OPERATOR: TokenSet = TokenSet::new(&[
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

    p.eat(SyntaxKind::PREFIX);
    p.expect(SyntaxKind::UPPER);

    m.end(p, SyntaxKind::ModuleName);
}

pub(super) fn lower(p: &mut Parser) {
    let mut m = p.start();

    p.eat(SyntaxKind::PREFIX);
    p.expect(SyntaxKind::LOWER);

    m.end(p, SyntaxKind::QualifiedName);
}

pub(super) fn upper(p: &mut Parser) {
    let mut m = p.start();

    p.eat(SyntaxKind::PREFIX);
    p.expect(SyntaxKind::UPPER);

    m.end(p, SyntaxKind::QualifiedName);
}

pub(super) fn at_operator(p: &Parser) -> bool {
    p.at_in(OPERATOR) || p.at(SyntaxKind::PREFIX) && OPERATOR.contains(p.nth(1))
}

pub(super) fn operator(p: &mut Parser) -> bool {
    let mut m = p.start();

    p.eat(SyntaxKind::PREFIX);
    if p.eat_in(OPERATOR, SyntaxKind::OPERATOR) {
        m.end(p, SyntaxKind::QualifiedName);
        true
    } else {
        m.cancel(p);
        false
    }
}

pub(super) fn operator_name(p: &mut Parser) {
    let mut m = p.start();

    p.eat(SyntaxKind::PREFIX);
    p.expect_in(OPERATOR_NAME, SyntaxKind::OPERATOR_NAME, "Expected OPERATOR_NAME");

    m.end(p, SyntaxKind::QualifiedName);
}

pub(super) const KEYWORD: TokenSet = TokenSet::new(&[
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
    SyntaxKind::TRUE,
    SyntaxKind::FALSE,
]);

pub(super) const RECORD_LABEL: TokenSet =
    TokenSet::new(&[SyntaxKind::STRING, SyntaxKind::RAW_STRING]).union(LOWER).union(KEYWORD);

pub(super) fn label(p: &mut Parser) {
    let mut m = p.start();

    // Unlike parsing for `LOWER`, which consumes tokens as
    // the `LOWER` token, we use a `LabelName` node to represent labels.
    if p.at(SyntaxKind::STRING) || p.at(SyntaxKind::RAW_STRING) || p.at_in(RECORD_LABEL) {
        p.consume();
    } else {
        p.error("Expected STRING, RAW_STRING or RECORD_LABEL");
    };

    m.end(p, SyntaxKind::LabelName);
}
