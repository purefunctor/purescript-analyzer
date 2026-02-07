use syntax::SyntaxKind;

pub(super) use syntax::names::{KEYWORD, LOWER, OPERATOR, OPERATOR_NAME, RECORD_LABEL, ROLE};

use super::Parser;

pub(super) fn module_name(p: &mut Parser) {
    p.annotate();
    let mut m = p.start();
    p.qualify();
    p.expect(SyntaxKind::UPPER);
    m.end(p, SyntaxKind::ModuleName);
}

pub(super) fn lower(p: &mut Parser) {
    p.annotate();
    let mut m = p.start();
    p.qualify();
    p.expect_in(LOWER, SyntaxKind::LOWER, "Expected LOWER");
    m.end(p, SyntaxKind::QualifiedName);
}

pub(super) fn upper(p: &mut Parser) {
    p.annotate();
    let mut m = p.start();
    p.qualify();
    p.expect(SyntaxKind::UPPER);
    m.end(p, SyntaxKind::QualifiedName);
}

pub(super) fn operator(p: &mut Parser) {
    p.annotate();
    let mut m = p.start();
    p.qualify();
    p.expect_in(OPERATOR, SyntaxKind::OPERATOR, "Expected OPERATOR");
    m.end(p, SyntaxKind::QualifiedName)
}

pub(super) fn operator_name(p: &mut Parser) {
    p.annotate();
    let mut m = p.start();
    p.qualify();
    p.expect_in(OPERATOR_NAME, SyntaxKind::OPERATOR_NAME, "Expected OPERATOR_NAME");
    m.end(p, SyntaxKind::QualifiedName);
}

pub(super) fn label(p: &mut Parser) {
    let mut m = p.start();
    p.expect_in(RECORD_LABEL, SyntaxKind::TEXT, "Expected RECORD_LABEL");
    m.end(p, SyntaxKind::LabelName);
}
