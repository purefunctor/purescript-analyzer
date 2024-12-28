use syntax::{SyntaxKind, TokenSet};

use super::{binders, binding, expressions, names, types, Parser};

pub(super) enum RecordItemKind {
    Binder,
    Expression,
}

pub(super) fn record_item(p: &mut Parser, k: RecordItemKind) {
    let mut m = p.start();

    if (p.at(SyntaxKind::STRING) || p.at(SyntaxKind::RAW_STRING)) && !p.at_next(SyntaxKind::COLON) {
        p.error("Invalid string label in record pun");
    }

    if p.at_in(names::RESERVED_KEYWORD) && !p.at_next(SyntaxKind::COLON) {
        p.error("Invalid reserved keyword in record pun");
    }

    names::label(p);

    if p.at(SyntaxKind::COMMA) || p.at(SyntaxKind::RIGHT_CURLY) {
        return m.end(p, SyntaxKind::RecordPun);
    }

    p.expect(SyntaxKind::COLON);
    match k {
        RecordItemKind::Binder => binders::binder(p),
        RecordItemKind::Expression => expressions::expression(p),
    }

    m.end(p, SyntaxKind::RecordField);
}

pub(super) fn annotation_or_equation(p: &mut Parser, a: SyntaxKind, e: SyntaxKind, s: SyntaxKind) {
    let mut m = p.start();
    p.expect_in(names::LOWER_NON_RESERVED, SyntaxKind::LOWER, "Expected LOWER_NON_RESERVED");
    if p.eat(SyntaxKind::DOUBLE_COLON) {
        types::ty(p);
        m.end(p, a);
    } else {
        binders_list(p, s);
        equation_guarded(p, s);
        m.end(p, e);
    }
}

const EQUATION_BINDERS_RECOVERY: TokenSet =
    TokenSet::new(&[SyntaxKind::LAYOUT_SEPARATOR, SyntaxKind::LAYOUT_END]);

pub(super) fn binders_list(p: &mut Parser, s: SyntaxKind) {
    let mut m = p.start();
    while !p.at(s) && !p.at(SyntaxKind::PIPE) && !p.at_eof() {
        if p.at_in(binders::BINDER_ATOM_START) {
            let m = p.start();
            binders::binder_atom(p, m);
        } else {
            if p.at_in(EQUATION_BINDERS_RECOVERY) {
                break;
            }
            p.error_recover("Invalid token");
        }
    }
    m.end(p, SyntaxKind::BindersList);
}

fn equation_guarded(p: &mut Parser, s: SyntaxKind) {
    let mut m = p.start();
    if p.eat(s) {
        equation_where(p);
        m.end(p, SyntaxKind::EquationUnguarded);
    } else if p.at(SyntaxKind::PIPE) {
        m.end(p, SyntaxKind::EquationGuarded);
        todo!("EquationGuarded");
    } else {
        m.cancel(p);
    }
}

fn equation_where(p: &mut Parser) {
    let mut m = p.start();
    expressions::expression(p);
    if p.eat(SyntaxKind::WHERE) {
        binding::bindings(p);
    }
    m.end(p, SyntaxKind::EquationWhere);
}
