use syntax::{SyntaxKind, TokenSet};

use super::{binders, binding, expressions, names, types, Parser};

pub(super) fn record_item(p: &mut Parser, k: impl Fn(&mut Parser)) {
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
    k(p);

    m.end(p, SyntaxKind::RecordField);
}

pub(super) fn annotation_or_equation(p: &mut Parser, a: SyntaxKind, e: SyntaxKind, s: SyntaxKind) {
    let mut m = p.start();
    p.expect_in(names::LOWER_NON_RESERVED, SyntaxKind::LOWER, "Expected LOWER_NON_RESERVED");
    if p.eat(SyntaxKind::DOUBLE_COLON) {
        types::ty(p);
        m.end(p, a);
    } else {
        equation_binders(p, s);
        unconditional_or_conditionals(p, s);
        m.end(p, e);
    }
}

const EQUATION_BINDERS_RECOVERY: TokenSet =
    TokenSet::new(&[SyntaxKind::LAYOUT_SEPARATOR, SyntaxKind::LAYOUT_END]);

pub(super) fn equation_binders(p: &mut Parser, s: SyntaxKind) {
    let mut m = p.start();
    while !p.at(s) && !p.at(SyntaxKind::PIPE) && !p.at_eof() {
        if p.at_in(binders::BINDER_ATOM_START) {
            binders::binder_atom(p);
        } else {
            if p.at_in(EQUATION_BINDERS_RECOVERY) {
                break;
            }
            p.error_recover("Invalid token");
        }
    }
    m.end(p, SyntaxKind::EquationBinders);
}

pub(super) fn unconditional_or_conditionals(p: &mut Parser, s: SyntaxKind) {
    let mut m = p.start();
    if p.eat(s) {
        where_expression(p);
        m.end(p, SyntaxKind::Unconditional);
    } else if p.at(SyntaxKind::PIPE) {
        conditionals(p, s);
        m.end(p, SyntaxKind::Conditionals);
    } else {
        m.cancel(p);
    }
}

fn where_expression(p: &mut Parser) {
    let mut m = p.start();
    expressions::expression(p);
    if p.eat(SyntaxKind::WHERE) {
        binding::bindings(p);
    }
    m.end(p, SyntaxKind::WhereExpression);
}

fn conditionals(p: &mut Parser, s: SyntaxKind) {
    while p.at(SyntaxKind::PIPE) && !p.at_eof() {
        pattern_guarded(p, s);
    }
}

fn pattern_guarded(p: &mut Parser, s: SyntaxKind) {
    let mut m = p.start();
    p.expect(SyntaxKind::PIPE);
    pattern_guards(p, s);
    p.expect(s);
    where_expression(p);
    m.end(p, SyntaxKind::PatternGuarded);
}

fn pattern_guards(p: &mut Parser, s: SyntaxKind) {
    while !p.at(s) && !p.at_eof() {
        if p.at_in(PATTERN_GUARD_START) {
            pattern_guard(p);
            if p.at(SyntaxKind::COMMA) && p.at_next(s) {
                p.error_recover("Trailing comma");
            } else if !p.at(s) {
                p.expect(SyntaxKind::COMMA);
            }
        } else {
            if p.at(s) || p.at_in(PATTERN_GUARD_RECOVERY) {
                break;
            }
            p.error_recover("Invalid token");
        }
    }
}

const PATTERN_GUARD_START: TokenSet =
    binders::BINDER_ATOM_START.union(expressions::EXPRESSION_START);

const PATTERN_GUARD_RECOVERY: TokenSet =
    TokenSet::new(&[SyntaxKind::LAYOUT_SEPARATOR, SyntaxKind::LAYOUT_END]);

fn pattern_guard(p: &mut Parser) {
    let c = p.checkpoint();
    let binder = c.branch(p, pattern_guard_binder);
    let expression = c.branch(p, pattern_guard_expression);
    p.decide([binder, expression]);
}

fn pattern_guard_binder(p: &mut Parser) {
    let mut m = p.start();
    binders::binder_atom(p);
    p.expect(SyntaxKind::LEFT_ARROW);
    expressions::expression(p);
    m.end(p, SyntaxKind::PatternGuardBinder);
}

fn pattern_guard_expression(p: &mut Parser) {
    let mut m = p.start();
    expressions::expression(p);
    m.end(p, SyntaxKind::PatternGuardExpression);
}
