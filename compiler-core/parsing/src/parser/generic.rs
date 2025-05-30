use syntax::{SyntaxKind, TokenSet};

use super::{binders, binding, expressions, names, types, Parser};

pub(super) fn record_item(p: &mut Parser, k: impl Fn(&mut Parser)) {
    let mut m = p.start();

    if (p.at(SyntaxKind::STRING) || p.at(SyntaxKind::RAW_STRING)) && !p.at_next(SyntaxKind::COLON) {
        p.error("Invalid string label in record pun");
    }

    if p.at_in(names::KEYWORD) && !p.at_next(SyntaxKind::COLON) {
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

const EQUATION_BINDERS_END: TokenSet = TokenSet::new(&[SyntaxKind::EQUAL, SyntaxKind::PIPE]);

pub(super) fn signature_or_equation(p: &mut Parser, s: SyntaxKind, e: SyntaxKind) {
    let mut m = p.start();
    p.expect_in(names::LOWER, SyntaxKind::LOWER, "Expected LOWER");
    if p.eat(SyntaxKind::DOUBLE_COLON) {
        types::type_(p);
        m.end(p, s);
    } else {
        function_binders(p, EQUATION_BINDERS_END);
        unconditional_or_conditionals(p, SyntaxKind::EQUAL);
        m.end(p, e);
    }
}

const FUNCTION_BINDERS_RECOVERY: TokenSet =
    TokenSet::new(&[SyntaxKind::LAYOUT_SEPARATOR, SyntaxKind::LAYOUT_END]);

pub(super) fn function_binders(p: &mut Parser, s: TokenSet) {
    let mut m = p.start();
    while !p.at_in(s) && !p.at_eof() {
        if p.at_in(binders::BINDER_ATOM_START) {
            binders::binder_atom(p);
        } else {
            if p.at_in(FUNCTION_BINDERS_RECOVERY) {
                break;
            }
            p.error_recover("Unexpected token in function binders");
        }
    }
    m.end(p, SyntaxKind::FunctionBinders);
}

pub(super) fn unconditional_or_conditionals(p: &mut Parser, s: SyntaxKind) {
    let mut m = p.start();
    if p.at(SyntaxKind::PIPE) {
        conditionals(p, s);
        m.end(p, SyntaxKind::Conditionals);
    } else {
        p.expect(s);
        where_expression(p);
        m.end(p, SyntaxKind::Unconditional);
    }
}

pub(super) fn where_expression(p: &mut Parser) {
    let mut m = p.start();
    expressions::expression(p);
    if p.eat(SyntaxKind::WHERE) {
        binding::let_binding_statements(p);
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
            p.error_recover("Unexpected token in pattern guards");
        }
    }
}

const PATTERN_GUARD_START: TokenSet = binders::BINDER_START.union(expressions::EXPRESSION_START);

const PATTERN_GUARD_RECOVERY: TokenSet =
    TokenSet::new(&[SyntaxKind::LAYOUT_SEPARATOR, SyntaxKind::LAYOUT_END]);

fn pattern_guard(p: &mut Parser) {
    p.alternative([pattern_guard_binder, pattern_guard_expression]);
}

fn pattern_guard_binder(p: &mut Parser) {
    let mut m = p.start();
    binders::binder(p);
    p.expect(SyntaxKind::LEFT_ARROW);
    expressions::expression(p);
    m.end(p, SyntaxKind::PatternGuardBinder);
}

fn pattern_guard_expression(p: &mut Parser) {
    let mut m = p.start();
    expressions::expression(p);
    m.end(p, SyntaxKind::PatternGuardExpression);
}
