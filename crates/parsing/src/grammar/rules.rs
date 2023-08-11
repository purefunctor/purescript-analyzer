use syntax::SyntaxKind;

use crate::parser::{NodeMarker, Parser};

use super::combinators::{attempt, layout_one_or_more, one_or_more, separated, zero_or_more};

// expr_1 '::' type_0 | expr_1
pub(super) fn expr_0(parser: &mut Parser) {
    let mut marker = parser.start();
    expr_1(parser);
    if parser.at(SyntaxKind::Colon2) {
        parser.consume();
        type_0(parser);
        marker.end(parser, SyntaxKind::TypedExpression)
    } else {
        marker.cancel(parser);
    }
}

fn at_operator_start(parser: &Parser) -> bool {
    if parser.current().is_end() {
        false
    } else {
        parser.current().is_operator()
    }
}

// expr_2 ('operator' expr_2) + | expr_2
fn expr_1(parser: &mut Parser) {
    let mut marker = parser.start();
    expr_2(parser);
    let at_least_one = one_or_more(parser, |parser| {
        if at_operator_start(parser) {
            let mut marker = parser.start();
            // FIXME: mark as NameRef to make it subject to name resolution...
            parser.consume_as(SyntaxKind::Operator);
            expr_2(parser);
            marker.end(parser, SyntaxKind::Pair);
            true
        } else {
            false
        }
    });
    if at_least_one {
        marker.end(parser, SyntaxKind::ExpressionOperatorChain);
    } else {
        marker.cancel(parser);
    }
}

fn at_tick_expr_start(parser: &Parser) -> bool {
    if parser.current().is_end() {
        false
    } else {
        parser.at(SyntaxKind::Tick)
    }
}

// expr_3 (`tick_expr` expr_3) + | expr_3
fn expr_2(parser: &mut Parser) {
    let mut marker = parser.start();
    expr_3(parser);
    let at_least_one = one_or_more(parser, |parser| {
        if at_tick_expr_start(parser) {
            let mut marker = parser.start();
            tick_expr(parser);
            expr_3(parser);
            marker.end(parser, SyntaxKind::Pair);
            true
        } else {
            false
        }
    });
    if at_least_one {
        marker.end(parser, SyntaxKind::ExpressionInfixChain);
    } else {
        marker.cancel(parser);
    }
}

// '`' tick_expr_1 '`'
fn tick_expr(parser: &mut Parser) {
    let mut marker = parser.start();
    parser.expect(SyntaxKind::Tick);
    tick_expr_1(parser);
    parser.expect(SyntaxKind::Tick);
    marker.end(parser, SyntaxKind::Wrapped);
}

// expr_3 ('operator' expr_3) | expr_3
fn tick_expr_1(parser: &mut Parser) {
    let mut marker = parser.start();
    expr_3(parser);
    let at_least_one = one_or_more(parser, |parser| {
        if at_operator_start(parser) {
            let mut marker = parser.start();
            // FIXME: mark as NameRef to make it subject to name resolution...
            parser.consume_as(SyntaxKind::Operator);
            expr_3(parser);
            marker.end(parser, SyntaxKind::Pair);
            true
        } else {
            false
        }
    });
    if at_least_one {
        marker.end(parser, SyntaxKind::ExpressionOperatorChain);
    } else {
        marker.cancel(parser);
    }
}

// '-' expr_3 | expr_4
fn expr_3(parser: &mut Parser) {
    let mut marker = parser.start();
    if parser.at(SyntaxKind::Minus) {
        // FIXME: make this a NameRef as well??
        parser.consume_as(SyntaxKind::Operator);
        expr_3(parser);
        marker.end(parser, SyntaxKind::NegateExpression);
    } else {
        expr_4(parser);
        marker.cancel(parser);
    }
}

/// Returns `true` if we're at the beginning of an expression.
fn at_expr_start(parser: &Parser) -> bool {
    if parser.current().is_end() {
        return false;
    }
    matches!(
        parser.current(),
        SyntaxKind::IfKw
            | SyntaxKind::CaseKw
            | SyntaxKind::DoKw
            | SyntaxKind::AdoKw
            | SyntaxKind::LetKw
            | SyntaxKind::Upper
            | SyntaxKind::Lower
            | SyntaxKind::AsKw
            | SyntaxKind::LeftParenthesis
            | SyntaxKind::LeftSquare
            | SyntaxKind::LeftBracket
            | SyntaxKind::LiteralChar
            | SyntaxKind::LiteralString
            | SyntaxKind::LiteralRawString
            | SyntaxKind::LiteralInteger
            | SyntaxKind::LiteralNumber
            | SyntaxKind::LiteralTrue
            | SyntaxKind::LiteralFalse
            | SyntaxKind::Question
            | SyntaxKind::Underscore
            | SyntaxKind::At
    )
}

// expr_5 expr_sp+ | expr_5
fn expr_4(parser: &mut Parser) {
    let mut marker = parser.start();
    expr_5(parser);
    let at_least_one = one_or_more(parser, |parser| {
        if at_expr_start(parser) {
            expr_sp(parser);
            true
        } else {
            false
        }
    });
    if at_least_one {
        marker.end(parser, SyntaxKind::ApplicationExpression);
    } else {
        marker.cancel(parser);
    }
}

// '@' type_atom | expr_5
fn expr_sp(parser: &mut Parser) {
    let mut marker = parser.start();
    if parser.at(SyntaxKind::At) {
        parser.consume();
        type_atom(parser);
        marker.end(parser, SyntaxKind::TypeArgument);
    } else {
        expr_5(parser);
        marker.end(parser, SyntaxKind::TermArgument);
    }
}

// expr_if | expr_case | qualified_prefix? ( expr_do | expr_ado ) | expr_atom
fn expr_5(parser: &mut Parser) {
    match parser.current() {
        SyntaxKind::IfKw => {
            return expr_if(parser);
        }
        SyntaxKind::CaseKw => {
            return expr_case(parser);
        }
        SyntaxKind::LetKw => {
            return expr_let(parser);
        }
        _ => (),
    }

    let mut expression = parser.start();
    let mut qualified = parser.start();
    let has_prefix = if parser.at(SyntaxKind::Upper) { qualified_prefix(parser) } else { false };

    match parser.current() {
        SyntaxKind::DoKw => {
            expr_do(parser, qualified, expression);
        }
        SyntaxKind::AdoKw => {
            parser.consume();
            qualified.end(parser, SyntaxKind::QualifiedAdo);
            expression.end(parser, SyntaxKind::AdoExpression);
        }
        SyntaxKind::Upper | SyntaxKind::Lower | SyntaxKind::AsKw | SyntaxKind::LeftParenthesis => {
            name_ref_or_parenthesized_expr(parser, has_prefix, qualified, expression);
        }
        _ => {
            expression.cancel(parser);
            qualified.cancel(parser);
            expr_atom(parser)
        }
    }
}

// 'if' expr_0 'then' expr_0 'else' expr_0
fn expr_if(parser: &mut Parser) {
    let mut marker = parser.start();

    parser.expect(SyntaxKind::IfKw);
    expr_0(parser);

    parser.expect(SyntaxKind::ThenKw);
    expr_0(parser);

    parser.expect(SyntaxKind::ElseKw);
    expr_0(parser);

    marker.end(parser, SyntaxKind::IfThenElseExpression);
}

fn expr_case(parser: &mut Parser) {
    todo!("case kw");
}

// 'let' '{' (expr_let_binding ';')* expr_let_binding '}' 'in' expr_0
fn expr_let(parser: &mut Parser) {
    let mut marker = parser.start();
    parser.expect(SyntaxKind::LetKw);
    layout_one_or_more(parser, expr_let_binding);
    parser.expect(SyntaxKind::InKw);
    expr_0(parser);
    marker.end(parser, SyntaxKind::LetInExpression);
}

fn expr_let_binding(parser: &mut Parser) {
    match parser.current() {
        token if token.is_lower() => {
            if parser.nth_at(1, SyntaxKind::Colon2) {
                expr_let_binding_signature(parser);
            } else {
                expr_let_binding_name(parser);
            }
        }
        SyntaxKind::Equal => {
            let mut binding = parser.start();

            let mut error = parser.start();
            parser.error("expected an identifier or pattern");
            error.end(parser, SyntaxKind::Error);

            parser.expect(SyntaxKind::Equal);
            expr_0(parser);

            binding.end(parser, SyntaxKind::Error);
        }
        _ => {
            expr_let_binding_pattern(parser);
        }
    }
}

// 'lower' '::' type_0
fn expr_let_binding_signature(parser: &mut Parser) {
    let mut marker = parser.start();

    let mut name = parser.start();
    parser.consume_as(SyntaxKind::Lower);
    name.end(parser, SyntaxKind::Name);

    parser.expect(SyntaxKind::Colon2);
    type_0(parser);

    marker.end(parser, SyntaxKind::LetBindingSignature);
}

// 'lower' pat_atom* expr_binding
fn expr_let_binding_name(parser: &mut Parser) {
    let mut marker = parser.start();

    let mut name = parser.start();
    parser.consume_as(SyntaxKind::Lower);
    name.end(parser, SyntaxKind::Name);

    zero_or_more(parser, |parser| {
        if at_pat_start(parser) {
            pat_atom(parser);
            true
        } else {
            false
        }
    });

    expr_binding(parser, SyntaxKind::Equal);

    marker.end(parser, SyntaxKind::LetBindingName);
}

// pat_1 '=' expr_where
fn expr_let_binding_pattern(parser: &mut Parser) {
    let mut marker = parser.start();
    pat_1(parser);
    parser.expect(SyntaxKind::Equal);
    expr_where(parser);
    marker.end(parser, SyntaxKind::LetBindingPattern);
}

// 'separator' expr_where | expr_guarded+
fn expr_binding(parser: &mut Parser, separator: SyntaxKind) {
    let mut marker = parser.start();
    if parser.at(separator) {
        parser.consume();
        expr_where(parser);
        marker.end(parser, SyntaxKind::UnconditionalBinding);
    } else {
        one_or_more(parser, |parser| {
            // FIXME: is there an advantage if we use positives here?
            if parser.current().is_end() || parser.at(separator) {
                false
            } else {
                expr_guarded(parser, separator);
                true
            }
        });
        marker.end(parser, SyntaxKind::GuardedBinding);
    }
}

// expr_0 ('where' '{' (expr_let_binding ';')* expr_let_binding '}')
fn expr_where(parser: &mut Parser) {
    let mut marker = parser.start();
    expr_0(parser);
    if parser.at(SyntaxKind::WhereKw) {
        parser.consume();
        layout_one_or_more(parser, expr_let_binding);
    }
    marker.end(parser, SyntaxKind::WhereExpression);
}

// '|' pat_guard (',' pat_guard)* 'separator' expr_where
fn expr_guarded(parser: &mut Parser, separator: SyntaxKind) {
    let mut marker = parser.start();
    parser.expect(SyntaxKind::Pipe);
    separated(parser, SyntaxKind::Comma, pat_guard);
    parser.expect(separator);
    expr_where(parser);
    marker.end(parser, SyntaxKind::GuardedExpression);
}

// (pat_0 '<-') expr_0
fn pat_guard(parser: &mut Parser) {
    let mut marker = parser.start();
    attempt(parser, |parser| {
        pat_0(parser);
        parser.expect(SyntaxKind::LeftArrow);
    });
    expr_0(parser);
    marker.end(parser, SyntaxKind::PatternGuard);
}

// 'do' '{' (do_statement ';')* do_statement '}'
fn expr_do(parser: &mut Parser, mut qualified: NodeMarker, mut expression: NodeMarker) {
    parser.expect(SyntaxKind::DoKw);
    qualified.end(parser, SyntaxKind::QualifiedDo);
    layout_one_or_more(parser, expr_do_statement);
    expression.end(parser, SyntaxKind::DoExpression);
}

// expr_do_let_statement | expr_do_bind_statement | expr_do_discard_statement
fn expr_do_statement(parser: &mut Parser) {
    if parser.at(SyntaxKind::LetKw) {
        expr_do_let_statement(parser);
    } else {
        if attempt(parser, expr_do_bind_statement) {
            return;
        }
        expr_do_discard_statement(parser);
    }
}

// 'let' '{' (expr_let_binding ';')* expr_let_binding '}'
fn expr_do_let_statement(parser: &mut Parser) {
    let mut marker = parser.start();
    parser.expect(SyntaxKind::LetKw);
    layout_one_or_more(parser, expr_let_binding);
    marker.end(parser, SyntaxKind::DoLetBinding);
}

// pat_0 '<-' expr_0
fn expr_do_bind_statement(parser: &mut Parser) {
    let mut marker = parser.start();
    pat_0(parser);
    parser.expect(SyntaxKind::LeftArrow);
    expr_0(parser);
    marker.end(parser, SyntaxKind::DoBind);
}

// expr_0
fn expr_do_discard_statement(parser: &mut Parser) {
    let mut marker = parser.start();
    expr_0(parser);
    marker.end(parser, SyntaxKind::DoDiscard);
}

// 'ado' '{' (ado_statement ';')* 'in' expr_0 '}'
fn expr_ado(parser: &mut Parser, mut qualified: NodeMarker, mut expression: NodeMarker) {
    parser.expect(SyntaxKind::AdoKw);
    qualified.end(parser, SyntaxKind::QualifiedAdo);

    expression.end(parser, SyntaxKind::DoExpression);
}

fn expr_atom(parser: &mut Parser) {
    let mut expression = parser.start();
    match parser.current() {
        SyntaxKind::LiteralChar
        | SyntaxKind::LiteralString
        | SyntaxKind::LiteralRawString
        | SyntaxKind::LiteralInteger
        | SyntaxKind::LiteralNumber
        | SyntaxKind::LiteralTrue
        | SyntaxKind::LiteralFalse => {
            parser.consume();
            return expression.end(parser, SyntaxKind::LiteralExpression);
        }
        SyntaxKind::LeftSquare => {
            expr_array(parser);
            return expression.end(parser, SyntaxKind::LiteralExpression);
        }
        SyntaxKind::LeftBracket => {
            expr_record(parser);
            return expression.end(parser, SyntaxKind::LiteralExpression);
        }
        _ => (),
    }

    let mut qualified = parser.start();
    let has_prefix = if parser.at(SyntaxKind::Upper) { qualified_prefix(parser) } else { false };

    match parser.current() {
        SyntaxKind::Upper | SyntaxKind::Lower | SyntaxKind::AsKw | SyntaxKind::LeftParenthesis => {
            name_ref_or_parenthesized_expr(parser, has_prefix, qualified, expression);
        }
        _ => {
            parser.error("expected an expression");
            expression.cancel(parser);
            qualified.cancel(parser);
        }
    }
}

fn expr_array(parser: &mut Parser) {
    array_container(parser, expr_0)
}

fn expr_record(parser: &mut Parser) {
    record_container(parser, SyntaxKind::NameRef, expr_0);
}

/// Shared code between [`expr_5`] and [`expr_atom`] for qualified names.
///
/// Because of qualified do/ado-notation, we end up handling qualified names
/// earlier in the [`expr_5`] rule. We want [`expr_atom`] to handle those too,
/// as it can be called standalone much like [`type_atom`].
///
/// This rule also handles parenthesized expressions, as implied by its name.
/// If `has_prefix` is true, as in if [`qualified_prefix`] succeeds, the rule
/// emits an error node rather than descending into [`expr_0`].
fn name_ref_or_parenthesized_expr(
    parser: &mut Parser,
    has_prefix: bool,
    mut qualified: NodeMarker,
    mut expression: NodeMarker,
) {
    match parser.current() {
        SyntaxKind::Upper => {
            let mut name = parser.start();
            parser.consume();
            name.end(parser, SyntaxKind::NameRef);
            qualified.end(parser, SyntaxKind::QualifiedName);
            expression.end(parser, SyntaxKind::ConstructorExpression);
        }
        token if token.is_lower() => {
            let mut name = parser.start();
            parser.consume_as(SyntaxKind::Lower);
            name.end(parser, SyntaxKind::NameRef);
            qualified.end(parser, SyntaxKind::QualifiedName);
            expression.end(parser, SyntaxKind::VariableExpression);
        }
        SyntaxKind::LeftParenthesis => {
            parser.expect(SyntaxKind::LeftParenthesis);
            if parser.current().is_operator() {
                let mut name = parser.start();
                parser.consume_as(SyntaxKind::Operator);
                name.end(parser, SyntaxKind::NameRef);

                parser.expect(SyntaxKind::RightParenthesis);
                qualified.end(parser, SyntaxKind::QualifiedName);
                expression.end(parser, SyntaxKind::OperatorNameExpression);
            } else if has_prefix {
                parser.error_recover("expected an operator");

                parser.expect(SyntaxKind::RightParenthesis);
                qualified.end(parser, SyntaxKind::QualifiedName);
                expression.end(parser, SyntaxKind::OperatorNameExpression);
            } else {
                expr_0(parser);

                parser.expect(SyntaxKind::RightParenthesis);
                qualified.cancel(parser);
                expression.end(parser, SyntaxKind::ParenthesizedExpression);
            };
        }
        _ => {
            qualified.cancel(parser);
            expression.cancel(parser);
        }
    }
}

pub(crate) fn type_0(parser: &mut Parser) {}

fn type_atom(parser: &mut Parser) {}

// pat_1 '::' type_0 | pat_1
pub(crate) fn pat_0(parser: &mut Parser) {
    let mut marker = parser.start();
    pat_1(parser);
    if parser.at(SyntaxKind::Colon2) {
        parser.consume();
        type_0(parser);
        marker.end(parser, SyntaxKind::TypedBinder);
    } else {
        marker.cancel(parser);
    }
}

// pat_2 ('operator' pat_2)+ | pat_2
fn pat_1(parser: &mut Parser) {
    let mut operator = parser.start();
    pat_2(parser);
    let at_least_one = one_or_more(parser, |parser| {
        if at_operator_start(parser) {
            let mut pair = parser.start();
            // FIXME: mark as NameRef here too...
            parser.consume_as(SyntaxKind::Operator);
            pat_2(parser);
            pair.end(parser, SyntaxKind::Pair);
            true
        } else {
            false
        }
    });
    if at_least_one {
        operator.end(parser, SyntaxKind::BinderOperatorChain);
    } else {
        operator.cancel(parser);
    }
}

// pat_negative | pat_constructor | pat_atom
fn pat_2(parser: &mut Parser) {
    match parser.current() {
        SyntaxKind::Minus => {
            pat_negative(parser);
        }
        SyntaxKind::Upper => {
            pat_constructor(parser);
        }
        _ => {
            pat_atom(parser);
        }
    }
}

// '-' ( 'integer' | 'number' )
fn pat_negative(parser: &mut Parser) {
    let mut negative = parser.start();
    parser.consume();

    let mut literal = parser.start();
    match parser.current() {
        SyntaxKind::LiteralInteger | SyntaxKind::LiteralNumber => {
            parser.consume();
            literal.end(parser, SyntaxKind::LiteralBinder);
        }
        _ => {
            parser.error("expected LiteralInteger or LiteralNumber");
            literal.cancel(parser)
        }
    }

    negative.end(parser, SyntaxKind::NegativeBinder);
}

/// Returns `true` if we're at the beginning of a pattern.
fn at_pat_start(parser: &Parser) -> bool {
    if parser.current().is_end() {
        return false;
    }
    matches!(
        parser.current(),
        SyntaxKind::LiteralChar
            | SyntaxKind::LiteralString
            | SyntaxKind::LiteralRawString
            | SyntaxKind::LiteralInteger
            | SyntaxKind::LiteralNumber
            | SyntaxKind::LiteralTrue
            | SyntaxKind::LiteralFalse
            | SyntaxKind::Underscore
            | SyntaxKind::Lower
            | SyntaxKind::AsKw
            | SyntaxKind::Upper
            | SyntaxKind::LeftParenthesis
            | SyntaxKind::LeftBracket
            | SyntaxKind::LeftSquare
    )
}

// qualified_prefix? 'upper' pat_atom*
fn pat_constructor(parser: &mut Parser) {
    let mut marker = parser.start();

    let mut qualified = parser.start();
    qualified_prefix(parser);

    if parser.at(SyntaxKind::Upper) {
        let mut name = parser.start();
        parser.consume();
        name.end(parser, SyntaxKind::NameRef);
    } else {
        parser.error_recover("expected Upper");
    }
    qualified.end(parser, SyntaxKind::QualifiedName);

    zero_or_more(parser, |parser| {
        if at_pat_start(parser) {
            pat_atom(parser);
            true
        } else {
            false
        }
    });

    marker.end(parser, SyntaxKind::ConstructorBinder);
}

fn pat_atom(parser: &mut Parser) {
    let mut marker = parser.start();
    match parser.current() {
        SyntaxKind::LiteralChar
        | SyntaxKind::LiteralString
        | SyntaxKind::LiteralRawString
        | SyntaxKind::LiteralInteger
        | SyntaxKind::LiteralNumber
        | SyntaxKind::LiteralTrue
        | SyntaxKind::LiteralFalse => {
            parser.consume();
            marker.end(parser, SyntaxKind::LiteralBinder);
        }
        SyntaxKind::Underscore => {
            parser.consume();
            marker.end(parser, SyntaxKind::WildcardBinder);
        }
        token if token.is_lower() => {
            let mut name = parser.start();
            parser.consume_as(SyntaxKind::Lower);
            name.end(parser, SyntaxKind::Name);
            marker.end(parser, SyntaxKind::VariableBinder);
        }
        SyntaxKind::Upper => {
            let mut qualified = parser.start();
            qualified_prefix(parser);
            let mut name = parser.start();
            parser.consume();
            name.end(parser, SyntaxKind::NameRef);
            qualified.end(parser, SyntaxKind::QualifiedName);
            marker.end(parser, SyntaxKind::ConstructorBinder);
        }
        SyntaxKind::LeftParenthesis => {
            parser.consume();
            pat_0(parser);
            parser.expect(SyntaxKind::RightParenthesis);
            marker.end(parser, SyntaxKind::ParenthesizedBinder);
        }
        SyntaxKind::LeftSquare => {
            pat_array(parser);
            marker.end(parser, SyntaxKind::LiteralBinder);
        }
        SyntaxKind::LeftBracket => {
            pat_record(parser);
            marker.end(parser, SyntaxKind::LiteralBinder);
        }
        _ => {
            parser.error("expected a binder");
            marker.cancel(parser);
        }
    }
}

fn pat_array(parser: &mut Parser) {
    array_container(parser, pat_0);
}

fn pat_record(parser: &mut Parser) {
    record_container(parser, SyntaxKind::Name, pat_0)
}

// ('upper' '.')+
fn qualified_prefix(parser: &mut Parser) -> bool {
    let mut marker = parser.start();
    let mut at_least_one = false;
    loop {
        // We look two tokens into the future to avoid eating qualified
        // constructors too early, for example: just Hello should not
        // have a prefix while Hello.World should leave World uneaten.
        if parser.at(SyntaxKind::Upper) && parser.nth_at(1, SyntaxKind::Period) {
            let mut marker = parser.start();
            parser.consume();
            marker.end(parser, SyntaxKind::NameRef);
            parser.consume();
            at_least_one = true;
        } else {
            break;
        }
    }
    if at_least_one {
        marker.end(parser, SyntaxKind::QualifiedPrefix);
        true
    } else {
        marker.cancel(parser);
        false
    }
}

// '[' rule (',' rule)* ']'
fn array_container(parser: &mut Parser, rule: impl Fn(&mut Parser)) {
    let mut marker = parser.start();
    parser.expect(SyntaxKind::LeftSquare);
    if parser.at(SyntaxKind::RightSquare) {
        parser.expect(SyntaxKind::RightSquare);
    } else {
        separated(parser, SyntaxKind::Comma, rule);
        parser.expect(SyntaxKind::RightSquare);
    }
    marker.end(parser, SyntaxKind::LiteralArray);
}

// '{' record_field_or_pun (',' record_field_or_pun)* '}'
fn record_container(parser: &mut Parser, pun_kind: SyntaxKind, rule: impl Fn(&mut Parser)) {
    let mut marker = parser.start();
    parser.expect(SyntaxKind::LeftBracket);
    if parser.at(SyntaxKind::RightBracket) {
        parser.expect(SyntaxKind::RightBracket);
    } else {
        separated(parser, SyntaxKind::Comma, |parser| {
            record_field_or_pun(parser, pun_kind, &rule);
        });
        parser.expect(SyntaxKind::RightBracket);
    }
    marker.end(parser, SyntaxKind::LiteralRecord);
}

// 'label' ':' rule | 'label'
fn record_field_or_pun(parser: &mut Parser, pun_kind: SyntaxKind, rule: impl Fn(&mut Parser)) {
    let mut marker = parser.start();
    if parser.current().is_label() {
        let punnable = matches!(parser.current(), SyntaxKind::Lower);

        let mut name = parser.start();
        parser.consume_as(SyntaxKind::Label);

        if punnable && !parser.at(SyntaxKind::Colon) {
            name.end(parser, pun_kind);
            marker.end(parser, SyntaxKind::RecordPun)
        } else {
            name.end(parser, SyntaxKind::Name);
            parser.expect(SyntaxKind::Colon);
            rule(parser);
            marker.end(parser, SyntaxKind::RecordField);
        }
    } else {
        parser.error_recover("expected a label");
        parser.expect(SyntaxKind::Colon);
        rule(parser);
        marker.end(parser, SyntaxKind::RecordField);
    }
}

#[cfg(test)]
mod tests {
    use lexing::{layout, lex};
    use syntax::SyntaxKind;

    use crate::parser::{Event, Parser};

    fn expect_parse<F, T>(source: &str, rule: F)
    where
        F: Fn(&mut Parser) -> T,
    {
        let lexed = lex(source);
        let input = layout(&lexed);
        let mut parser = Parser::new(&input);
        let _ = rule(&mut parser);

        println!("Input: {source}");

        let mut indentation = 0;
        for actual in parser.finalize() {
            if let Event::Start { kind: SyntaxKind::Sentinel } = actual {
                continue;
            }
            match actual {
                Event::Start { .. } => {
                    println!("{:indentation$}{:?}", "", actual, indentation = indentation);
                    indentation += 2
                }
                Event::Finish => {
                    indentation -= 2;
                    println!("{:indentation$}{:?}", "", actual, indentation = indentation);
                }
                _ => {
                    println!("{:indentation$}{:?}", "", actual, indentation = indentation);
                }
            }
        }
    }

    #[test]
    fn __() {
        expect_parse("{ a, b }", super::pat_0);
    }
}
