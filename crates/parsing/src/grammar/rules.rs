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
            operator_ref(parser);
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
            operator_ref(parser);
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
        operator_ref(parser);
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

// expr_if | expr_case | expr_let | qualified_prefix? ( expr_do | expr_ado ) | expr_6
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

    // We only backtrack parsing for `qualified_prefix` rather than the
    // entire `do`/`ado` expression, allowing us to preserve errors.
    let mut save = parser.save();
    let mut expression = parser.start();
    let mut qualified = parser.start();
    if parser.at(SyntaxKind::Upper) {
        qualified_prefix(parser);
    }
    match parser.current() {
        SyntaxKind::DoKw => {
            save.delete(parser);
            return expr_do(parser, qualified, expression);
        }
        SyntaxKind::AdoKw => {
            save.delete(parser);
            return expr_ado(parser, qualified, expression);
        }
        _ => (),
    }
    expression.cancel(parser);
    qualified.cancel(parser);
    save.load(parser);

    expr_6(parser);
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

fn expr_case(_: &mut Parser) {
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

    name(parser, SyntaxKind::Lower);
    parser.expect(SyntaxKind::Colon2);
    type_0(parser);

    marker.end(parser, SyntaxKind::LetBindingSignature);
}

// 'lower' pat_atom* expr_binding
fn expr_let_binding_name(parser: &mut Parser) {
    let mut marker = parser.start();

    name(parser, SyntaxKind::Lower);
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

fn at_record_update(parser: &Parser) -> bool {
    if !parser.at(SyntaxKind::LeftBracket) {
        return false;
    }

    if !parser.nth(1).is_label() {
        return false;
    }

    if !matches!(parser.nth(2), SyntaxKind::Equal | SyntaxKind::LeftBracket) {
        return false;
    }

    true
}

fn expr_6(parser: &mut Parser) {
    let mut marker = parser.start();
    expr_7(parser);
    if at_record_update(parser) {
        let mut wrapped = parser.start();
        parser.expect(SyntaxKind::LeftBracket);
        separated(parser, SyntaxKind::Comma, record_update_leaf_or_branch);
        parser.expect(SyntaxKind::RightBracket);
        wrapped.end(parser, SyntaxKind::Wrapped);
        marker.end(parser, SyntaxKind::RecordUpdateExpression);
    } else {
        marker.cancel(parser);
    }
}

fn record_update_leaf_or_branch(parser: &mut Parser) {
    let mut leaf_or_branch = parser.start();
    label_name(parser);
    match parser.current() {
        SyntaxKind::Equal => {
            parser.consume();
            expr_0(parser);
            leaf_or_branch.end(parser, SyntaxKind::RecordUpdateLeaf);
        }
        SyntaxKind::LeftBracket => {
            let mut wrapped = parser.start();
            parser.consume();
            separated(parser, SyntaxKind::Comma, record_update_leaf_or_branch);
            parser.expect(SyntaxKind::RightBracket);
            wrapped.end(parser, SyntaxKind::Wrapped);
            leaf_or_branch.end(parser, SyntaxKind::RecordUpdateBranch);
        }
        _ => parser.error_recover("expected '=' or '{'"),
    }
}

fn expr_7(parser: &mut Parser) {
    let mut marker = parser.start();
    expr_atom(parser);
    if parser.at(SyntaxKind::Period) {
        parser.consume();
        separated(parser, SyntaxKind::Period, label_name);
        marker.end(parser, SyntaxKind::RecordAccessExpression);
    } else {
        marker.cancel(parser);
    }
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

    let mut name_end = |parser: &mut Parser, kind: SyntaxKind| {
        qualified.end(parser, SyntaxKind::QualifiedName);
        expression.end(parser, kind);
    };

    match parser.current() {
        SyntaxKind::Upper => {
            name_ref(parser, SyntaxKind::Upper);
            name_end(parser, SyntaxKind::ConstructorExpression);
        }
        token if token.is_lower() => {
            name_ref(parser, SyntaxKind::Lower);
            name_end(parser, SyntaxKind::VariableExpression);
        }
        SyntaxKind::LeftParenthesis => {
            let mut wrapped = parser.start();
            parser.expect(SyntaxKind::LeftParenthesis);

            if parser.current().is_operator() {
                name_ref(parser, SyntaxKind::Operator);
                parser.expect(SyntaxKind::RightParenthesis);
                wrapped.end(parser, SyntaxKind::Wrapped);
                name_end(parser, SyntaxKind::OperatorNameExpression);
            } else if has_prefix {
                parser.error_recover("expected an operator");
                parser.expect(SyntaxKind::RightParenthesis);
                wrapped.end(parser, SyntaxKind::Wrapped);
                name_end(parser, SyntaxKind::OperatorNameExpression);
            } else {
                expr_0(parser);
                parser.expect(SyntaxKind::RightParenthesis);
                wrapped.cancel(parser);
                qualified.cancel(parser);
                expression.end(parser, SyntaxKind::ParenthesizedExpression);
            };
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

// type_1 '::' type_1 | type_1
pub(crate) fn type_0(parser: &mut Parser) {
    let mut marker = parser.start();
    type_1(parser);
    if parser.at(SyntaxKind::Colon2) {
        parser.consume();
        type_1(parser);
        marker.end(parser, SyntaxKind::KindedType);
    } else {
        marker.cancel(parser);
    }
}

// type_forall | type_2
fn type_1(parser: &mut Parser) {
    if parser.at(SyntaxKind::ForallKw) {
        type_forall(parser);
    } else {
        type_2(parser);
    }
}

fn type_forall(parser: &mut Parser) {
    let mut marker = parser.start();
    parser.expect(SyntaxKind::ForallKw);
    one_or_more(parser, |parser| {
        if parser.current().is_end() || parser.at(SyntaxKind::Period) {
            false
        } else {
            type_variable_binding_with_visibility(parser);
            true
        }
    });
    parser.expect(SyntaxKind::Period);
    type_1(parser);
    marker.end(parser, SyntaxKind::ForallType);
}

fn type_variable_binding_with_visibility(parser: &mut Parser) {
    type_variable_binding(parser, |parser| {
        let mut prefixed = parser.start();
        if parser.at(SyntaxKind::At) {
            parser.consume();
        }

        if parser.current().is_lower() {
            name(parser, SyntaxKind::Lower);
        } else {
            parser.error_recover("expected type variable");
        }

        prefixed.end(parser, SyntaxKind::Prefixed);
    });
}

fn type_variable_binding(parser: &mut Parser, binding_name: impl Fn(&mut Parser)) {
    let mut marker = parser.start();
    if parser.at(SyntaxKind::LeftParenthesis) {
        let mut wrapped = parser.start();
        parser.consume();
        let mut kinded = parser.start();
        binding_name(parser);
        parser.expect(SyntaxKind::Colon2);
        type_1(parser);
        kinded.end(parser, SyntaxKind::Labeled);
        parser.expect(SyntaxKind::RightParenthesis);
        wrapped.end(parser, SyntaxKind::Wrapped);
    } else {
        binding_name(parser);
    }
    marker.end(parser, SyntaxKind::TypeVariableBinding);
}

// type_3 '->' type_1 | type_3 '=>' type_1 | type_3
fn type_2(parser: &mut Parser) {
    let mut marker = parser.start();
    type_3(parser);
    match parser.current() {
        SyntaxKind::RightArrow => {
            parser.consume();
            type_1(parser);
            marker.end(parser, SyntaxKind::ArrowType);
        }
        SyntaxKind::RightThickArrow => {
            parser.consume();
            type_1(parser);
            marker.end(parser, SyntaxKind::ConstrainedType);
        }
        _ => {
            marker.cancel(parser);
        }
    }
}

// type_4 ('operator' type_4)+ | type_4
fn type_3(parser: &mut Parser) {
    let mut marker = parser.start();
    type_4(parser);
    let at_least_one = one_or_more(parser, |parser| {
        if at_operator_start(parser) {
            let mut marker = parser.start();
            operator_ref(parser);
            type_4(parser);
            marker.end(parser, SyntaxKind::Pair);
            true
        } else {
            false
        }
    });
    if at_least_one {
        marker.end(parser, SyntaxKind::TypeOperatorChain);
    } else {
        marker.cancel(parser);
    }
}

fn type_4(parser: &mut Parser) {
    let mut marker = parser.start();
    if parser.at(SyntaxKind::Minus) {
        parser.consume();
        parser.expect(SyntaxKind::LiteralInteger);
        marker.end(parser, SyntaxKind::IntegerType);
    } else {
        type_5(parser);
        marker.cancel(parser);
    }
}

fn at_type_start(parser: &mut Parser) -> bool {
    if parser.current().is_end() {
        return false;
    }
    matches!(
        parser.current(),
        SyntaxKind::Lower
            | SyntaxKind::Upper
            | SyntaxKind::LiteralInteger
            | SyntaxKind::Minus
            | SyntaxKind::LeftParenthesis
    )
}

fn type_5(parser: &mut Parser) {
    let mut marker = parser.start();
    type_atom(parser);
    let at_least_one = one_or_more(parser, |parser| {
        if at_type_start(parser) {
            type_atom(parser);
            true
        } else {
            false
        }
    });
    if at_least_one {
        marker.end(parser, SyntaxKind::ApplicationType);
    } else {
        marker.cancel(parser);
    }
}

fn type_atom(parser: &mut Parser) {
    let mut ty = parser.start();
    match parser.current() {
        token if token.is_lower() => {
            name_ref(parser, SyntaxKind::Lower);
            ty.end(parser, SyntaxKind::VariableType);
            return;
        }
        SyntaxKind::LiteralString => {
            parser.consume();
            ty.end(parser, SyntaxKind::StringType);
            return;
        }
        SyntaxKind::LiteralInteger => {
            parser.consume();
            ty.end(parser, SyntaxKind::IntegerType);
            return;
        }
        SyntaxKind::Underscore => {
            parser.consume();
            ty.end(parser, SyntaxKind::WildcardType);
            return;
        }
        SyntaxKind::LeftBracket => {
            type_record(parser, ty);
            return;
        }
        _ => {}
    }

    let mut qualified = parser.start();
    let has_prefix = if parser.at(SyntaxKind::Upper) { qualified_prefix(parser) } else { false };

    match parser.current() {
        SyntaxKind::Upper | SyntaxKind::LeftParenthesis => {
            qualified_name_or_row_or_parenthesized_type(parser, has_prefix, qualified, ty);
        }
        _ => {
            parser.error("expected a type");
            ty.cancel(parser);
            qualified.cancel(parser);
        }
    }
}

fn type_record(parser: &mut Parser, mut ty: NodeMarker) {
    let mut wrapped = parser.start();
    parser.expect(SyntaxKind::LeftBracket);

    let mut inner = parser.start();

    match parser.current() {
        // '{' '}'
        SyntaxKind::RightBracket => (),
        // '{' '|' type_0 '}'
        SyntaxKind::Pipe => {
            row_tail(parser);
        }
        // '{' ('label' '::' type_0)+? ('|' type_0?) '}'
        _ => {
            separated(parser, SyntaxKind::Comma, row_field);
            row_tail(parser);
        }
    }

    inner.end(parser, SyntaxKind::RowInner);
    parser.expect(SyntaxKind::RightBracket);
    wrapped.end(parser, SyntaxKind::Wrapped);
    ty.end(parser, SyntaxKind::RecordType);
}

fn qualified_name_or_row_or_parenthesized_type(
    parser: &mut Parser,
    has_prefix: bool,
    mut qualified: NodeMarker,
    mut ty: NodeMarker,
) {
    match parser.current() {
        SyntaxKind::Upper => {
            name_ref(parser, SyntaxKind::Upper);
            qualified.end(parser, SyntaxKind::QualifiedName);
            ty.end(parser, SyntaxKind::ConstructorType);
        }
        SyntaxKind::LeftParenthesis => {
            let mut wrapped = parser.start();
            parser.expect(SyntaxKind::LeftParenthesis);

            let mut operator_name_end = |parser: &mut Parser| {
                parser.expect(SyntaxKind::RightParenthesis);
                wrapped.end(parser, SyntaxKind::Wrapped);
                qualified.end(parser, SyntaxKind::QualifiedName);
                ty.end(parser, SyntaxKind::OperatorNameType);
            };

            if parser.current().is_operator() {
                name_ref(parser, SyntaxKind::Operator);
                operator_name_end(parser);
            } else if has_prefix {
                if parser.current().is_reserved_operator() {
                    parser.error_recover("unexpected reserved operator");
                } else {
                    parser.error_recover("expected an operator");
                }
                operator_name_end(parser);
            } else {
                qualified.cancel(parser);
                open_row_or_parenthesized_type(parser, wrapped, ty);
            }
        }
        _ => {
            qualified.cancel(parser);
            ty.cancel(parser);
        }
    }
}

fn open_row_or_parenthesized_type(
    parser: &mut Parser,
    mut wrapped: NodeMarker,
    mut ty: NodeMarker,
) {
    let mut inner = parser.start();
    let mut row_type_end = |parser: &mut Parser| {
        inner.end(parser, SyntaxKind::RowInner);
        parser.expect(SyntaxKind::RightParenthesis);
        wrapped.end(parser, SyntaxKind::Wrapped);
        ty.end(parser, SyntaxKind::RowType);
    };
    match parser.current() {
        // '(' ')'
        SyntaxKind::RightParenthesis => {
            row_type_end(parser);
        }
        // '(' '|' type_0 ')'
        SyntaxKind::Pipe => {
            row_tail(parser);
            row_type_end(parser);
        }
        // '(' ('label' '::' type_0)+? ('|' type_0?) ')'
        token if token.is_label() && parser.nth_at(1, SyntaxKind::Colon2) => {
            separated(parser, SyntaxKind::Comma, row_field);
            row_tail(parser);
            row_type_end(parser);
        }
        _ => {
            type_0(parser);
            parser.expect(SyntaxKind::RightParenthesis);
            inner.cancel(parser);
            wrapped.cancel(parser);
            ty.end(parser, SyntaxKind::ParenthesizedType);
        }
    }
}

fn row_field(parser: &mut Parser) {
    let mut field = parser.start();
    label_name(parser);
    parser.expect(SyntaxKind::Colon2);
    type_0(parser);
    field.end(parser, SyntaxKind::RowField);
}

fn row_tail(parser: &mut Parser) {
    let mut tail = parser.start();
    if parser.at(SyntaxKind::Pipe) {
        parser.expect(SyntaxKind::Pipe);
        type_0(parser);
        tail.end(parser, SyntaxKind::RowTail);
    } else {
        tail.cancel(parser);
    }
}

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
            operator_ref(parser);
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
        name_ref(parser, SyntaxKind::Upper);
    } else {
        parser.error_recover("expected Upper");
    }
    qualified.end(parser, SyntaxKind::QualifiedName);

    one_or_more(parser, |parser| {
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
            name(parser, SyntaxKind::Lower);
            marker.end(parser, SyntaxKind::VariableBinder);
        }
        SyntaxKind::Upper => {
            let mut qualified = parser.start();
            qualified_prefix(parser);
            name_ref(parser, SyntaxKind::Upper);
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
            name_ref(parser, SyntaxKind::Upper);
            parser.expect(SyntaxKind::Period);
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
    let mut wrapped = parser.start();
    parser.expect(SyntaxKind::LeftSquare);
    if parser.at(SyntaxKind::RightSquare) {
        parser.expect(SyntaxKind::RightSquare);
    } else {
        separated(parser, SyntaxKind::Comma, rule);
        parser.expect(SyntaxKind::RightSquare);
    }
    wrapped.end(parser, SyntaxKind::Wrapped);
    marker.end(parser, SyntaxKind::LiteralArray);
}

// '{' record_field_or_pun (',' record_field_or_pun)* '}'
fn record_container(parser: &mut Parser, pun_kind: SyntaxKind, rule: impl Fn(&mut Parser)) {
    let mut marker = parser.start();
    let mut wrapped = parser.start();
    parser.expect(SyntaxKind::LeftBracket);
    if parser.at(SyntaxKind::RightBracket) {
        parser.expect(SyntaxKind::RightBracket);
    } else {
        separated(parser, SyntaxKind::Comma, |parser| {
            record_field_or_pun(parser, pun_kind, &rule);
        });
        parser.expect(SyntaxKind::RightBracket);
    }
    wrapped.end(parser, SyntaxKind::Wrapped);
    marker.end(parser, SyntaxKind::LiteralRecord);
}

// 'label' ':' rule | 'label'
fn record_field_or_pun(parser: &mut Parser, pun_kind: SyntaxKind, rule: impl Fn(&mut Parser)) {
    let mut marker = parser.start();
    if parser.current().is_label() {
        let punnable = matches!(parser.current(), SyntaxKind::Lower);

        // FIXME: use name/name_ref instead?
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

fn name(parser: &mut Parser, kind: SyntaxKind) {
    let mut marker = parser.start();
    parser.consume_as(kind);
    marker.end(parser, SyntaxKind::Name);
}

fn name_ref(parser: &mut Parser, kind: SyntaxKind) {
    let mut marker = parser.start();
    parser.consume_as(kind);
    marker.end(parser, SyntaxKind::NameRef);
}

fn label_name(parser: &mut Parser<'_>) {
    if parser.current().is_label() {
        name(parser, SyntaxKind::Label);
    } else {
        parser.error_recover("expected a label");
    }
}

fn operator_ref(parser: &mut Parser) {
    assert!(parser.current().is_operator());
    let mut marker = parser.start();
    name_ref(parser, SyntaxKind::Operator);
    marker.end(parser, SyntaxKind::QualifiedName);
}

fn operator_name_ref(parser: &mut Parser) {
    let mut wrapped = parser.start();
    parser.expect(SyntaxKind::LeftParenthesis);
    if parser.current().is_operator() {
        name_ref(parser, SyntaxKind::Operator);
    } else if parser.current().is_reserved_operator() {
        parser.error_recover("unexpected reserved operator");
    } else {
        parser.error_recover("expected an operator");
    }
    parser.expect(SyntaxKind::RightParenthesis);
    wrapped.end(parser, SyntaxKind::Wrapped);
}

pub(crate) fn module(parser: &mut Parser) {
    let mut marker = parser.start();
    module_header(parser);
    parser.expect(SyntaxKind::LayoutStart);
    module_imports(parser);
    module_body(parser);
    parser.expect(SyntaxKind::LayoutEnd);
    marker.end(parser, SyntaxKind::Module);
}

fn module_header(parser: &mut Parser) {
    let mut marker = parser.start();
    parser.expect(SyntaxKind::ModuleKw);
    module_name(parser);
    if parser.at(SyntaxKind::LeftParenthesis) {
        export_list(parser);
    }
    parser.expect(SyntaxKind::WhereKw);
    marker.end(parser, SyntaxKind::ModuleHeader);
}

fn module_name(parser: &mut Parser) {
    let mut marker = parser.start();
    if parser.at(SyntaxKind::Upper) {
        name_ref(parser, SyntaxKind::Upper)
    } else {
        parser.error_recover("expected a module name");
    }
    loop {
        if parser.at(SyntaxKind::Period) {
            parser.consume();
        } else {
            break;
        }
        if parser.at(SyntaxKind::Upper) {
            name_ref(parser, SyntaxKind::Upper);
        } else {
            parser.error_recover("expected a module segment");
        }
    }
    marker.end(parser, SyntaxKind::ModuleName);
}

fn export_list(parser: &mut Parser) {
    let mut marker = parser.start();
    let mut wrapped = parser.start();
    parser.expect(SyntaxKind::LeftParenthesis);
    separated(parser, SyntaxKind::Comma, export_item);
    parser.expect(SyntaxKind::RightParenthesis);
    wrapped.end(parser, SyntaxKind::Wrapped);
    marker.end(parser, SyntaxKind::ExportList);
}

fn export_item(parser: &mut Parser) {
    let mut marker = parser.start();
    match parser.current() {
        SyntaxKind::TypeKw => {
            parser.consume();
            operator_name_ref(parser);
            marker.end(parser, SyntaxKind::ExportTypeOp);
        }
        SyntaxKind::ClassKw => {
            parser.consume();
            name_ref(parser, SyntaxKind::Upper);
            marker.end(parser, SyntaxKind::ExportClass);
        }
        SyntaxKind::ModuleKw => {
            parser.consume();
            module_name(parser);
            marker.end(parser, SyntaxKind::ExportModule);
        }
        SyntaxKind::LeftParenthesis => {
            operator_name_ref(parser);
            marker.end(parser, SyntaxKind::ExportOp);
        }
        token if token.is_lower() => {
            name_ref(parser, SyntaxKind::Lower);
            marker.end(parser, SyntaxKind::ExportValue);
        }
        SyntaxKind::Upper => {
            name_ref(parser, SyntaxKind::Upper);
            marker.end(parser, SyntaxKind::ExportType);
        }
        _ => {
            parser.error("expected an export item");
            marker.cancel(parser);
        }
    }
}

fn module_imports(parser: &mut Parser) {
    let mut marker = parser.start();
    zero_or_more(parser, |parser| {
        if !parser.at(SyntaxKind::ImportKw) {
            return false;
        }
        import_declaration(parser);
        match parser.current() {
            SyntaxKind::LayoutSep => {
                parser.consume();
                true
            }
            SyntaxKind::LayoutEnd => false,
            _ => {
                parser.error("expected LayoutEnd or LayoutSep");
                false
            }
        }
    });
    marker.end(parser, SyntaxKind::ModuleImports);
}

fn import_declaration(parser: &mut Parser) {
    let mut marker = parser.start();
    parser.expect(SyntaxKind::ImportKw);
    module_name(parser);

    match parser.current() {
        SyntaxKind::LeftParenthesis | SyntaxKind::HidingKw => {
            import_list(parser);
        }
        _ => (),
    }

    if parser.at(SyntaxKind::AsKw) {
        let mut qualified = parser.start();
        parser.consume();
        module_name(parser);
        qualified.end(parser, SyntaxKind::ImportQualified);
    }

    marker.end(parser, SyntaxKind::ImportDeclaration);
}

fn import_list(parser: &mut Parser) {
    let mut marker = parser.start();
    if parser.at(SyntaxKind::HidingKw) {
        parser.consume();
    }
    let mut wrapped = parser.start();
    parser.expect(SyntaxKind::LeftParenthesis);
    separated(parser, SyntaxKind::Comma, import_item);
    parser.expect(SyntaxKind::RightParenthesis);
    wrapped.end(parser, SyntaxKind::Wrapped);
    marker.end(parser, SyntaxKind::ImportList);
}

fn import_item(parser: &mut Parser) {
    let mut marker = parser.start();
    match parser.current() {
        SyntaxKind::LeftParenthesis => {
            operator_name_ref(parser);
            marker.end(parser, SyntaxKind::ImportOp);
        }
        SyntaxKind::Upper => {
            name_ref(parser, SyntaxKind::Upper);
            if parser.at(SyntaxKind::LeftParenthesis) {
                let mut data = parser.start();
                let mut wrapped = parser.start();
                parser.consume();
                let kind = match parser.current() {
                    SyntaxKind::RightParenthesis => {
                        parser.consume();
                        SyntaxKind::DataEnumerated
                    }
                    SyntaxKind::Period2 => {
                        parser.consume();
                        parser.expect(SyntaxKind::RightParenthesis);
                        SyntaxKind::DataAll
                    }
                    _ => {
                        separated(parser, SyntaxKind::Comma, |parser| {
                            if parser.at(SyntaxKind::Upper) {
                                name_ref(parser, SyntaxKind::Upper);
                            } else {
                                parser.error("expected a constructor");
                            }
                        });
                        parser.expect(SyntaxKind::RightParenthesis);
                        SyntaxKind::DataEnumerated
                    }
                };
                data.end(parser, kind);
                wrapped.end(parser, SyntaxKind::Wrapped)
            }
            marker.end(parser, SyntaxKind::ImportType);
        }
        SyntaxKind::TypeKw => {
            parser.consume();
            operator_name_ref(parser);
            marker.end(parser, SyntaxKind::ImportTypeOp);
        }
        SyntaxKind::ClassKw => {
            parser.consume();
            name_ref(parser, SyntaxKind::Upper);
            marker.end(parser, SyntaxKind::ImportClass);
        }
        token if token.is_lower() => {
            name_ref(parser, SyntaxKind::Lower);
            marker.end(parser, SyntaxKind::ImportValue);
        }
        _ => {
            marker.cancel(parser);
            parser.error("expected an import item");
        }
    }
}

fn module_body(_: &mut Parser) {}
