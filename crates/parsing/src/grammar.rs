mod combinators;
mod rules;

use either::Either::{self, Left, Right};
use syntax::SyntaxKind;

use crate::parser::Parser;

use self::combinators::{attempt, layout_one_or_more, one_or_more, separated, zero_or_more};

pub fn expression(parser: &mut Parser) {
    let mut typed = parser.start();
    expression_1(parser);

    if parser.at(SyntaxKind::Colon2) {
        parser.consume();
        ty(parser);
        typed.end(parser, SyntaxKind::TypedExpression);
    } else {
        typed.cancel(parser);
    }
}

fn expression_1(parser: &mut Parser) {
    let mut operator = parser.start();
    expression_2(parser);

    let has_chain = one_or_more(parser, |parser| {
        if matches!(
            parser.current(),
            SyntaxKind::LayoutSep | SyntaxKind::LayoutEnd | SyntaxKind::EndOfFile
        ) || !parser.current().is_operator()
        {
            return false;
        }

        let mut pair = parser.start();
        parser.consume_as(SyntaxKind::Operator);
        expression_2(parser);
        pair.end(parser, SyntaxKind::Pair);

        true
    });

    if has_chain {
        operator.end(parser, SyntaxKind::ExpressionOperatorChain);
    } else {
        operator.cancel(parser);
    }
}

fn expression_2(parser: &mut Parser) {
    let mut infix = parser.start();
    expression_3(parser);

    let has_chain = one_or_more(parser, |parser| {
        if matches!(
            parser.current(),
            SyntaxKind::LayoutSep | SyntaxKind::LayoutEnd | SyntaxKind::EndOfFile
        ) || !parser.at(SyntaxKind::Tick)
        {
            return false;
        }

        let mut pair = parser.start();
        tick_expression(parser);
        expression_3(parser);
        pair.end(parser, SyntaxKind::Pair);

        true
    });

    if has_chain {
        infix.end(parser, SyntaxKind::ExpressionInfixChain);
    } else {
        infix.cancel(parser);
    }
}

fn tick_expression(parser: &mut Parser) {
    let mut wrapped = parser.start();
    parser.expect(SyntaxKind::Tick);
    tick_expression_1(parser);
    parser.expect(SyntaxKind::Tick);
    wrapped.end(parser, SyntaxKind::Wrapped);
}

fn tick_expression_1(parser: &mut Parser) {
    let mut operator = parser.start();
    expression_3(parser);

    let has_chain = one_or_more(parser, |parser| {
        if matches!(
            parser.current(),
            SyntaxKind::LayoutSep | SyntaxKind::LayoutEnd | SyntaxKind::EndOfFile
        ) || !parser.at(SyntaxKind::Operator)
        {
            return false;
        }

        let mut pair = parser.start();
        parser.consume();
        expression_3(parser);
        pair.end(parser, SyntaxKind::Pair);

        true
    });

    if has_chain {
        operator.end(parser, SyntaxKind::ExpressionOperatorChain);
    } else {
        operator.cancel(parser);
    }
}

fn expression_3(parser: &mut Parser) {
    let mut negate = parser.start();
    if parser.at(SyntaxKind::Minus) {
        parser.consume_as(SyntaxKind::Operator);
        expression_3(parser);
        negate.end(parser, SyntaxKind::NegateExpression);
    } else {
        expression_4(parser);
        negate.cancel(parser);
    }
}

fn expression_4(parser: &mut Parser) {
    let mut application = parser.start();
    expression_5(parser);

    let has_arguments = one_or_more(parser, |parser| {
        if matches!(
            parser.current(),
            SyntaxKind::LayoutSep | SyntaxKind::LayoutEnd | SyntaxKind::EndOfFile
        ) || at_expression_boundary(parser)
        {
            return false;
        }

        expr_spine(parser);

        true
    });

    if has_arguments {
        application.end(parser, SyntaxKind::ApplicationExpression);
    } else {
        application.cancel(parser);
    }
}

fn at_expression_boundary(parser: &Parser) -> bool {
    parser.current().is_operator()
        || matches!(
            parser.current(),
            SyntaxKind::Colon2
                | SyntaxKind::Comma
                | SyntaxKind::ElseKw
                | SyntaxKind::Equal
                | SyntaxKind::LeftArrow
                | SyntaxKind::Pipe
                | SyntaxKind::RightBracket
                | SyntaxKind::RightParenthesis
                | SyntaxKind::RightSquare
                | SyntaxKind::ThenKw
                | SyntaxKind::Tick
                | SyntaxKind::WhereKw
        )
}

fn expr_spine(parser: &mut Parser) {
    let mut marker = parser.start();
    if parser.at(SyntaxKind::At) {
        parser.consume();
        type_atom(parser);
        marker.end(parser, SyntaxKind::TypeArgument);
    } else {
        expression_5(parser);
        marker.end(parser, SyntaxKind::TermArgument);
    }
}

fn expression_5(parser: &mut Parser) {
    match parser.current() {
        SyntaxKind::IfKw => {
            expression_if(parser);
        }
        SyntaxKind::Upper => {
            let mut expression = parser.start();
            let mut qualified_do_or_ado = parser.start();

            match qualified_name_or_do_ado(parser) {
                Some(Left(kind)) => {
                    expression.end(parser, kind);
                    qualified_do_or_ado.cancel(parser);
                }
                Some(Right(kind)) => {
                    qualified_do_or_ado.end(parser, kind);

                    layout_one_or_more(parser, do_statement);
                    expression.end(parser, SyntaxKind::DoExpression);
                }
                None => {
                    expression.cancel(parser);
                    qualified_do_or_ado.cancel(parser);
                }
            }
        }
        SyntaxKind::DoKw => {
            let mut do_expression = parser.start();

            let mut qualified_do = parser.start();
            parser.consume();
            qualified_do.end(parser, SyntaxKind::QualifiedDo);

            layout_one_or_more(parser, do_statement);
            do_expression.end(parser, SyntaxKind::DoExpression);
        }
        SyntaxKind::AdoKw => {
            todo!("PARSE ADO!");
        }
        _ => {
            expression_atom(parser);
        }
    }
}

fn expression_if(parser: &mut Parser) {
    let mut marker = parser.start();

    parser.expect(SyntaxKind::IfKw);
    expression(parser);

    parser.expect(SyntaxKind::ThenKw);
    expression(parser);

    parser.expect(SyntaxKind::ElseKw);

    if !matches!(
        parser.current(),
        SyntaxKind::LayoutSep | SyntaxKind::LayoutEnd | SyntaxKind::EndOfFile
    ) {
        expression(parser);
    } else {
        parser.error("expected Expression");
    }

    marker.end(parser, SyntaxKind::IfThenElseExpression);
}

fn do_statement(parser: &mut Parser) {
    if parser.at(SyntaxKind::LetKw) {
        do_let_binding(parser);
    } else {
        if attempt(parser, do_bind) {
            return;
        }

        do_discard(parser);
    };
}

fn do_let_binding(parser: &mut Parser) {
    let mut marker = parser.start();
    parser.expect(SyntaxKind::LetKw);
    layout_one_or_more(parser, let_binding);
    marker.end(parser, SyntaxKind::DoLetBinding);
}

fn do_bind(parser: &mut Parser) {
    let mut marker = parser.start();
    binder(parser);
    parser.expect(SyntaxKind::LeftArrow);
    expression(parser);
    marker.end(parser, SyntaxKind::DoBind);
}

fn do_discard(parser: &mut Parser) {
    let mut marker = parser.start();
    expression(parser);
    marker.end(parser, SyntaxKind::DoDiscard);
}

fn let_binding(parser: &mut Parser) {
    match parser.current() {
        SyntaxKind::Lower if parser.nth_at(1, SyntaxKind::Colon2) => {
            let_binding_signature(parser);
        }
        SyntaxKind::Lower => {
            let_binding_name(parser);
        }
        SyntaxKind::Equal => {
            let mut binding = parser.start();
            let mut error = parser.start();
            parser.error("expected Identifier or Binder");
            error.end(parser, SyntaxKind::Error);

            parser.expect(SyntaxKind::Equal);
            expression(parser);
            binding.end(parser, SyntaxKind::Error);
        }
        _ => {
            let_binding_pattern(parser);
        }
    }
}

fn let_binding_signature(parser: &mut Parser) {
    let mut marker = parser.start();
    lower_name(parser);
    parser.expect(SyntaxKind::Colon2);
    ty(parser);
    marker.end(parser, SyntaxKind::LetBindingSignature);
}

fn let_binding_name(parser: &mut Parser) {
    let mut binding = parser.start();

    lower_name(parser);

    zero_or_more(parser, |parser| {
        if matches!(parser.current(), SyntaxKind::Equal | SyntaxKind::Pipe) {
            return false;
        }

        binder_atom(parser);

        true
    });

    guarded_binding(parser, SyntaxKind::Equal);

    binding.end(parser, SyntaxKind::LetBindingName);
}

fn let_binding_pattern(parser: &mut Parser) {
    let mut binding = parser.start();
    binder_1(parser);
    parser.expect(SyntaxKind::Equal);
    where_expression(parser);
    binding.end(parser, SyntaxKind::LetBindingPattern);
}

fn pattern_guard(parser: &mut Parser) {
    let mut marker = parser.start();

    attempt(parser, |parser| {
        binder(parser);
        parser.expect(SyntaxKind::LeftArrow);
    });

    expression(parser);

    marker.end(parser, SyntaxKind::PatternGuard);
}

fn guarded_binding(parser: &mut Parser, separator: SyntaxKind) {
    let mut marker = parser.start();

    if parser.at(separator) {
        parser.consume();
        where_expression(parser);
        marker.end(parser, SyntaxKind::UnconditionalBinding);
    } else if parser.at(SyntaxKind::Pipe) {
        one_or_more(parser, |parser| {
            if matches!(
                parser.current(),
                SyntaxKind::LayoutEnd | SyntaxKind::LayoutSep | SyntaxKind::EndOfFile
            ) || parser.at(separator)
            {
                return false;
            }
            let mut marker = parser.start();
            parser.expect(SyntaxKind::Pipe);
            separated(parser, SyntaxKind::Comma, pattern_guard);
            parser.expect(separator);
            where_expression(parser);
            marker.end(parser, SyntaxKind::GuardedExpression);
            true
        });
        marker.end(parser, SyntaxKind::GuardedBinding);
    }
}

fn where_expression(parser: &mut Parser) {
    let mut marker = parser.start();
    expression(parser);

    if parser.at(SyntaxKind::WhereKw) {
        parser.consume();
        layout_one_or_more(parser, let_binding);
    }

    marker.end(parser, SyntaxKind::WhereExpression);
}

fn expression_atom(parser: &mut Parser) {
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
            marker.end(parser, SyntaxKind::LiteralExpression);
        }
        SyntaxKind::LeftParenthesis => {
            if attempt(parser, qualified_name_or_do_ado) {
                marker.end(parser, SyntaxKind::OperatorNameExpression);
                return;
            }

            parser.expect(SyntaxKind::LeftParenthesis);
            expression(parser);
            parser.expect(SyntaxKind::RightParenthesis);
            marker.end(parser, SyntaxKind::ParenthesizedExpression);
        }
        SyntaxKind::LeftSquare => {
            literal_array(parser);
            marker.end(parser, SyntaxKind::LiteralExpression);
        }
        SyntaxKind::LeftBracket => {
            literal_record(parser);
            marker.end(parser, SyntaxKind::LiteralExpression);
        }
        SyntaxKind::Upper | SyntaxKind::Lower | SyntaxKind::AsKw => {
            match qualified_name_or_do_ado(parser) {
                Some(Left(kind)) => {
                    marker.end(parser, kind);
                }
                Some(Right(_)) => {
                    unreachable!("should have been handled by `expression_5`");
                }
                None => {
                    marker.cancel(parser);
                }
            }
        }
        _ => {
            parser.error("expected Expression");
            marker.cancel(parser);
        }
    }
}

fn literal_array(parser: &mut Parser) {
    let mut array = parser.start();

    parser.expect(SyntaxKind::LeftSquare);
    if parser.at(SyntaxKind::RightSquare) {
        parser.expect(SyntaxKind::RightSquare);
    } else {
        separated(parser, SyntaxKind::Comma, expression);
        parser.expect(SyntaxKind::RightSquare);
    }

    array.end(parser, SyntaxKind::LiteralArray);
}

fn record_item(parser: &mut Parser) {
    let mut marker = parser.start();

    let optional_field = match parser.current() {
        SyntaxKind::Lower => {
            label_name(parser);
            true
        }
        SyntaxKind::LiteralString | SyntaxKind::LiteralRawString => {
            label_name(parser);
            false
        }
        _ => {
            parser.error_recover("expected Lower or LiteralString");
            false
        }
    };

    if optional_field && !parser.at(SyntaxKind::Colon) {
        marker.end(parser, SyntaxKind::RecordPun);
        return;
    }

    parser.expect(SyntaxKind::Colon);
    expression(parser);
    marker.end(parser, SyntaxKind::RecordField);
}

fn literal_record(parser: &mut Parser) {
    let mut record = parser.start();

    parser.expect(SyntaxKind::LeftBracket);
    if parser.at(SyntaxKind::RightBracket) {
        parser.expect(SyntaxKind::RightBracket);
    } else {
        separated(parser, SyntaxKind::Comma, record_item);
        parser.expect(SyntaxKind::RightBracket);
    }

    record.end(parser, SyntaxKind::LiteralRecord);
}

pub fn ty(parser: &mut Parser) {
    let mut kinded = parser.start();
    type_1(parser);

    if parser.at(SyntaxKind::Colon2) {
        parser.consume();
        type_1(parser);
        kinded.end(parser, SyntaxKind::KindedType);
    } else {
        kinded.cancel(parser);
    }
}

fn type_1(parser: &mut Parser) {
    if parser.at(SyntaxKind::ForallKw) {
        type_forall(parser);
    } else {
        type_atom(parser);
    }
}

fn type_atom(parser: &mut Parser) -> bool {
    let mut marker = parser.start();
    match parser.current() {
        SyntaxKind::Lower => {
            parser.consume();
            marker.end(parser, SyntaxKind::VariableType);
            return true;
        }
        SyntaxKind::Upper => {
            parser.consume();
            marker.end(parser, SyntaxKind::ConstructorType);
            return true;
        }
        _ => (),
    }
    marker.cancel(parser);
    false
}

fn type_forall(parser: &mut Parser) {
    let mut forall = parser.start();
    parser.expect(SyntaxKind::ForallKw);

    one_or_more(parser, |parser| {
        if matches!(
            parser.current(),
            SyntaxKind::LayoutSep | SyntaxKind::LayoutEnd | SyntaxKind::EndOfFile
        ) || parser.at(SyntaxKind::Period)
        {
            return false;
        }

        type_variable_binding_with_visibility(parser);

        true
    });

    parser.expect(SyntaxKind::Period);

    type_1(parser);

    forall.end(parser, SyntaxKind::ForallType);
}

#[allow(unused)]
fn type_variable_binding_plain(parser: &mut Parser) {
    type_variable_binding(parser, |parser| {
        let mut name = parser.start();
        parser.expect(SyntaxKind::Lower);
        name.end(parser, SyntaxKind::Name);
    });
}

fn type_variable_binding_with_visibility(parser: &mut Parser) {
    type_variable_binding(parser, |parser| {
        let mut prefixed = parser.start();
        if parser.at(SyntaxKind::At) {
            parser.consume();
        }

        let mut name = parser.start();
        parser.expect(SyntaxKind::Lower);
        name.end(parser, SyntaxKind::Name);

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

pub fn binder(parser: &mut Parser) {
    let mut typed = parser.start();
    binder_1(parser);

    if parser.at(SyntaxKind::Colon2) {
        parser.consume();
        ty(parser);
        typed.end(parser, SyntaxKind::TypedBinder);
    } else {
        typed.cancel(parser);
    }
}

fn binder_1(parser: &mut Parser) {
    let mut operator = parser.start();
    binder_2(parser);

    let has_chain = one_or_more(parser, |parser| {
        if matches!(
            parser.current(),
            SyntaxKind::LayoutSep | SyntaxKind::LayoutEnd | SyntaxKind::EndOfFile
        ) || !parser.current().is_operator()
        {
            return false;
        }

        let mut pair = parser.start();
        parser.consume_as(SyntaxKind::Operator);
        binder_2(parser);
        pair.end(parser, SyntaxKind::Pair);

        true
    });

    if has_chain {
        operator.end(parser, SyntaxKind::BinderOperatorChain);
    } else {
        operator.cancel(parser);
    }
}

fn binder_2(parser: &mut Parser) {
    match parser.current() {
        SyntaxKind::Minus => {
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
        SyntaxKind::Upper => {
            let mut constructor = parser.start();
            let mut qualified_name = parser.start();
            qualified_prefix(parser);

            if parser.at(SyntaxKind::Upper) {
                upper_name_ref(parser);
                qualified_name.end(parser, SyntaxKind::QualifiedName);
            } else {
                parser.error_recover("expected Upper");
                qualified_name.cancel(parser);
            }

            one_or_more(parser, |parser| {
                if matches!(
                    parser.current(),
                    SyntaxKind::LayoutSep | SyntaxKind::LayoutEnd | SyntaxKind::EndOfFile
                ) || matches!(
                    parser.current(),
                    SyntaxKind::Pipe
                        | SyntaxKind::Equal
                        | SyntaxKind::LeftArrow
                        | SyntaxKind::RightArrow
                        | SyntaxKind::RightParenthesis
                ) {
                    return false;
                }

                binder_atom(parser);

                true
            });

            constructor.end(parser, SyntaxKind::ConstructorBinder);
        }
        _ => {
            binder_atom(parser);
        }
    }
}

fn binder_atom(parser: &mut Parser) {
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
        SyntaxKind::Lower => {
            lower_name(parser);
            marker.end(parser, SyntaxKind::VariableBinder);
        }
        SyntaxKind::Upper => {
            upper_name_ref(parser);
            marker.end(parser, SyntaxKind::ConstructorBinder);
        }
        SyntaxKind::LeftParenthesis => {
            parser.consume();
            binder(parser);
            parser.expect(SyntaxKind::RightParenthesis);
            marker.end(parser, SyntaxKind::ParenthesizedBinder);
        }
        SyntaxKind::LeftBracket => {
            panic!("Record Binder");
        }
        SyntaxKind::LeftSquare => {
            parser.error("todo: array binder");
            marker.cancel(parser);
        }
        _ => {
            marker.cancel(parser);
        }
    }
}

fn qualified_prefix(parser: &mut Parser) {
    let mut prefix = parser.start();

    let mut at_least_one = false;
    loop {
        if parser.at(SyntaxKind::Upper) && parser.nth_at(1, SyntaxKind::Period) {
            upper_name_ref(parser);
            parser.consume();
            at_least_one = true;
        } else {
            break;
        }
    }
    if at_least_one {
        prefix.end(parser, SyntaxKind::QualifiedPrefix);
    } else {
        prefix.cancel(parser);
    }
}

fn qualified_name_or_do_ado(parser: &mut Parser) -> Option<Either<SyntaxKind, SyntaxKind>> {
    let mut qualified_name = parser.start();

    qualified_prefix(parser);

    match parser.current() {
        SyntaxKind::Upper => {
            upper_name_ref(parser);
            qualified_name.end(parser, SyntaxKind::QualifiedName);
            Some(Left(SyntaxKind::ConstructorExpression))
        }
        SyntaxKind::Lower | SyntaxKind::AsKw => {
            lower_name_ref(parser);
            qualified_name.end(parser, SyntaxKind::QualifiedName);
            Some(Left(SyntaxKind::VariableExpression))
        }
        SyntaxKind::LeftParenthesis => {
            operator_name_ref(parser);
            qualified_name.end(parser, SyntaxKind::QualifiedName);
            Some(Left(SyntaxKind::OperatorNameExpression))
        }
        SyntaxKind::DoKw => {
            parser.consume();
            qualified_name.cancel(parser);
            Some(Right(SyntaxKind::QualifiedDo))
        }
        SyntaxKind::AdoKw => {
            parser.consume();
            qualified_name.cancel(parser);
            Some(Right(SyntaxKind::QualifiedAdo))
        }
        _ => {
            parser.error_recover("expected Upper, Lower, LeftParenthesis, DoKw, or AdoKw");
            None
        }
    }
}

fn upper_name_ref(parser: &mut Parser) {
    let mut name = parser.start();
    parser.expect(SyntaxKind::Upper);
    name.end(parser, SyntaxKind::NameRef);
}

fn lower_name_ref(parser: &mut Parser) {
    let mut name = parser.start();
    parser.consume_as(SyntaxKind::Lower);
    name.end(parser, SyntaxKind::NameRef);
}

fn lower_name(parser: &mut Parser) {
    let mut name = parser.start();
    parser.consume_as(SyntaxKind::Lower);
    name.end(parser, SyntaxKind::Name);
}

fn label_name(parser: &mut Parser) {
    let mut name = parser.start();
    parser.consume_as(SyntaxKind::Label);
    name.end(parser, SyntaxKind::Name);
}

fn operator_name_ref(parser: &mut Parser) {
    let mut wrapped = parser.start();
    parser.expect(SyntaxKind::LeftParenthesis);

    match parser.current() {
        SyntaxKind::Operator | SyntaxKind::Minus => {
            let mut name = parser.start();
            parser.consume_as(SyntaxKind::Operator);
            name.end(parser, SyntaxKind::NameRef);
        }
        _ => {
            parser.error_recover("expected Operator");
        }
    }

    parser.expect(SyntaxKind::RightParenthesis);
    wrapped.end(parser, SyntaxKind::Wrapped);
}

pub fn declaration(parser: &mut Parser) {
    match parser.current() {
        SyntaxKind::Lower => {
            annotation_or_value_declaration(parser);
        }
        _ => {
            parser.error("expected Declaration");
        }
    }
}

fn annotation_or_value_declaration(parser: &mut Parser) {
    let mut marker = parser.start();
    lower_name(parser);
    if parser.at(SyntaxKind::Colon2) {
        parser.consume();
        ty(parser);
        marker.end(parser, SyntaxKind::AnnotationDeclaration);
    } else {
        zero_or_more(parser, |parser| {
            if matches!(parser.current(), SyntaxKind::Equal | SyntaxKind::Pipe) {
                return false;
            }

            binder_atom(parser);

            true
        });
        guarded_binding(parser, SyntaxKind::Equal);
        marker.end(parser, SyntaxKind::ValueDeclaration);
    }
}
