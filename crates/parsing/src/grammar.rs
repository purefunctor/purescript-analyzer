use syntax::SyntaxKind;

use crate::parser::Parser;

pub fn expression(parser: &mut Parser) {
    let mut typed = parser.start();
    expression_1(parser);

    if parser.at(SyntaxKind::Colon2) {
        parser.consume();
        type_0(parser);
        typed.end(parser, SyntaxKind::TypedExpression);
    } else {
        typed.cancel(parser);
    }
}

fn expression_1(parser: &mut Parser) {
    let mut operator = parser.start();
    expression_2(parser);

    let mut one_or_more = parser.start();
    let mut entries = 0;
    loop {
        if parser.is_eof() {
            break;
        }

        if parser.at(SyntaxKind::Operator) {
            let mut pair = parser.start();
            parser.consume();
            expression_2(parser);
            pair.end(parser, SyntaxKind::Pair);
            entries += 1;
        } else {
            break;
        }
    }

    if entries > 0 {
        operator.end(parser, SyntaxKind::OperatorChain);
        one_or_more.end(parser, SyntaxKind::OneOrMore);
    } else {
        operator.cancel(parser);
        one_or_more.cancel(parser);
    }
}

fn expression_2(parser: &mut Parser) {
    let mut application = parser.start();
    expression_atom(parser);

    let mut arguments = 0;
    loop {
        if parser.is_eof() {
            break;
        }

        if expression_atom(parser) {
            arguments += 1;
        } else {
            break;
        }
    }

    if arguments > 0 {
        application.end(parser, SyntaxKind::ApplicationExpression);
    } else {
        application.cancel(parser);
    }
}

fn expression_atom(parser: &mut Parser) -> bool {
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
            true
        }
        SyntaxKind::LeftParenthesis => {
            parser.consume();
            expression(parser);
            parser.expect(SyntaxKind::RightParenthesis);
            marker.end(parser, SyntaxKind::ParenthesizedExpression);
            true
        }
        SyntaxKind::LeftSquare => {
            todo!("Array");
        }
        SyntaxKind::LeftBracket => {
            todo!("Record");
        }
        SyntaxKind::Upper | SyntaxKind::Lower | SyntaxKind::AsKw => {
            if let Some(kind) = qualified_name(parser) {
                marker.end(parser, kind);
                true
            } else {
                marker.cancel(parser);
                false
            }
        }
        _ => {
            marker.cancel(parser);
            false
        }
    }
}

fn qualified_prefix(parser: &mut Parser) {
    let mut prefix = parser.start();
    loop {
        if parser.at(SyntaxKind::Upper) && parser.nth_at(1, SyntaxKind::Period) {
            let mut name = parser.start();
            parser.consume();
            name.end(parser, SyntaxKind::NameRef);
            parser.consume();
        } else {
            break;
        }
    }
    prefix.end(parser, SyntaxKind::QualifiedPrefix);
}

fn qualified_name(parser: &mut Parser) -> Option<SyntaxKind> {
    let mut qualified = parser.start();

    qualified_prefix(parser);

    let mut name = parser.start();
    let kind = match parser.current() {
        SyntaxKind::Upper => {
            parser.consume();
            SyntaxKind::ConstructorExpression
        }
        SyntaxKind::Lower | SyntaxKind::AsKw => {
            parser.consume_as(SyntaxKind::Lower);
            SyntaxKind::VariableExpression
        }
        SyntaxKind::LeftParenthesis => {
            parser.consume();

            if parser.at(SyntaxKind::Operator) {
                parser.consume();
            } else {
                parser.error_recover("expected Operator");
            }

            parser.expect(SyntaxKind::RightParenthesis);

            SyntaxKind::OperatorNameExpression
        }
        _ => {
            name.cancel(parser);
            qualified.cancel(parser);
            parser.error_recover("expected Upper, Lower, or LeftParenthesis");
            return None;
        }
    };

    name.end(parser, SyntaxKind::NameRef);
    qualified.end(parser, SyntaxKind::QualifiedName);

    Some(kind)
}

pub fn type_0(parser: &mut Parser) {
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

    let mut one_or_more = parser.start();
    loop {
        if parser.is_eof() {
            break;
        }

        if parser.at(SyntaxKind::Period) {
            break;
        }

        type_variable_binding_with_visibility(parser);
    }
    one_or_more.end(parser, SyntaxKind::OneOrMore);

    parser.expect(SyntaxKind::Period);

    type_1(parser);

    forall.end(parser, SyntaxKind::ForallType);
}

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
