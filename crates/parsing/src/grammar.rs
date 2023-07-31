use syntax::SyntaxKind;

use crate::parser::Parser;

pub fn expression(parser: &mut Parser) {
    let mut typed = parser.start();
    expression_1(parser);

    if parser.at(SyntaxKind::Colon2) {
        parser.consume();
        type_(parser);
        typed.end(parser, SyntaxKind::TypedExpression);
    } else {
        typed.cancel(parser);
    }
}

fn expression_1(parser: &mut Parser) {
    let mut operator = parser.start();
    expression_2(parser);

    let mut entries = 0;
    loop {
        if parser.is_eof() {
            break;
        }

        if parser.at(SyntaxKind::Operator) {
            parser.consume();
            expression_2(parser);
            entries += 1;
        } else {
            break;
        }
    }

    if entries > 0 {
        operator.end(parser, SyntaxKind::OperatorChain);
    } else {
        operator.cancel(parser);
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
            return true;
        }
        _ => (),
    }

    marker.cancel(parser);
    false
}

pub fn qualified_prefix(parser: &mut Parser) {
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

pub fn qualified_name(parser: &mut Parser) {
    let mut qualified = parser.start();

    qualified_prefix(parser);

    match parser.current() {
        SyntaxKind::Upper | SyntaxKind::Lower | SyntaxKind::AsKw => {
            let mut name = parser.start();
            if parser.at(SyntaxKind::AsKw) {
                parser.consume_as(SyntaxKind::Lower);
            } else {
                parser.consume();
            }
            name.end(parser, SyntaxKind::NameRef);
        }
        SyntaxKind::LeftParenthesis => {
            parser.consume();

            let mut name = parser.start();
            if parser.at(SyntaxKind::Operator) {
                parser.consume();
            } else {
                parser.error_recover("expected an operator");
            }
            name.end(parser, SyntaxKind::NameRef);

            parser.expect(SyntaxKind::RightParenthesis);
        }
        _ => {
            parser.error_recover("expected a name");
        }
    }

    qualified.end(parser, SyntaxKind::QualifiedName);
}

pub fn type_(parser: &mut Parser) {
    type_atom(parser);
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
