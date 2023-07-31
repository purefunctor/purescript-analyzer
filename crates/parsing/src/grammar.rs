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
        SyntaxKind::LeftParenthesis => {
            parser.consume();
            expression(parser);
            parser.expect(SyntaxKind::RightParenthesis);
            marker.end(parser, SyntaxKind::ParenthesizedExpression);
            return true;
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
                return true;
            } else {
                marker.cancel(parser);
                return false;
            }
        }
        _ => {
            marker.cancel(parser);
            return false;
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
