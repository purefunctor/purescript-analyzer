use syntax::SyntaxKind;

use crate::parser::Parser;

pub fn expression_atom(parser: &mut Parser) {
    match parser.current() {
        SyntaxKind::LiteralChar
        | SyntaxKind::LiteralString
        | SyntaxKind::LiteralRawString
        | SyntaxKind::LiteralInteger
        | SyntaxKind::LiteralNumber
        | SyntaxKind::LiteralTrue
        | SyntaxKind::LiteralFalse => {
            parser.consume();
        }
        _ => (),
    }
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
