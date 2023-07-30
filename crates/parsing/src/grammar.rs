use syntax::SyntaxKind;

use crate::parser::Parser;

pub fn qualified_name(parser: &mut Parser) {
    let mut qualified = parser.start();

    let mut prefix = parser.start();
    loop {
        if parser.at(SyntaxKind::Upper) && parser.nth_at(1, SyntaxKind::Period) {
            parser.consume();
            parser.consume();
        } else {
            break;
        }
    }
    prefix.end(parser, SyntaxKind::QualifiedPrefix);

    if parser.at(SyntaxKind::Upper) || parser.at(SyntaxKind::Lower) || parser.at(SyntaxKind::AsKw) {
        if parser.at(SyntaxKind::AsKw) {
            parser.consume_as(SyntaxKind::Lower);
        } else {
            parser.consume();
        }
    }

    qualified.end(parser, SyntaxKind::QualifiedName);
}
