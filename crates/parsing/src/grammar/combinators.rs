use syntax::SyntaxKind;

use crate::parser::Parser;

/// Performs `rule` until it returns `false`.
pub fn one_or_more(parser: &mut Parser, rule: impl Fn(&mut Parser) -> bool) -> bool {
    let mut marker = parser.start();
    let mut at_least_one = false;
    loop {
        if !rule(parser) {
            break;
        }
        at_least_one = true;
    }
    if at_least_one {
        marker.end(parser, SyntaxKind::OneOrMore);
    } else {
        marker.cancel(parser);
    }
    at_least_one
}

/// Performs `rule` until it returns `false.
pub fn zero_or_more(parser: &mut Parser, rule: impl Fn(&mut Parser) -> bool) {
    let mut marker = parser.start();
    loop {
        if !rule(parser) {
            break;
        }
    }
    marker.end(parser, SyntaxKind::ZeroOrMore);
}
