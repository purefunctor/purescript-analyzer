use syntax::SyntaxKind;

use crate::parser::Parser;

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
