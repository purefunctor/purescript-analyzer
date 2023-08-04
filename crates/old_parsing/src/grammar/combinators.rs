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

/// Performs a `rule` and conditionally backtracks.
pub fn attempt<T>(parser: &mut Parser, rule: impl Fn(&mut Parser) -> T) -> bool {
    let mut save = parser.save();
    rule(parser);
    if save.has_error(parser) {
        save.load(parser);
        false
    } else {
        save.delete(parser);
        true
    }
}

/// Parses `rule`s separated by the `separator`.
pub fn separated(parser: &mut Parser, separator: SyntaxKind, rule: impl Fn(&mut Parser)) {
    let mut marker = parser.start();

    rule(parser);
    loop {
        if parser.at(separator) {
            parser.consume();
            rule(parser);
        } else {
            break;
        }
    }

    marker.end(parser, SyntaxKind::Separated);
}
