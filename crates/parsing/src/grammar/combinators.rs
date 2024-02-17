use syntax::SyntaxKind;

use crate::parser::Parser;

pub(super) fn one_or_more(parser: &mut Parser, rule: impl Fn(&mut Parser) -> bool) -> bool {
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

pub(super) fn zero_or_more(parser: &mut Parser, rule: impl Fn(&mut Parser) -> bool) {
    let mut marker = parser.start();
    loop {
        if !rule(parser) {
            break;
        }
    }
    marker.end(parser, SyntaxKind::ZeroOrMore);
}

pub(super) fn attempt<T>(parser: &mut Parser, rule: impl Fn(&mut Parser) -> T) -> bool {
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

pub(super) fn separated(parser: &mut Parser, separator: SyntaxKind, rule: impl Fn(&mut Parser)) {
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

pub(super) fn separated_quiet(
    parser: &mut Parser,
    separator: SyntaxKind,
    rule: impl Fn(&mut Parser),
) {
    rule(parser);
    loop {
        if parser.at(separator) {
            parser.consume();
            rule(parser);
        } else {
            break;
        }
    }
}

pub(super) fn layout_one_or_more(parser: &mut Parser, rule: impl Fn(&mut Parser)) {
    if !parser.expect(SyntaxKind::LayoutStart) {
        return;
    }

    let mut marker = parser.start();

    rule(parser);

    loop {
        match parser.current() {
            SyntaxKind::LayoutSep => {
                parser.consume();
                rule(parser);
            }
            SyntaxKind::LayoutEnd => {
                marker.end(parser, SyntaxKind::OneOrMore);
                parser.consume();
                break;
            }
            _ => {
                marker.cancel(parser);
                parser.error("expected LayoutSep or LayoutEnd");
                break;
            }
        }
    }
}
