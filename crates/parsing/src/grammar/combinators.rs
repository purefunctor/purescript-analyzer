use syntax::SyntaxKind;

use crate::parser::Parser;

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
