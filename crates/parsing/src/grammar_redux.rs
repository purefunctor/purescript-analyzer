//! Implements a recursive descent parser for PureScript.

#[cfg(test)]
mod tests;

use syntax::SyntaxKind;

use crate::Parser;

// region: Names

fn name(p: &mut Parser, kind: SyntaxKind) {
    let mut m = p.start();
    p.consume_as(kind);
    m.end(p, SyntaxKind::Name);
}

fn upper(p: &mut Parser, message: &str) {
    if p.at(SyntaxKind::Upper) {
        name(p, SyntaxKind::Upper);
    } else {
        p.error_recover(message);
    }
}

fn module_name(p: &mut Parser) {
    fn at_end(p: &Parser) -> bool {
        let current = p.current();
        current.is_end() || matches!(current, SyntaxKind::LeftParenthesis | SyntaxKind::WhereKw)
    }

    fn segment(p: &mut Parser) {
        // If we're at the end, don't eat.
        if at_end(p) {
            p.error("expected a module name");
        } else {
            upper(p, "expected a module name");
        }
    }

    fn separator(p: &mut Parser) -> bool {
        // If we're at the end, just stop.
        if at_end(p) {
            return false;
        }

        // If we see a period, great!
        // Otherwise, begin recovery.
        if p.eat(SyntaxKind::Period) {
            return true;
        }

        // Otherwise, recover and continue.
        p.error_recover("expected a period");
        true
    }

    let mut m = p.start();
    p.separated(segment, separator);
    m.end(p, SyntaxKind::ModuleName);
}

// end

// region: Module

fn module(p: &mut Parser) {
    let mut m = p.start();
    module_header(p);
    m.end(p, SyntaxKind::Module);
}

fn module_header(p: &mut Parser) {
    let mut m = p.start();
    p.expect_recover(SyntaxKind::ModuleKw);
    module_name(p);
    if p.at(SyntaxKind::LeftParenthesis) {
        export_list(p);
    }
    p.expect_recover(SyntaxKind::WhereKw);
    m.end(p, SyntaxKind::ModuleHeader);
}

fn export_list(p: &mut Parser) {
    let mut m = p.start();
    p.expect(SyntaxKind::LeftParenthesis);
    if p.eat(SyntaxKind::RightParenthesis) {
        return m.end(p, SyntaxKind::ExportList);
    }

    fn at_end(p: &mut Parser) -> bool {
        let current = p.current();
        current.is_end() || matches!(current, SyntaxKind::RightParenthesis | SyntaxKind::WhereKw)
    }

    fn segment(p: &mut Parser) {
        if at_end(p) {
            p.error("unexpected end");
        } else {
            export_item(p);
        }
    }

    fn separator(p: &mut Parser) -> bool {
        if at_end(p) {
            return false;
        }

        if p.eat(SyntaxKind::Comma) {
            return true;
        }

        p.error_recover("expected a comma");
        true
    }

    p.separated(segment, separator);
    p.expect(SyntaxKind::RightParenthesis);
    m.end(p, SyntaxKind::ExportList);
}

fn export_item(p: &mut Parser) {
    name(p, SyntaxKind::Lower);
}

fn module_imports(p: &mut Parser) {}

fn module_body(p: &mut Parser) {}

// endregion
