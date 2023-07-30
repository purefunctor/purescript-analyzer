use syntax::SyntaxKind;

use crate::parser::Parser;

/// module <qualified_propper_name> <export_list> where <imports>
pub fn module_header(parser: &mut Parser) {
    let mut module = parser.start();
    parser.expect(SyntaxKind::ModuleKw);
    qualified_propper_name(parser);
    export_list(parser);

    module.end(parser, SyntaxKind::Module);
}

/// "" | "(a, (+), module B, class C, T, type (+))"
pub fn export_list(parser: &mut Parser) {
    let mut marker = parser.start();
    if parser.at(SyntaxKind::LeftParenthesis) {
        parser.expect(SyntaxKind::LeftParenthesis);
        loop {
            let mut export = parser.start();
            let kind = match parser.current() {
                SyntaxKind::RightParenthesis => {
                    export.cancel(parser);
                    parser.expect(SyntaxKind::RightParenthesis);
                    break;
                }
                SyntaxKind::Lower => {
                    qualified_name(parser);
                    SyntaxKind::ExportName
                }
                SyntaxKind::LeftParenthesis => {
                    parser.expect(SyntaxKind::LeftParenthesis);
                    parser.expect(SyntaxKind::Operator);
                    parser.expect(SyntaxKind::RightParenthesis);
                    SyntaxKind::ExportOperator
                }
                SyntaxKind::TypeKw if parser.nth_at(1, SyntaxKind::LeftParenthesis) => {
                    parser.expect(SyntaxKind::TypeKw);
                    parser.expect(SyntaxKind::LeftParenthesis);
                    parser.expect(SyntaxKind::Operator);
                    parser.expect(SyntaxKind::RightParenthesis);
                    SyntaxKind::ExportTypeOperator
                }
                SyntaxKind::Upper => {
                    parser.expect(SyntaxKind::TypeKw);
                    qualified_propper_name(parser);
                    SyntaxKind::ExportType
                }
                SyntaxKind::ModuleKw => {
                    parser.expect(SyntaxKind::ModuleKw);
                    qualified_propper_name(parser);
                    SyntaxKind::ExportModule
                }
                SyntaxKind::ClassKw => {
                    parser.expect(SyntaxKind::ClassKw);
                    qualified_propper_name(parser);
                    SyntaxKind::ExportClass
                }

                t => {
                    export.cancel(parser);
                    marker.cancel(parser);
                    parser.error(format!("Invalid export list, {:?} does not start an export", t));
                    return;
                }
            };
            export.end(parser, kind);
            if parser.at(SyntaxKind::RightParenthesis) {
                continue
            } else {
                parser.expect(SyntaxKind::Comma);
            }
        }
    }
    marker.end(parser, SyntaxKind::ExportList);
}

/// "" | "A.B."
fn qualified_prefix(parser: &mut Parser) {
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
}

/// "A.B.c"
pub fn qualified_name(parser: &mut Parser) {
    let mut qualified = parser.start();

    qualified_prefix(parser);
    if parser.at(SyntaxKind::Lower) || parser.at(SyntaxKind::AsKw) {
        if parser.at(SyntaxKind::AsKw) {
            parser.consume_as(SyntaxKind::Lower);
        } else {
            parser.consume();
        }
    } else {
        parser.error("Not a valid qualified name");
    }

    qualified.end(parser, SyntaxKind::QualifiedName);
}

/// "A.B.C"
pub fn qualified_propper_name(parser: &mut Parser) {
    let mut qualified = parser.start();

    qualified_prefix(parser);
    if parser.at(SyntaxKind::Upper) {
        if parser.at(SyntaxKind::AsKw) {
            parser.consume_as(SyntaxKind::Lower);
        } else {
            parser.consume();
        }
    } else {
        parser.error("Not a valid qualified propper name");
    }

    qualified.end(parser, SyntaxKind::QualifiedPropperName);
}
