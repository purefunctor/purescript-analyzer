use std::{iter::Chain, str::Chars};

use async_lsp::lsp_types::*;
use rowan::{TokenAtOffset, ast::AstNode};
use smol_str::SmolStr;
use strsim::{generic_jaro_winkler, jaro_winkler};
use syntax::{SyntaxToken, cst};

use crate::locate;

#[derive(Debug, Default)]
pub(crate) struct CompletionFilter {
    pub(crate) prefix: Option<SmolStr>,
    pub(crate) name: Option<SmolStr>,
    pub(crate) range: Option<Range>,
}

impl CompletionFilter {
    pub(crate) fn new(content: &str, token: &SyntaxToken) -> CompletionFilter {
        CompletionFilter::try_qualified(content, token)
            .or_else(|| CompletionFilter::try_qualifier(content, token))
            .or_else(|| CompletionFilter::try_module_name(content, token))
            .unwrap_or_default()
    }

    fn try_qualified(content: &str, token: &SyntaxToken) -> Option<CompletionFilter> {
        token.parent_ancestors().find_map(|node| {
            let qualified = cst::QualifiedName::cast(node)?;

            let prefix_token = qualified.qualifier().and_then(|qualifier| qualifier.text());
            let prefix_range = prefix_token.as_ref().map(|token| token.text_range());
            let prefix = prefix_token.map(|token| token.text().into());

            let name_token = qualified.lower().or_else(|| qualified.upper());
            let name_range = name_token.as_ref().map(|token| token.text_range());
            let name = name_token.map(|token| token.text().into());

            let range = match (prefix_range, name_range) {
                (Some(p), Some(n)) => Some(p.cover(n)),
                (Some(r), None) => Some(r),
                (None, Some(r)) => Some(r),
                (None, None) => None,
            };

            let range = range.map(|range| locate::text_range_to_range(content, range));
            Some(CompletionFilter { prefix, name, range })
        })
    }

    fn try_qualifier(content: &str, token: &SyntaxToken) -> Option<CompletionFilter> {
        token.parent_ancestors().find_map(|node| {
            let qualifier = cst::Qualifier::cast(node)?;
            let token = qualifier.text()?;

            let prefix = token.text();
            let prefix = SmolStr::new(prefix);

            let range = token.text_range();
            let range = locate::text_range_to_range(content, range);

            let prefix = Some(prefix);
            let name = None;
            let range = Some(range);

            Some(CompletionFilter { prefix, name, range })
        })
    }

    fn try_module_name(content: &str, token: &SyntaxToken) -> Option<CompletionFilter> {
        token.parent_ancestors().find_map(|node| {
            let module_name = cst::ModuleName::cast(node)?;

            let prefix_token = module_name.qualifier().and_then(|qualifier| qualifier.text());
            let prefix_range = prefix_token.as_ref().map(|token| token.text_range());
            let prefix = prefix_token.map(|token| token.text().into());

            let name_token = module_name.name_token();
            let name_range = name_token.as_ref().map(|token| token.text_range());
            let name = name_token.map(|token| token.text().into());

            let range = match (prefix_range, name_range) {
                (Some(p), Some(n)) => Some(p.cover(n)),
                (Some(r), None) => Some(r),
                (None, Some(r)) => Some(r),
                (None, None) => None,
            };

            let range = range.map(|range| locate::text_range_to_range(content, range));
            Some(CompletionFilter { prefix, name, range })
        })
    }

    pub(crate) fn prefix_score(&self, other: &str) -> f64 {
        if let Some(prefix) = &self.prefix { jaro_winkler(prefix, other) } else { 1.0 }
    }

    pub(crate) fn name_score(&self, other: &str) -> f64 {
        if let Some(name) = &self.name { jaro_winkler(name, other) } else { 1.0 }
    }

    pub(crate) fn full_score(&self, other: &str) -> f64 {
        struct Full<'a> {
            prefix: &'a str,
            name: &'a str,
        }

        struct Other<'a> {
            other: &'a str,
        }

        impl<'b> IntoIterator for &Full<'b> {
            type Item = char;
            type IntoIter = Chain<Chars<'b>, Chars<'b>>;

            fn into_iter(self) -> Self::IntoIter {
                let prefix = self.prefix.chars();
                let name = self.name.chars();
                prefix.chain(name)
            }
        }

        impl<'b> IntoIterator for &Other<'b> {
            type Item = char;
            type IntoIter = Chars<'b>;

            fn into_iter(self) -> Self::IntoIter {
                self.other.chars()
            }
        }

        match (&self.prefix, &self.name) {
            (Some(prefix), Some(name)) => {
                let full = Full { prefix, name };
                let other = Other { other };
                generic_jaro_winkler(&full, &other)
            }
            (Some(prefix), None) => jaro_winkler(prefix, other),
            (None, Some(name)) => jaro_winkler(name, other),
            (None, None) => 1.0,
        }
    }
}

#[derive(Debug)]
pub(crate) enum CompletionLocation {
    Term,
    Type,
    Module,
    General,
    Comment,
}

impl CompletionLocation {
    pub(crate) fn new(content: &str, position: Position) -> CompletionLocation {
        // We insert a placeholder identifier at the current position of the
        // text cursor. This is done as an effort to produce as valid of a
        // parse tree as possible before we perform further analysis.
        //
        // This is particularly helpful for incomplete qualified names. Since
        // the parser represents qualifiers as "trivia" for the current token,
        // the following source string yields a lexing error:
        //
        // component = Halogen.
        //
        // Inserting a placeholder gets rid of this error, allowing the parser
        // to produce a valid parse tree that we can use for analysis:
        //
        // component = Halogen.z'PureScript'z

        let Some(offset) = locate::position_to_offset(content, position) else {
            return CompletionLocation::General;
        };

        let (left, right) = content.split_at(offset.into());
        let source = format!("{left}z'PureScript'z{right}");

        let lexed = lexing::lex(&source);
        let tokens = lexing::layout(&lexed);
        let (parsed, _) = parsing::parse(&lexed, &tokens);

        let node = parsed.syntax_node();
        let token = node.token_at_offset(offset);

        let token = match token {
            TokenAtOffset::None => {
                return CompletionLocation::General;
            }
            TokenAtOffset::Single(token) => token,
            TokenAtOffset::Between(token, _) => token,
        };

        token
            .parent_ancestors()
            .find_map(|node| {
                let kind = node.kind();
                if cst::Annotation::can_cast(kind) {
                    Some(CompletionLocation::Comment)
                } else if cst::Expression::can_cast(kind) {
                    Some(CompletionLocation::Term)
                } else if cst::Type::can_cast(kind) || cst::ExpressionTypeArgument::can_cast(kind) {
                    Some(CompletionLocation::Type)
                } else if cst::ImportStatement::can_cast(kind) {
                    Some(CompletionLocation::Module)
                } else {
                    None
                }
            })
            .unwrap_or(CompletionLocation::General)
    }
}
