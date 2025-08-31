use async_lsp::lsp_types::*;
use building::QueryEngine;
use files::{FileId, Files};
use indexing::FullIndexedModule;
use parsing::ParsedModule;
use resolving::FullResolvedModule;
use rowan::{TokenAtOffset, ast::AstNode};
use smol_str::SmolStr;
use syntax::{SyntaxToken, cst};

use crate::locate;

pub struct Context<'c, 'a> {
    pub engine: &'c QueryEngine,
    pub files: &'c Files,

    pub id: FileId,
    pub content: &'a str,
    pub indexed: &'a FullIndexedModule,
    pub parsed: &'a ParsedModule,
    pub resolved: &'a FullResolvedModule,

    pub prim_id: FileId,
    pub prim_resolved: &'a FullResolvedModule,

    pub semantics: CursorSemantics,
    pub text: CursorText,
    pub range: Option<Range>,
}

impl Context<'_, '_> {
    pub fn insert_import_range(&self) -> Option<Range> {
        let cst = self.parsed.cst();

        let range = cst.imports().map_or_else(
            || {
                let header = cst.header()?;
                Some(header.syntax().text_range())
            },
            |cst| Some(cst.syntax().text_range()),
        )?;

        let mut position = locate::offset_to_position(self.content, range.end());

        position.line += 1;
        position.character = 0;

        Some(Range::new(position, position))
    }

    pub fn collect_modules(&self) -> bool {
        matches!(self.semantics, CursorSemantics::Module)
    }

    pub fn collect_terms(&self) -> bool {
        matches!(self.semantics, CursorSemantics::Term)
    }

    pub fn collect_types(&self) -> bool {
        matches!(self.semantics, CursorSemantics::Type)
    }

    pub fn collect_implicit_prim(&self) -> bool {
        self.resolved.unqualified.values().flatten().all(|import| import.file != self.prim_id)
    }

    pub fn has_qualified_import(&self, name: &str) -> bool {
        self.resolved.qualified.contains_key(name)
    }
}

/// A trait for describing completion sources.
pub trait Source {
    fn candidates<F: Filter>(
        &self,
        context: &Context,
        filter: F,
    ) -> impl Iterator<Item = CompletionItem>;
}

/// A trait for describing completion filters.
pub trait Filter: Copy {
    fn matches(&self, name: &str) -> bool;
}

#[derive(Debug)]
pub enum CursorSemantics {
    Term,
    Type,
    Module,
    General,
    Comment,
}

const COMPLETION_MARKER: &str = "Z'PureScript'Z";

impl CursorSemantics {
    pub fn new(content: &str, position: Position) -> CursorSemantics {
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
            return CursorSemantics::General;
        };

        let (left, right) = content.split_at(offset.into());
        let source = format!("{left}{COMPLETION_MARKER}{right}");

        let lexed = lexing::lex(&source);
        let tokens = lexing::layout(&lexed);
        let (parsed, _) = parsing::parse(&lexed, &tokens);

        let node = parsed.syntax_node();
        let token = node.token_at_offset(offset);

        let token = match token {
            TokenAtOffset::None => {
                return CursorSemantics::General;
            }
            TokenAtOffset::Single(token) => token,
            TokenAtOffset::Between(left, right) => {
                if left.text().contains(COMPLETION_MARKER) {
                    left
                } else if right.text().contains(COMPLETION_MARKER) {
                    right
                } else {
                    return CursorSemantics::General;
                }
            }
        };

        token
            .parent_ancestors()
            .find_map(|node| {
                let kind = node.kind();
                if cst::Annotation::can_cast(kind) {
                    Some(CursorSemantics::Comment)
                } else if cst::Expression::can_cast(kind) {
                    Some(CursorSemantics::Term)
                } else if cst::Type::can_cast(kind) || cst::ExpressionTypeArgument::can_cast(kind) {
                    Some(CursorSemantics::Type)
                } else if cst::ImportStatement::can_cast(kind) {
                    Some(CursorSemantics::Module)
                } else {
                    None
                }
            })
            .unwrap_or(CursorSemantics::General)
    }
}

#[derive(Debug)]
pub enum CursorText {
    None,
    Prefix(SmolStr),
    Name(SmolStr),
    Both(SmolStr, SmolStr),
}

impl CursorText {
    pub fn new(content: &str, token: &SyntaxToken) -> (CursorText, Option<Range>) {
        CursorText::of_qualified(content, token)
            .or_else(|| CursorText::of_qualifier(content, token))
            .or_else(|| CursorText::of_module_name(content, token))
            .unwrap_or((CursorText::None, None))
    }

    fn of_qualified(content: &str, token: &SyntaxToken) -> Option<(CursorText, Option<Range>)> {
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
            let text = match (prefix, name) {
                (None, None) => CursorText::None,
                (Some(p), None) => CursorText::Prefix(p),
                (None, Some(n)) => CursorText::Name(n),
                (Some(p), Some(n)) => CursorText::Both(p, n),
            };

            Some((text, range))
        })
    }

    fn of_qualifier(content: &str, token: &SyntaxToken) -> Option<(CursorText, Option<Range>)> {
        token.parent_ancestors().find_map(|node| {
            let qualifier = cst::Qualifier::cast(node)?;
            let token = qualifier.text()?;

            let prefix = token.text();
            let prefix = SmolStr::new(prefix);

            let range = token.text_range();
            let range = locate::text_range_to_range(content, range);

            let range = Some(range);
            let text = CursorText::Prefix(prefix);

            Some((text, range))
        })
    }

    fn of_module_name(content: &str, token: &SyntaxToken) -> Option<(CursorText, Option<Range>)> {
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
            let text = match (prefix, name) {
                (None, None) => CursorText::None,
                (Some(p), None) => CursorText::Prefix(p),
                (None, Some(n)) => CursorText::Name(n),
                (Some(p), Some(n)) => CursorText::Both(p, n),
            };

            Some((text, range))
        })
    }
}
