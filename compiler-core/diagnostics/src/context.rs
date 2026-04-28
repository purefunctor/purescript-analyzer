use checking::error::ErrorCrumb;
use indexing::IndexedModule;
use lowering::LoweredModule;
use rowan::ast::{AstNode, AstPtr};
use rowan::{NodeOrToken, TextRange};
use stabilizing::StabilizedModule;
use syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxNodePtr};

use crate::Span;

pub trait ExternalQueries {
    fn lookup_checking_smol_str(&self, id: checking::core::SmolStrId) -> smol_str::SmolStr;
}

impl<T> ExternalQueries for T
where
    T: checking::ExternalQueries + ?Sized,
{
    fn lookup_checking_smol_str(&self, id: checking::core::SmolStrId) -> smol_str::SmolStr {
        self.lookup_smol_str(id)
    }
}

fn is_trivia(element: &SyntaxElement) -> bool {
    match element {
        NodeOrToken::Node(node) => matches!(node.kind(), SyntaxKind::Annotation),
        NodeOrToken::Token(token) => {
            token.text_range().is_empty()
                || matches!(token.kind(), SyntaxKind::LAYOUT_SEPARATOR | SyntaxKind::ELSE)
        }
    }
}

fn first_significant_range(node: &SyntaxNode) -> Option<TextRange> {
    for child in node.children_with_tokens() {
        if is_trivia(&child) {
            continue;
        }
        match child {
            NodeOrToken::Token(token) => return Some(token.text_range()),
            NodeOrToken::Node(node) => {
                if let Some(range) = first_significant_range(&node) {
                    return Some(range);
                }
            }
        }
    }
    None
}

fn last_significant_range(node: &SyntaxNode) -> Option<TextRange> {
    let mut significant = None;
    for child in node.children_with_tokens() {
        if is_trivia(&child) {
            continue;
        }
        match child {
            NodeOrToken::Token(token) => significant = Some(token.text_range()),
            NodeOrToken::Node(node) => {
                if let Some(range) = last_significant_range(&node) {
                    significant = Some(range);
                }
            }
        }
    }
    significant
}

fn significant_ranges(node: &SyntaxNode) -> Option<TextRange> {
    let start = first_significant_range(node)?;
    let end = last_significant_range(node)?;
    Some(start.cover(end))
}

fn pointers_span<I>(context: &DiagnosticsContext<'_>, iterator: I) -> Option<Span>
where
    I: IntoIterator<Item = SyntaxNodePtr>,
    I::IntoIter: DoubleEndedIterator,
{
    let mut iter = iterator.into_iter();

    let start_ptr = iter.next()?;
    let end_ptr = iter.next_back().unwrap_or(start_ptr);

    let start_span = context.span_from_syntax_ptr(&start_ptr)?;
    let end_span = context.span_from_syntax_ptr(&end_ptr)?;

    Some(Span::new(start_span.start, end_span.end))
}

pub struct DiagnosticsContext<'a> {
    pub queries: &'a dyn ExternalQueries,
    pub content: &'a str,
    pub root: &'a SyntaxNode,
    pub stabilized: &'a StabilizedModule,
    pub indexed: &'a IndexedModule,
    pub lowered: &'a LoweredModule,
}

impl<'a> DiagnosticsContext<'a> {
    pub fn new(
        queries: &'a dyn ExternalQueries,
        content: &'a str,
        root: &'a SyntaxNode,
        stabilized: &'a StabilizedModule,
        indexed: &'a IndexedModule,
        lowered: &'a LoweredModule,
    ) -> DiagnosticsContext<'a> {
        DiagnosticsContext { queries, content, root, stabilized, indexed, lowered }
    }

    pub fn span_from_syntax_ptr(&self, ptr: &SyntaxNodePtr) -> Option<Span> {
        let node = ptr.try_to_node(self.root)?;
        self.span_from_syntax_node(&node)
    }

    pub fn span_from_ast_ptr<N: AstNode<Language = syntax::PureScript>>(
        &self,
        ptr: &AstPtr<N>,
    ) -> Option<Span> {
        let node = ptr.try_to_node(self.root)?;
        self.span_from_syntax_node(node.syntax())
    }

    fn span_from_syntax_node(&self, node: &SyntaxNode) -> Option<Span> {
        let range = significant_ranges(node)?;
        Some(Span::new(range.start().into(), range.end().into()))
    }

    pub fn text_of(&self, span: Span) -> &'a str {
        &self.content[span.start as usize..span.end as usize]
    }

    pub fn module_span(&self) -> Span {
        let range = if let Some(range) = significant_ranges(self.root) {
            range
        } else {
            self.root.text_range()
        };

        let start = range.start().into();
        let end = range.end().into();

        Span::new(start, end)
    }

    pub fn span_from_error_crumb(&self, crumb: &ErrorCrumb) -> Option<Span> {
        let ptr = match crumb {
            ErrorCrumb::ConstructorArgument(id) => self.stabilized.syntax_ptr(*id)?,
            ErrorCrumb::InferringKind(id) | ErrorCrumb::CheckingKind(id) => {
                self.stabilized.syntax_ptr(*id)?
            }
            ErrorCrumb::InferringBinder(id) | ErrorCrumb::CheckingBinder(id) => {
                self.stabilized.syntax_ptr(*id)?
            }
            ErrorCrumb::InferringExpression(id) | ErrorCrumb::CheckingExpression(id) => {
                self.stabilized.syntax_ptr(*id)?
            }
            ErrorCrumb::TermDeclaration(id) => {
                self.indexed.term_item_ptr(self.stabilized, *id).next()?
            }
            ErrorCrumb::TypeDeclaration(id) => {
                self.indexed.type_item_ptr(self.stabilized, *id).next()?
            }
            ErrorCrumb::InferringDoBind(id)
            | ErrorCrumb::InferringDoDiscard(id)
            | ErrorCrumb::CheckingDoLet(id) => self.stabilized.syntax_ptr(*id)?,
            ErrorCrumb::InferringAdoMap(id)
            | ErrorCrumb::InferringAdoApply(id)
            | ErrorCrumb::CheckingAdoLet(id) => self.stabilized.syntax_ptr(*id)?,
            ErrorCrumb::CheckingLetName(id) => {
                let group = self.lowered.info.get_let_binding_group(*id);

                let signature = group
                    .signature
                    .as_slice()
                    .iter()
                    .filter_map(|signature| self.stabilized.syntax_ptr(*signature));

                let equations = group
                    .equations
                    .as_ref()
                    .iter()
                    .filter_map(|equation| self.stabilized.syntax_ptr(*equation));

                return pointers_span(self, signature.chain(equations));
            }
        };
        self.span_from_syntax_ptr(&ptr)
    }

    pub fn primary_span_from_crumbs(&self, crumbs: &[ErrorCrumb]) -> Span {
        let primary = crumbs.iter().rev().find_map(|crumb| self.span_from_error_crumb(crumb));
        if let Some(primary) = primary { primary } else { self.module_span() }
    }
}
