use checking::CheckedModule;
use checking::error::ErrorStep;
use indexing::IndexedModule;
use lowering::LoweredModule;
use rowan::ast::{AstNode, AstPtr};
use rowan::{NodeOrToken, TextRange};
use stabilizing::StabilizedModule;
use syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxNodePtr};

use crate::Span;

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
    pub content: &'a str,
    pub root: &'a SyntaxNode,
    pub stabilized: &'a StabilizedModule,
    pub indexed: &'a IndexedModule,
    pub lowered: &'a LoweredModule,
    pub checked: &'a CheckedModule,
}

impl<'a> DiagnosticsContext<'a> {
    pub fn new(
        content: &'a str,
        root: &'a SyntaxNode,
        stabilized: &'a StabilizedModule,
        indexed: &'a IndexedModule,
        lowered: &'a LoweredModule,
        checked: &'a CheckedModule,
    ) -> DiagnosticsContext<'a> {
        DiagnosticsContext { content, root, stabilized, indexed, lowered, checked }
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

    pub fn span_from_error_step(&self, step: &ErrorStep) -> Option<Span> {
        let ptr = match step {
            ErrorStep::ConstructorArgument(id) => self.stabilized.syntax_ptr(*id)?,
            ErrorStep::InferringKind(id) | ErrorStep::CheckingKind(id) => {
                self.stabilized.syntax_ptr(*id)?
            }
            ErrorStep::InferringBinder(id) | ErrorStep::CheckingBinder(id) => {
                self.stabilized.syntax_ptr(*id)?
            }
            ErrorStep::InferringExpression(id) | ErrorStep::CheckingExpression(id) => {
                self.stabilized.syntax_ptr(*id)?
            }
            ErrorStep::TermDeclaration(id) => {
                self.indexed.term_item_ptr(self.stabilized, *id).next()?
            }
            ErrorStep::TypeDeclaration(id) => {
                self.indexed.type_item_ptr(self.stabilized, *id).next()?
            }
            ErrorStep::InferringDoBind(id)
            | ErrorStep::InferringDoDiscard(id)
            | ErrorStep::CheckingDoLet(id) => self.stabilized.syntax_ptr(*id)?,
            ErrorStep::InferringAdoMap(id)
            | ErrorStep::InferringAdoApply(id)
            | ErrorStep::CheckingAdoLet(id) => self.stabilized.syntax_ptr(*id)?,
            ErrorStep::CheckingLetName(id) => {
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

    pub fn primary_span_from_steps(&self, steps: &[ErrorStep]) -> Option<Span> {
        steps.last().and_then(|step| self.span_from_error_step(step))
    }
}
