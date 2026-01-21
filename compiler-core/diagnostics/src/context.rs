use checking::{CheckedModule, error::ErrorStep};
use indexing::IndexedModule;
use rowan::ast::{AstNode, AstPtr};
use stabilizing::StabilizedModule;
use syntax::{SyntaxNode, SyntaxNodePtr};

use crate::Span;

pub struct DiagnosticsContext<'a> {
    pub content: &'a str,
    pub root: &'a SyntaxNode,
    pub stabilized: &'a StabilizedModule,
    pub indexed: &'a IndexedModule,
    pub checked: &'a CheckedModule,
}

impl<'a> DiagnosticsContext<'a> {
    pub fn new(
        content: &'a str,
        root: &'a SyntaxNode,
        stabilized: &'a StabilizedModule,
        indexed: &'a IndexedModule,
        checked: &'a CheckedModule,
    ) -> DiagnosticsContext<'a> {
        DiagnosticsContext { content, root, stabilized, indexed, checked }
    }

    pub fn span_from_syntax_ptr(&self, ptr: &SyntaxNodePtr) -> Option<Span> {
        let range = ptr.to_node(self.root).text_range();
        Some(Span::new(range.start().into(), range.end().into()))
    }

    pub fn span_from_ast_ptr<N: AstNode<Language = syntax::PureScript>>(
        &self,
        ptr: &AstPtr<N>,
    ) -> Option<Span> {
        let ptr = ptr.syntax_node_ptr();
        self.span_from_syntax_ptr(&ptr)
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
        };
        self.span_from_syntax_ptr(&ptr)
    }

    pub fn primary_span_from_steps(&self, steps: &[ErrorStep]) -> Option<Span> {
        steps.last().and_then(|step| self.span_from_error_step(step))
    }
}
