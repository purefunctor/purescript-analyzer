use std::sync::Arc;

use lexing::{Lexed, Position};
use rowan::{ast::AstNode, GreenNode};
use syntax::{cst, SyntaxKind, SyntaxNode};

mod builder;
mod parser;

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
    pub offset: usize,
    pub position: Position,
    pub message: Arc<str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedModule {
    node: GreenNode,
}

impl ParsedModule {
    pub(crate) fn new(node: GreenNode) -> ParsedModule {
        ParsedModule { node }
    }

    pub fn syntax_node(&self) -> SyntaxNode {
        let node = self.node.clone();
        SyntaxNode::new_root(node)
    }

    pub fn cst(self) -> cst::Module {
        let node = self.syntax_node();
        cst::Module::cast(node).expect("invariant violated: expected cst::Module")
    }
}

pub type FullParsedModule = (ParsedModule, Arc<[ParseError]>);

pub fn parse(lexed: &Lexed<'_>, tokens: &[SyntaxKind]) -> FullParsedModule {
    let mut parser = parser::Parser::new(tokens);
    parser::module(&mut parser);

    let output = parser.finish();
    let (parsed, errors) = builder::build(lexed, output);

    (parsed, Arc::from(errors))
}
