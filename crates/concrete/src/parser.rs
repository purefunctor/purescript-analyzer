use std::iter::Peekable;

use rowan::{GreenNodeBuilder, NodeOrToken};

use crate::syntax::{SyntaxElement, SyntaxKind, SyntaxNode};

type Token = (usize, usize, SyntaxKind);

struct Parser<'a, I: Iterator<Item = Token>> {
    source: &'a str,
    tokens: Peekable<I>,

    builder: GreenNodeBuilder<'static>,
    errors: Vec<String>,
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    fn new(source: &'a str, tokens: I) -> Parser<'a, I> {
        let tokens = tokens.peekable();
        let builder = GreenNodeBuilder::default();
        let errors = vec![];
        Parser { source, tokens, builder, errors }
    }
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    fn peek(&mut self) -> Option<SyntaxKind> {
        while let Some((_, _, SyntaxKind::Whitespace)) = self.tokens.peek() {
            self.bump();
        }
        self.tokens.peek().map(|&(_, _, kind)| kind)
    }

    fn bump(&mut self) {
        if let Some((begin, end, kind)) = self.tokens.next() {
            self.builder.token(kind.into(), &self.source[begin..end]);
        }
    }

    fn parse_module(mut self) -> SyntaxNode {
        self.builder.start_node(SyntaxKind::Module.into());
        self.parse_module_declaration();
        self.builder.finish_node();
        SyntaxNode::new_root(self.builder.finish())
    }

    fn parse_module_declaration(&mut self) {
        if let Some(SyntaxKind::ModuleKw) = self.peek() {
            self.bump();
            self.bump();
            self.parse_qualified_name();
        }
    }

    fn parse_qualified_name(&mut self) {
        self.builder.start_node(SyntaxKind::Qualified.into());
        if let Some(SyntaxKind::Upper) = self.peek() {
            self.builder.start_node(SyntaxKind::Name.into());
            self.bump();
            self.builder.finish_node();

            loop {
                if let Some(SyntaxKind::Period) = self.peek() {
                    self.bump();
                    if let Some(SyntaxKind::Upper) = self.peek() {
                        self.builder.start_node(SyntaxKind::Name.into());
                        self.bump();
                        self.builder.finish_node();
                    }
                } else {
                    break;
                }
            }
        }
        self.builder.finish_node();
    }
}

fn print(indent: usize, element: SyntaxElement) {
    let kind: SyntaxKind = element.kind().into();
    print!("{:indent$}", "", indent = indent);
    match element {
        NodeOrToken::Node(node) => {
            println!("- {:?}", kind);
            for child in node.children_with_tokens() {
                print(indent + 2, child);
            }
        }
        NodeOrToken::Token(token) => println!("- {:?} {:?}", token.text(), kind),
    }
}

#[test]
fn parsing_test() {
    let source = "module Hello.World where";
    let tokens = crate::lexer::lex(source);
    let parser = Parser::new(source, tokens.into_iter());
    print(2, parser.parse_module().into());
}

/*

Module
  ModuleDeclaration
    ModuleKw
    Qualified
      Name
        Upper
      Name
        Upper
    ExportList
      NameRef
        Upper
      Comma
      NameRef
        Lower
      Comma
      NameRef
        Upper
    WhereKw

  ImportDeclaration
    ImportKw
    Qualified
      NameRef
        Upper
      NameRef
        Upper
    ImportList
      NameRef
        Upper
    As
      AsKw
      Name
*/
