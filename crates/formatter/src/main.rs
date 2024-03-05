//! The formatter.

use std::fs;

use gumdrop::Options;
use syntax::SyntaxNode;

#[derive(Options)]
struct Args {
    #[options(help = "print help message")]
    help: bool,

    #[options(required, free, help = "the source file to parse")]
    source_file: String,
}

pub fn main() {
    let args = Args::parse_args_default_or_exit();
    let source =
        fs::read_to_string(args.source_file).expect("Should have been able to read the file");

    let (node, errors) = parsing::parse_module(&source);

    render(node, 0);

    println!("{:?}", errors);
}

fn render(node: SyntaxNode, indentation: usize) {
    println!("{:indentation$}{:?}", "", node.kind(), indentation = indentation);
    for child in node.children() {
        render(child, indentation + 1);
    }
}
