//! The formatter.

use std::fs;

use gumdrop::Options;

#[derive(Options)]
struct Args {
    #[options(help = "print help message")]
    help: bool,

    #[options(required, free, help = "the source file to parse")]
    source_files: Vec<String>,
}

pub fn main() {
    let args = Args::parse_args_default_or_exit();
    for file in args.source_files.iter() {
        let source =
            fs::read_to_string(file).expect("Should have been able to read the file");

        let (_node, errors) = parsing::parse_module(&source);
        // render(node, 0);
        println!("{}:{:?}", file, errors);
    }
    println!("{}", args.source_files.len());
}

// fn render(node: SyntaxNode, indentation: usize) {
//     println!("{:indentation$}{:?}", "", node.kind(), indentation = indentation);
//     for child in node.children() {
//         render(child, indentation + 1);
//     }
// }
