use std::fs::read_to_string;

use glob::glob;

fn main() {
    let mut with_error = 0;
    for file in glob("packages/**/*.purs").unwrap() {
        let Ok(file) = file else {
            continue;
        };
        let Ok(source) = read_to_string(file.clone()) else {
            continue;
        };

        let lexed = lexing::lex(&source);
        let tokens = lexing::layout(&lexed);

        let (_, errors) = parsing::parse(&lexed, &tokens);
        if !errors.is_empty() {
            println!("File: {:?}", file);
            println!("Errors: {:#?}", errors);
            with_error += 1;
        }
    }
    println!("Files remaining: {}", with_error);
}
