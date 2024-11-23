pub mod lexer;
pub mod syntax;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    pub offset: u32,
    pub line: u32,
    pub column: u32,
}
