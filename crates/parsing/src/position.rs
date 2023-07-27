//! Position information--could probably be its own crate.

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    pub offset: usize,
    pub line: u32,
    pub column: u32,
}
