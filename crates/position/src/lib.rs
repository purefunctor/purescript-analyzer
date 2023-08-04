//! Generic source position information.

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    pub offset: usize,
    pub line: u32,
    pub column: u32,
}

impl Default for Position {
    fn default() -> Self {
        Position { offset: 0, line: 1, column: 1 }
    }
}
