pub mod lexer;
pub mod syntax;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    pub offset: u32,
    pub line: u32,
    pub column: u32,
}

impl Position {
    pub fn from_source(source: &str, offset: usize) -> Position {
        assert!(offset <= source.len());

        let start = source[..offset].rfind("\n").map_or(0, |i| i + 1);
        let line = memchr::memchr_iter('\n' as u8, source[..start].as_bytes()).count() as u32 + 1;
        let column = source[start..offset as usize].chars().count() as u32 + 1;
        let offset = offset as u32;

        Position { offset, line, column }
    }
}
