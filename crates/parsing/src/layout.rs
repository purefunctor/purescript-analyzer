use crate::position::Position;

#[derive(Debug)]
pub struct Layout {
    pub kind: LayoutKind,
    pub position: Position,
}

#[derive(Debug)]
pub enum LayoutKind {
    Root,
    Module,
    Instance,
}

impl Layout {
    pub fn new(kind: LayoutKind, position: Position) -> Layout {
        Layout { kind, position }
    }
}

/*

# Root

Always starts at offset 0, line 1, column 1, regardless
of the first token's current position. At the moment, this
is just a sentinel

# Module

A layout that starts after the module header, and allows
for subsequent declarations to be indented by `n` spaces,
where `n` is determined by the indentation of the first
token after `where`. Since this is an indentation context,
tokens appearing before `n` break this context, while
tokens appearing at `n` finish a group.

# Instance

A layout that starts in an instance declaration, and allows
for subsequent declarations to be indented by `n` spaces,
where `n` is determined by the indentation of the first
token after `where`. Since this is an indentation context,
tokens appearing before `n` break this context, while
tokens appearing at `n` finish a group.

*/
