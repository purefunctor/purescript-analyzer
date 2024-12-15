#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
#[allow(non_camel_case_types)]
pub enum SyntaxKind {
    // Comments
    WHITESPACE = 0,
    LINE_COMMENT,
    BLOCK_COMMENT,

    // Names
    PREFIX,
    UPPER,
    LOWER,
    OPERATOR,

    // Operators
    AT,
    BACKSLASH,
    COLON,
    DOUBLE_COLON,
    COMMA,
    EQUAL,
    MINUS,
    PERIOD,
    DOUBLE_PERIOD,
    PIPE,
    QUESTION,
    TICK,
    UNDERSCORE,
    LEFT_ARROW,
    RIGHT_ARROW,
    LEFT_THICK_ARROW,
    RIGHT_THICK_ARROW,
    LEFT_PARENTHESIS,
    RIGHT_PARENTHESIS,
    LEFT_CURLY,
    RIGHT_CURLY,
    LEFT_SQUARE,
    RIGHT_SQUARE,

    // Keywords
    ADO,
    AS,
    CASE,
    CLASS,
    DATA,
    DERIVE,
    DO,
    ELSE,
    FORALL,
    FOREIGN,
    HIDING,
    IF,
    IMPORT,
    IN,
    INFIX,
    INFIXL,
    INFIXR,
    INSTANCE,
    LET,
    MODULE,
    NEWTYPE,
    OF,
    THEN,
    TYPE,
    WHERE,

    // Literals
    CHAR,
    INTEGER,
    NUMBER,
    RAW_STRING,
    STRING,
    TRUE,
    FALSE,

    // Nodes
    Node,
    Comment,
    Module,
    ModuleHeader,
    ModuleName,

    // Control
    LAYOUT_START,
    LAYOUT_SEPARATOR,
    LAYOUT_END,
    ERROR,
    END_OF_FILE,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PureScript {}

impl rowan::Language for PureScript {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::END_OF_FILE as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(value: SyntaxKind) -> Self {
        Self(value as u16)
    }
}

pub type SyntaxNode = rowan::SyntaxNode<PureScript>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<PureScript>;
pub type SyntaxNodePtr = rowan::ast::SyntaxNodePtr<PureScript>;
pub type SyntaxToken = rowan::SyntaxToken<PureScript>;
pub type SyntaxElement = rowan::SyntaxElement<PureScript>;

impl SyntaxKind {
    pub fn is_whitespace_or_comment(&self) -> bool {
        matches!(self, Self::WHITESPACE | Self::LINE_COMMENT | Self::BLOCK_COMMENT)
    }

    pub fn is_whitespace(&self) -> bool {
        matches!(self, Self::WHITESPACE)
    }

    pub fn is_operator(&self) -> bool {
        matches!(self, Self::OPERATOR | Self::MINUS | Self::COLON)
    }

    pub fn is_reserved_operator(&self) -> bool {
        matches!(self, Self::RIGHT_ARROW | Self::RIGHT_THICK_ARROW)
    }

    pub fn is_lower(&self) -> bool {
        matches!(self, Self::LOWER | Self::AS)
    }

    pub fn is_label(&self) -> bool {
        matches!(
            self,
            SyntaxKind::MODULE
                | SyntaxKind::WHERE
                | SyntaxKind::IMPORT
                | SyntaxKind::AS
                | SyntaxKind::ADO
                | SyntaxKind::DO
                | SyntaxKind::IF
                | SyntaxKind::THEN
                | SyntaxKind::ELSE
                | SyntaxKind::LET
                | SyntaxKind::IN
                | SyntaxKind::CASE
                | SyntaxKind::OF
                | SyntaxKind::DATA
                | SyntaxKind::NEWTYPE
                | SyntaxKind::FORALL
                | SyntaxKind::TYPE
                | SyntaxKind::CLASS
                | SyntaxKind::INSTANCE
                | SyntaxKind::DERIVE
                | SyntaxKind::FOREIGN
                | SyntaxKind::INFIXL
                | SyntaxKind::INFIXR
                | SyntaxKind::INFIX
                | SyntaxKind::STRING
                | SyntaxKind::RAW_STRING
                | SyntaxKind::LOWER
        )
    }

    pub fn is_end(&self) -> bool {
        matches!(self, Self::LAYOUT_SEPARATOR | Self::LAYOUT_END | Self::END_OF_FILE)
    }

    pub fn is_layout(&self) -> bool {
        matches!(self, Self::LAYOUT_START | Self::LAYOUT_SEPARATOR | Self::LAYOUT_END)
    }
}
