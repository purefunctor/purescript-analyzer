#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub enum SyntaxKind {
    // region: whitespace
    WHITESPACE,
    LINE_COMMENT,
    BLOCK_COMMENT,
    // endregion: whitespace

    // region: keywords
    /// _
    UNDERSCORE,
    /// ado
    ADO,
    /// as
    AS,
    /// case
    CASE,
    /// class
    CLASS,
    /// data
    DATA,
    /// derive
    DERIVE,
    /// do
    DO,
    /// else
    ELSE,
    /// false
    FALSE,
    /// foreign
    FOREIGN,
    /// hiding
    HIDING,
    /// if
    IF,
    /// import
    IMPORT,
    /// in
    IN,
    /// infix
    INFIX,
    /// infixl
    INFIXL,
    /// infixr
    INFIXR,
    /// instance
    INSTANCE,
    /// let
    LET,
    /// module
    MODULE,
    /// newtype
    NEWTYPE,
    /// of
    OF,
    /// then
    THEN,
    /// true
    TRUE,
    /// type
    TYPE,
    /// where
    WHERE,
    // endregion: keywords

    // region: names
    /// Module names
    MODULE_NAME,
    /// Named holes
    SOURCE_HOLE,
    /// Lower-case name
    SOURCE_IDENTIFIER,
    /// Upper-case name
    SOURCE_PROPER,
    /// +, -, *, /
    SOURCE_OPERATOR,
    /// (+), (-), (*), (/)
    SOURCE_SYMBOL,
    // endregion: names

    // region: operators
    /// <-, ←
    LEFT_ARROW,
    /// <=, ⇐
    LEFT_DOUBLE_ARROW,
    /// ->, →
    RIGHT_ARROW,
    /// =>, ⇒
    RIGHT_DOUBLE_ARROW,
    /// ::, ∷
    DOUBLE_COLON,
    /// forall, ∀
    FORALL,
    /// =
    EQUALS,
    /// .
    DOT,
    /// ,
    COMMA,
    /// \\
    BACKSLASH,
    /// |
    PIPE,
    /// @
    AT,
    /// `
    TICK,
    // endregion: operators

    // region: delimiters
    /// (
    LEFT_PARENTHESIS,
    /// )
    RIGHT_PARENTHESIS,
    /// [
    LEFT_SQUARE,
    /// ]
    RIGHT_SQUARE,
    /// {
    LEFT_CURLY,
    /// }
    RIGHT_CURLY,
    // endregion: delimiters

    // region: values
    /// ""
    STRING,
    /// """
    RAW_STRING,
    /// 'a', '\n', '\xFFFFFF'
    CHAR,
    /// 0, 0xFF
    INTEGER,
    /// 0.0, 1.23e+5
    NUMBER,
    // endregion: values

    // region: control
    END_OF_FILE,
    LAYOUT_START,
    LAYOUT_DELIMITER,
    LAYOUT_END,
    // endregion: control
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    pub fn is_whitespace_or_comment(self) -> bool {
        matches!(
            self,
            SyntaxKind::WHITESPACE | SyntaxKind::LINE_COMMENT | SyntaxKind::BLOCK_COMMENT
        )
    }
}
