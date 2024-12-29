mod token_set;

pub use token_set::TokenSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
#[allow(non_camel_case_types)]
pub enum SyntaxKind {
    // Comments
    WHITESPACE = 0,
    LINE_COMMENT,
    BLOCK_COMMENT,

    // Names
    HOLE,
    PREFIX,
    UPPER,
    LOWER,
    OPERATOR,
    OPERATOR_NAME,

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

    // Control
    LAYOUT_START,
    LAYOUT_SEPARATOR,
    LAYOUT_END,
    END_OF_FILE,

    // Special
    ERROR,

    // Nodes
    Node,
    Comment,

    ModuleName,
    QualifiedName,
    LabelName,

    Module,
    ModuleHeader,
    ModuleExportList,
    ModuleExportValue,
    ModuleExportClass,
    ModuleExportType,
    ModuleExportOperator,
    ModuleExportTypeOperator,
    ModuleExportModule,
    ModuleImports,
    ImportStatement,
    ImportList,
    ImportValue,
    ImportClass,
    ImportType,
    ImportOperator,
    ImportTypeOperator,
    ImportAlias,
    ModuleStatements,

    TypeItemsAll,
    TypeItemsList,

    ValueAnnotation,
    ValueEquation,
    EquationBinders,

    Unconditional,
    WhereExpression,

    Conditionals,
    PatternGuarded,
    PatternGuards,
    PatternGuardBinder,
    PatternGuardExpression,

    LetBindingAnnotation,
    LetBindingEquation,
    LetBindingStatements,

    TypeApplicationChain,
    TypeArrow,
    TypeConstrained,
    TypeConstructor,
    TypeForall,
    TypeHole,
    TypeInteger,
    TypeKinded,
    TypeOperator,
    TypeOperatorChain,
    TypeString,
    TypeVariable,
    TypeVariableBinding,
    TypeWildcard,
    TypeRecord,
    TypeRow,
    TypeRowItem,
    TypeRowTail,
    TypeParenthesized,

    BinderTyped,
    BinderOperatorChain,
    BinderInteger,
    BinderNumber,
    BinderConstructor,
    BinderVariable,
    BinderNamed,
    BinderWildcard,
    BinderString,
    BinderChar,
    BinderTrue,
    BinderFalse,
    BinderArray,
    BinderRecord,
    BinderParenthesized,

    ExpressionTyped,
    ExpressionOperatorChain,
    ExpressionInfixChain,
    ExpressionTick,
    ExpressionNegate,
    ExpressionApplicationChain,
    ExpressionTypeArgument,
    ExpressionTermArgument,
    ExpressionIfThenElse,
    ExpressionLetIn,
    ExpressionLambda,
    ExpressionCaseOf,
    ExpressionDo,
    ExpressionAdo,
    ExpressionConstructor,
    ExpressionVariable,
    ExpressionOperatorName,
    ExpressionSection,
    ExpressionHole,
    ExpressionString,
    ExpressionChar,
    ExpressionTrue,
    ExpressionFalse,
    ExpressionInteger,
    ExpressionNumber,
    ExpressionArray,
    ExpressionRecord,
    ExpressionParenthesized,
    ExpressionRecordAccess,
    ExpressionRecordUpdate,

    CaseTrunk,
    CaseBranches,
    CaseBranchBinders,
    CaseBranch,

    DoStatements,
    DoStatementBind,
    DoStatementLet,
    DoStatementDiscard,

    RecordField,
    RecordPun,
    RecordUpdates,
    RecordUpdateLeaf,
    RecordUpdateBranch,

    #[doc(hidden)]
    __LAST,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PureScript {}

impl rowan::Language for PureScript {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::__LAST as u16);
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

    pub fn is_end(&self) -> bool {
        matches!(self, Self::LAYOUT_SEPARATOR | Self::LAYOUT_END | Self::END_OF_FILE)
    }

    pub fn is_layout(&self) -> bool {
        matches!(self, Self::LAYOUT_START | Self::LAYOUT_SEPARATOR | Self::LAYOUT_END)
    }
}
