pub mod ast;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    Whitespace = 0,
    LineComment,
    BlockComment,

    Module,
    ModuleHeader,
    ModuleImports,
    ModuleBody,
    ModuleKw,
    WhereKw,

    ExportList,
    ExportTypeOp,
    ExportClass,
    ExportModule,
    ExportOp,
    ExportValue,
    ExportType,

    ImportList,
    ImportOp,
    ImportType,
    DataAll,
    DataEnumerated,
    ImportTypeOp,
    ImportClass,
    ImportValue,

    ImportDeclaration,
    ImportKw,
    ImportHidden,
    HidingKw,
    ImportQualified,
    AsKw,

    ModuleName,
    QualifiedName,
    QualifiedPrefix,
    Name,
    NameRef,
    Upper,
    Lower,
    Label,
    Hole,
    Operator,

    At,
    Backslash,
    Colon,
    Colon2,
    Comma,
    Equal,
    Minus,
    Period,
    Period2,
    Pipe,
    Question,
    Tick,
    Underscore,

    LeftArrow,
    RightArrow,
    LeftThickArrow,
    RightThickArrow,
    LeftParenthesis,
    RightParenthesis,
    LeftCurly,
    RightCurly,
    LeftSquare,
    RightSquare,

    AdoExpression,
    QualifiedAdo,
    AdoKw,

    DoExpression,
    QualifiedDo,
    // UnqualifiedDo,
    DoKw,

    DoBind,
    DoLetBinding,
    DoDiscard,

    ApplicationExpression,
    TermArgument,
    TypeArgument,

    IfThenElseExpression,
    IfKw,
    ThenKw,
    ElseKw,

    LetInExpression,
    LetKw,
    InKw,
    LetBindingSignature,
    LetBindingName,
    LetBindingPattern,
    WhereExpression,

    CaseKw,
    OfKw,
    CaseBranch,
    CaseExpression,

    ExpressionInfixChain,
    ExpressionOperatorChain,
    NegateExpression,

    ConstructorExpression,
    OperatorNameExpression,
    VariableExpression,

    SectionExpression,
    LambdaExpression,
    LiteralExpression,
    ParenthesizedExpression,
    RecordAccessExpression,
    RecordUpdateExpression,
    RecordUpdateLeaf,
    RecordUpdateBranch,
    TypedExpression,

    LiteralChar,
    LiteralString,
    LiteralRawString,
    LiteralInteger,
    LiteralNumber,
    LiteralTrue,
    LiteralFalse,
    LiteralArray,
    LiteralRecord,
    RecordPun,
    RecordField,

    ForallType,
    TypeVariableKinded,
    TypeVariableName,

    ArrowType,
    ConstrainedType,

    ApplicationType,
    ConstructorType,
    IntegerType,
    KindedType,
    OperatorNameType,
    ParenthesizedType,
    RecordType,
    RowType,
    RowInner,
    RowField,
    RowTail,
    StringType,
    TypeOperatorChain,
    VariableType,
    WildcardType,

    BinderOperatorChain,
    ConstructorBinder,
    LiteralBinder,
    NegativeBinder,
    ParenthesizedBinder,
    TypedBinder,
    VariableBinder,
    WildcardBinder,

    ValueEquationDeclaration,
    ValueAnnotationDeclaration,

    DataAnnotation,
    DataDeclaration,
    DataVariables,
    DataConstructors,
    DataConstructor,
    ConstructorFields,
    DataKw,

    NewtypeDeclarationAnnotation,
    NewtypeDeclaration,
    NewtypeKw,
    ForallKw,

    TypeDeclarationAnnotation,
    TypeDeclarationRole,
    TypeDeclaration,
    TypeKw,
    RoleKw,
    NominalKw,
    RepresentationalKw,
    PhantomKw,

    ClassSignature,
    ClassDeclaration,
    ClassConstraints,
    ClassVariables,
    ClassFundeps,
    FundepDetermined,
    FundepDetermines,
    FundepVariables,
    ClassMembers,
    ClassMember,
    ClassKw,

    InstanceChain,
    InstanceDeclaration,
    InstanceAssertions,
    InstanceMemberSignature,
    InstanceMemberEquation,
    InstanceKw,

    DeriveNewtypeDeclaration,
    DeriveInstanceDeclaration,
    DeriveKw,

    ForeignDataDeclaration,
    ForeignValueDeclaration,
    ForeignKw,

    FixityDeclaration,
    InfixlKw,
    InfixrKw,
    InfixKw,

    UnconditionalBinding,
    GuardedBinding,
    PatternGuard,
    GuardedExpression,

    /// Convenience node for patterns such as `l: e` or `e :: T`.
    Labeled,
    /// Convenience node for patterns such as `@variable` or `?hole`.
    Prefixed,
    /// Convenience node for patterns such as `( element )`.
    Wrapped,
    /// Convenience node for a non-empty array of elements.
    OneOrMore,
    /// Convenience node for an array of elements.
    ZeroOrMore,
    /// Convenience node for a tuple of elements.
    Pair,
    /// Convenience node for patterns such as `a , b , c`
    Separated,

    LayoutStart,
    LayoutSep,
    LayoutEnd,

    Sentinel,
    Source,
    Error,
    EndOfFile,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PureScript {}

impl rowan::Language for PureScript {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::EndOfFile as u16);
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
        matches!(self, Self::Whitespace | Self::LineComment | Self::BlockComment)
    }

    pub fn is_operator(&self) -> bool {
        matches!(self, Self::Operator | Self::Minus | Self::Colon | Self::LeftThickArrow | Self::RightThickArrow)
    }

    pub fn is_reserved_operator(&self) -> bool {
        matches!(self, Self::RightArrow | Self::RightThickArrow)
    }

    pub fn is_lower(&self) -> bool {
        matches!(self, Self::Lower | Self::AsKw)
    }

    pub fn is_label(&self) -> bool {
        matches!(
            self,
            SyntaxKind::ModuleKw
                | SyntaxKind::WhereKw
                | SyntaxKind::ImportKw
                | SyntaxKind::AsKw
                | SyntaxKind::AdoKw
                | SyntaxKind::DoKw
                | SyntaxKind::IfKw
                | SyntaxKind::ThenKw
                | SyntaxKind::ElseKw
                | SyntaxKind::LetKw
                | SyntaxKind::InKw
                | SyntaxKind::CaseKw
                | SyntaxKind::OfKw
                | SyntaxKind::DataKw
                | SyntaxKind::NewtypeKw
                | SyntaxKind::ForallKw
                | SyntaxKind::TypeKw
                | SyntaxKind::ClassKw
                | SyntaxKind::InstanceKw
                | SyntaxKind::DeriveKw
                | SyntaxKind::ForeignKw
                | SyntaxKind::InfixlKw
                | SyntaxKind::InfixrKw
                | SyntaxKind::InfixKw
                | SyntaxKind::LiteralString
                | SyntaxKind::LiteralRawString
                | SyntaxKind::Lower
        )
    }

    pub fn is_end(&self) -> bool {
        matches!(self, Self::LayoutSep | Self::LayoutEnd | Self::EndOfFile)
    }

    pub fn is_layout(&self) -> bool {
        matches!(self, Self::LayoutStart | Self::LayoutSep | Self::LayoutEnd)
    }
}
