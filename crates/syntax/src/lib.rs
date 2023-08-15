pub mod ast;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    Whitespace = 0,
    LineComment,
    BlockComment,

    Module,
    ModuleHeader,
    ModuleKw,
    WhereKw,

    ExportList,
    ImportList,

    ImportDeclaration,
    ImportKw,
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
    LeftBracket,
    RightBracket,
    LeftSquare,
    RightSquare,

    AdoExpression,
    QualifiedAdo,
    AdoKw,

    DoExpression,
    QualifiedDo,
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

    CaseExpression,
    CaseKw,
    OfKw,

    ExpressionInfixChain,
    ExpressionOperatorChain,
    NegateExpression,

    ConstructorExpression,
    OperatorNameExpression,
    VariableExpression,

    LiteralExpression,
    ParenthesizedExpression,
    RecordAccessExpression,
    RecordUpdate,
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
    TypeVariableBinding,

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

    ValueDeclaration,
    AnnotationDeclaration,

    DataDeclaration,
    DataKw,

    NewtypeDeclaration,
    NewtypeKw,
    ForallKw,

    TypeDeclaration,
    TypeKw,

    ClassDeclaration,
    ClassKw,

    InstanceDeclaration,
    InstanceKw,

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
pub type SyntaxToken = rowan::SyntaxToken<PureScript>;
pub type SyntaxElement = rowan::SyntaxElement<PureScript>;

impl SyntaxKind {
    pub fn is_whitespace_or_comment(&self) -> bool {
        matches!(self, Self::Whitespace | Self::LineComment | Self::BlockComment)
    }

    pub fn is_operator(&self) -> bool {
        matches!(self, Self::Operator | Self::Minus | Self::Colon)
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
}

#[cfg(test)]
mod tests {
    use rowan::{ast::AstNode, NodeOrToken};

    use crate::{ast, SyntaxElement, SyntaxKind, SyntaxNode};

    fn print(indent: usize, element: SyntaxElement) {
        let kind: SyntaxKind = element.kind();
        print!("{:indent$}", "", indent = indent);
        match element {
            NodeOrToken::Node(node) => {
                println!("- {:?}", kind);
                for child in node.children_with_tokens() {
                    print(indent + 2, child);
                }
            }
            NodeOrToken::Token(token) => println!("- {:?} {:?}", token.text(), kind),
        }
    }

    #[test]
    fn syntax_playground() {
        let mut builder = rowan::GreenNodeBuilder::default();

        builder.start_node(SyntaxKind::Module.into());
        builder.start_node(SyntaxKind::ModuleHeader.into());
        builder.token(SyntaxKind::ModuleKw.into(), "module");
        builder.token(SyntaxKind::Whitespace.into(), " ");
        builder.start_node(SyntaxKind::ModuleName.into());
        builder.token(SyntaxKind::Upper.into(), "PureScript");
        builder.token(SyntaxKind::Period.into(), ".");
        builder.token(SyntaxKind::Upper.into(), "Main");
        builder.finish_node();
        builder.token(SyntaxKind::Whitespace.into(), " ");
        builder.start_node(SyntaxKind::ExportList.into());
        builder.token(SyntaxKind::LeftParenthesis.into(), "(");
        builder.token(SyntaxKind::RightParenthesis.into(), ")");
        builder.finish_node();
        builder.token(SyntaxKind::Whitespace.into(), " ");
        builder.token(SyntaxKind::WhereKw.into(), "where");
        builder.finish_node();
        builder.finish_node();

        let purescript_module = SyntaxNode::new_root(builder.finish());

        println!("{}", purescript_module);
        print(2, purescript_module.clone().into());

        let module_name = purescript_module
            .children()
            .next()
            .unwrap()
            .children()
            .next()
            .and_then(ast::ModuleName::cast)
            .unwrap();

        let rust_module = SyntaxNode::new_root(
            module_name
                .segments()
                .next()
                .unwrap()
                .replace_with(rowan::GreenToken::new(SyntaxKind::Upper.into(), "Rust")),
        );

        println!("{}", rust_module);
        print(2, rust_module.into());
    }
}
