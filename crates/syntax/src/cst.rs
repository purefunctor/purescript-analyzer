use rowan::ast::AstNode;

#[macro_use]
mod macros;

create_cst_struct!(Module, ModuleHeader, ExportList);

create_cst_enum!(
    ExportItem
        | ExportValue
        | ExportClass
        | ExportType
        | ExportOperator
        | ExportTypeOperator
        | ExportModule
);

create_cst_struct!(ModuleImports, ImportStatement, ImportList, ImportAlias);

create_cst_enum!(
    ImportItem // Comment to fix formatting :P
        | ImportValue
        | ImportClass
        | ImportType
        | ImportOperator
        | ImportTypeOperator
);

create_cst_enum!(TypeItems | TypeItemsAll | TypeItemsList);

create_cst_struct!(ModuleStatements);

create_cst_enum!(
    Declaration
        | ValueSignature
        | ValueEquation
        | InfixDeclaration
        | TypeRoleDeclaration
        | InstanceChain
        | InstanceDeclaration
        | InstanceSignatureStatement
        | InstanceEquationStatement
        | TypeSynonymSignature
        | TypeSynonymEquation
        | ClassSignature
        | ClassDeclaration
        | ClassMemberStatement
        | ForeignImportDataDeclaration
        | ForeignImportValueDeclaration
        | NewtypeSignature
        | NewtypeEquation
        | DataSignature
        | DataEquation
        | DeriveDeclaration
);

associated_declarations!(
    ClassDeclaration where ClassMemberStatement
);

associated_declarations!(
    InstanceDeclaration where InstanceSignatureStatement | InstanceEquationStatement
);

create_cst_struct!(
    TypeRole,
    ClassConstraints,
    ClassHead,
    ClassFunctionalDependencies,
    ClassStatements
);

create_cst_enum!(
    FunctionalDependency | FunctionalDependencyDetermined | FunctionalDependencyDetermines
);

create_cst_struct!(InstanceName, InstanceConstraints, InstanceHead, InstanceStatements);

create_cst_struct!(DataConstructor);

create_cst_enum!(
    Type // Comment to fix formatting :P
        | TypeApplicationChain
        | TypeArrow
        | TypeConstrained
        | TypeConstructor
        | TypeForall
        | TypeHole
        | TypeInteger
        | TypeKinded
        | TypeOperator
        | TypeOperatorChain
        | TypeString
        | TypeVariable
        | TypeVariableBinding
        | TypeWildcard
        | TypeRecord
        | TypeRow
        | TypeParenthesized
);

create_cst_enum!(
    Binder
        | BinderTyped
        | BinderOperatorChain
        | BinderInteger
        | BinderNumber
        | BinderConstructor
        | BinderVariable
        | BinderNamed
        | BinderWildcard
        | BinderString
        | BinderChar
        | BinderTrue
        | BinderFalse
        | BinderArray
        | BinderRecord
        | BinderParenthesized
);

create_cst_struct!(
    FunctionBinders,
    Unconditional,
    WhereExpression,
    Conditionals,
    PatternGuarded,
    PatternGuards
);

create_cst_enum!(PatternGuard | PatternGuardBinder | PatternGuardExpression);

create_cst_enum!(
    Expression
        | ExpressionTyped
        | ExpressionOperatorChain
        | ExpressionInfixChain
        | ExpressionTick
        | ExpressionNegate
        | ExpressionApplicationChain
        | ExpressionTypeArgument
        | ExpressionTermArgument
        | ExpressionIfThenElse
        | ExpressionLetIn
        | ExpressionLambda
        | ExpressionCaseOf
        | ExpressionDo
        | ExpressionAdo
        | ExpressionConstructor
        | ExpressionVariable
        | ExpressionOperatorName
        | ExpressionSection
        | ExpressionHole
        | ExpressionString
        | ExpressionChar
        | ExpressionTrue
        | ExpressionFalse
        | ExpressionInteger
        | ExpressionNumber
        | ExpressionArray
        | ExpressionRecord
        | ExpressionParenthesized
        | ExpressionRecordAccess
        | ExpressionRecordUpdate
);

create_cst_struct!(CaseTrunk, CaseBranches, CaseBranchBinders, CaseBranch);

create_cst_enum!(DoStatement | DoStatementBind | DoStatementLet | DoStatementDiscard);

create_cst_enum!(LetBinding | LetBindingPattern | LetBindingSignature | LetBindingEquation);

create_cst_enum!(RecordItem | RecordField | RecordPun);

create_cst_struct!(RecordUpdates);

create_cst_enum!(RecordUpdate | RecordUpdateLeaf | RecordUpdateBranch);

has_child!(
    Module
    | statements() -> ModuleStatements
);

has_children!(
    ModuleStatements
    | children() -> Declaration
);

has_token!(
    ValueSignature
    | name_token() -> LOWER
);

has_token!(
    ValueEquation
    | name_token() -> LOWER
);

has_children!(
    InstanceChain
    | instance_declarations() -> InstanceDeclaration
);

has_child!(
    InstanceDeclaration
    | instance_head() -> InstanceHead
    | instance_name() -> InstanceName
    | instance_statements() -> InstanceStatements
);

has_token!(
    InstanceName
    | name_token() -> LOWER
);

has_token!(
    InstanceHead
    | type_name_token() -> UPPER
);

has_children!(
    InstanceStatements
    | children() -> InstanceDeclarationStatement
);

has_token!(
    InstanceSignatureStatement
    | name_token() -> LOWER
);

has_token!(
    InstanceEquationStatement
    | name_token() -> LOWER
);

has_token!(
    TypeSynonymSignature
    | name_token() -> UPPER
);

has_token!(
    TypeSynonymEquation
    | name_token() -> UPPER
);

has_token!(
    ClassSignature
    | name_token() -> UPPER
);

has_child!(
    ClassDeclaration
    | class_head() -> ClassHead
    | class_statements() -> ClassStatements
);

has_token!(
    ClassHead
    | name_token() -> UPPER
);

has_children!(
    ClassStatements
    | children() -> ClassDeclarationStatement
);

has_token!(
    ClassMemberStatement
    | name_token() -> LOWER
);
