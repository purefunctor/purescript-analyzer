use rowan::ast::AstNode;

#[macro_use]
mod macros;

create_cst_struct!(Annotation, Qualifier, QualifiedName);

has_token!(
    Annotation
    | text() -> TEXT
);

has_token!(
    Qualifier
    | text() -> TEXT
);

has_token!(
    QualifiedName
    | operator() -> OPERATOR
);

has_child!(
    QualifiedName
    | qualifier() -> Qualifier
);

create_cst_struct!(Module, ModuleHeader, ModuleName, ExportList);

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
        | TypeSynonymSignature
        | TypeSynonymEquation
        | ClassSignature
        | ClassDeclaration
        | ForeignImportDataDeclaration
        | ForeignImportValueDeclaration
        | NewtypeSignature
        | NewtypeEquation
        | DataSignature
        | DataEquation
        | DeriveDeclaration
);

create_cst_struct!(InstanceDeclaration);

create_cst_enum!(InstanceMemberStatement | InstanceSignatureStatement | InstanceEquationStatement);

create_cst_struct!(ClassMemberStatement);

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

create_cst_enum!(GuardedExpression | Unconditional | Conditionals);

create_cst_struct!(FunctionBinders, WhereExpression, PatternGuarded, PatternGuards);

create_cst_enum!(PatternGuard | PatternGuardBinder | PatternGuardExpression);

create_cst_enum!(
    Expression
        | ExpressionTyped
        | ExpressionOperatorChain
        | ExpressionInfixChain
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

create_cst_struct!(ExpressionOperatorPair, ExpressionInfixPair, ExpressionTick);

create_cst_struct!(CaseTrunk, CaseBranches, CaseBranchBinders, CaseBranch);

create_cst_enum!(DoStatement | DoStatementBind | DoStatementLet | DoStatementDiscard);

create_cst_enum!(LetBinding | LetBindingPattern | LetBindingSignature | LetBindingEquation);

create_cst_enum!(RecordItem | RecordField | RecordPun);

create_cst_struct!(RecordUpdates);

create_cst_enum!(RecordUpdate | RecordUpdateLeaf | RecordUpdateBranch);

has_child!(
    Module
    | header() -> ModuleHeader
    | imports() -> ModuleImports
    | statements() -> ModuleStatements
);

has_child!(
    ModuleHeader
    | exports() -> ExportList
);

has_children!(
    ExportList
    | children() -> ExportItem
);

has_token!(
    ExportValue
    | name_token() -> LOWER
);

has_token!(
    ExportClass
    | name_token() -> UPPER
);

has_token!(
    ExportType
    | name_token() -> UPPER
);

has_tokens!(
    TypeItemsList
    | name_tokens() -> UPPER
);

has_child!(
    ExportType
    | type_items() -> TypeItems
);

has_token!(
    ExportOperator
    | name_token() -> OPERATOR_NAME
);

has_token!(
    ExportTypeOperator
    | name_token() -> OPERATOR_NAME
);

has_child!(
    ExportModule
    | module_name() -> ModuleName
);

has_children!(
    ModuleImports
    | children() -> ImportStatement
);

has_child!(
    ImportStatement
    | import_alias() -> ImportAlias
);

has_child!(
    ImportAlias
    | module_name() -> ModuleName
);

has_child!(
    ModuleName
    | qualifier() -> Qualifier
);

has_token!(
    ModuleName
    | name_token() -> UPPER
);

has_children!(
    ModuleStatements
    | children() -> Declaration
);

has_token!(
    ValueSignature
    | name_token() -> LOWER
);

has_child!(
    ValueSignature
    | signature() -> Type
);

has_token!(
    ValueEquation
    | name_token() -> LOWER
);

has_child!(
    ValueEquation
    | function_binders() -> FunctionBinders
    | guarded_expression() -> GuardedExpression
);

has_children!(
    FunctionBinders
    | children() -> Binder
);

has_child!(
    GuardedExpression
    | where_expression() -> WhereExpression
);

has_child!(
    WhereExpression
    | expression() -> Expression
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
    | children() -> InstanceMemberStatement
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
    | children() -> ClassMemberStatement
);

has_token!(
    ClassMemberStatement
    | name_token() -> LOWER
);

has_token!(
    NewtypeSignature
    | name_token() -> UPPER
);

has_token!(
    NewtypeEquation
    | name_token() -> UPPER
);

has_children!(
    NewtypeEquation
    | data_constructors() -> DataConstructor
);

has_token!(
    DataSignature
    | name_token() -> UPPER
);

has_token!(
    DataEquation
    | name_token() -> UPPER
);

has_children!(
    DataEquation
    | data_constructors() -> DataConstructor
);

has_token!(
    DataConstructor
    | name_token() -> UPPER
);

has_token!(
    ForeignImportDataDeclaration
    | name_token() -> UPPER
);

has_token!(
    ForeignImportValueDeclaration
    | name_token() -> LOWER
);

has_child!(
    DeriveDeclaration
    | instance_name() -> InstanceName
);

has_token!(
    TypeRoleDeclaration
    | name_token() -> UPPER
);

has_token!(
    InfixDeclaration
    | type_token() -> TYPE
    | operator_token() -> OPERATOR
);

has_child!(
    ExpressionTyped
    | expression() -> Expression
    | signature() -> Type
);

has_child!(
    ExpressionOperatorChain
    | expression() -> Expression
);

has_children!(
    ExpressionOperatorChain
    | children() -> ExpressionOperatorPair
);

has_child!(
    ExpressionOperatorPair
    | qualified() -> QualifiedName
    | expression() -> Expression
);

has_child!(
    ExpressionInfixChain
    | expression() -> Expression
);

has_children!(
    ExpressionInfixChain
    | children() -> ExpressionInfixPair
);

has_child!(
    ExpressionInfixPair
    | tick() -> ExpressionTick
    | expression() -> Expression
);

has_child!(
    ExpressionTick
    | expression() -> Expression
);
