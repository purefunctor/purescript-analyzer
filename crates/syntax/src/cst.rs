use rowan::ast::AstNode;

#[macro_use]
mod macros;

create_cst_struct!(Annotation, Qualifier, QualifiedName, LabelName);

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
    | upper() -> UPPER
    | lower() -> LOWER
    | operator() -> OPERATOR
    | operator_name() -> OPERATOR_NAME
);

has_child!(
    QualifiedName
    | qualifier() -> Qualifier
);

has_token!(
    LabelName
    | text() -> TEXT
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
        | TypeWildcard
        | TypeRecord
        | TypeRow
        | TypeParenthesized
);

create_cst_struct!(TypeVariableBinding, TypeOperatorPair, TypeRowItem, TypeRowTail);

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

create_cst_struct!(BinderOperatorPair);

create_cst_enum!(GuardedExpression | Unconditional | Conditionals);

create_cst_struct!(FunctionBinders, WhereExpression, PatternGuarded);

create_cst_enum!(PatternGuard | PatternGuardBinder | PatternGuardExpression);

create_cst_enum!(
    Expression
        | ExpressionTyped
        | ExpressionOperatorChain
        | ExpressionInfixChain
        | ExpressionNegate
        | ExpressionApplicationChain
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

create_cst_struct!(
    ExpressionOperatorPair,
    ExpressionInfixPair,
    ExpressionTick,
    ExpressionIf,
    ExpressionThen,
    ExpressionElse,
);

create_cst_enum!(ExpressionArgument | ExpressionTypeArgument | ExpressionTermArgument);

create_cst_struct!(
    LetBindingStatements,
    CaseTrunk,
    CaseBranches,
    CaseBranchBinders,
    CaseBranch,
    DoStatements
);

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
    Unconditional
    | where_expression() -> WhereExpression
);

has_children!(
    Conditionals
    | children() -> PatternGuarded
);

has_child!(
    PatternGuarded
    | where_expression() -> WhereExpression
);

has_children!(
    PatternGuarded
    | children() -> PatternGuard
);

has_child!(
    WhereExpression
    | expression() -> Expression
    | bindings() -> LetBindingStatements
);

has_child!(
    PatternGuardBinder
    | binder() -> Binder
    | expression() -> Expression
);

has_child!(
    PatternGuardExpression
    | expression() -> Expression
);

has_children!(
    InstanceChain
    | instance_declarations() -> InstanceDeclaration
);

has_child!(
    InstanceDeclaration
    | instance_constraints() -> InstanceConstraints
    | instance_head() -> InstanceHead
    | instance_name() -> InstanceName
    | instance_statements() -> InstanceStatements
);

has_children!(
    InstanceConstraints
    | children() -> Type
);

has_token!(
    InstanceName
    | name_token() -> LOWER
);

has_token!(
    InstanceHead
    | type_name_token() -> UPPER
);

has_child!(
    InstanceHead
    | qualifier() -> Qualifier
);

has_children!(
    InstanceHead
    | children() -> Type
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

has_child!(
    ClassMemberStatement
    | signature() -> Type
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

has_children!(
    DataConstructor
    | children() -> Type
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
    | instance_constraints() -> InstanceConstraints
    | instance_head() -> InstanceHead
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
    InfixDeclaration
    | qualified() -> QualifiedName
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

has_child!(
    ExpressionNegate
    | expression() -> Expression
);

has_child!(
    ExpressionApplicationChain
    | expression() -> Expression
);

has_children!(
    ExpressionApplicationChain
    | children() -> ExpressionArgument
);

has_child!(
    ExpressionTypeArgument
    | type_argument() -> Type
);

has_child!(
    ExpressionTermArgument
    | expression() -> Expression
);

has_child!(
    ExpressionIfThenElse
    | r#if() -> ExpressionIf
    | then() -> ExpressionThen
    | r#else() -> ExpressionElse
);

has_child!(
    ExpressionIf
    | expression() -> Expression
);

has_child!(
    ExpressionThen
    | expression() -> Expression
);

has_child!(
    ExpressionElse
    | expression() -> Expression
);

has_child!(
    ExpressionLetIn
    | bindings() -> LetBindingStatements
    | expression() -> Expression
);

has_children!(
    LetBindingStatements
    | children() -> LetBinding
);

has_child!(
    LetBindingPattern
    | binder() -> Binder
    | where_expression() -> WhereExpression
);

has_token!(
    LetBindingSignature
    | name_token() -> LOWER
);

has_child!(
    LetBindingSignature
    | signature() -> Type
);

has_token!(
    LetBindingEquation
    | name_token() -> LOWER
);

has_child!(
    LetBindingEquation
    | function_binders() -> FunctionBinders
    | guarded_expression() -> GuardedExpression
);

has_child!(
    ExpressionLambda
    | function_binders() -> FunctionBinders
    | expression() -> Expression
);

has_child!(
    ExpressionCaseOf
    | trunk() -> CaseTrunk
    | branches() -> CaseBranches
);

has_children!(
    CaseTrunk
    | children() -> Expression
);

has_children!(
    CaseBranches
    | children() -> CaseBranch
);

has_child!(
    CaseBranch
    | binders() -> CaseBranchBinders
    | guarded_expression() -> GuardedExpression
);

has_children!(
    CaseBranchBinders
    | children() -> Binder
);

has_child!(
    ExpressionDo
    | qualifier() -> Qualifier
    | statements() -> DoStatements
);

has_children!(
    DoStatements
    | children() -> DoStatement
);

has_child!(
    DoStatementBind
    | binder() -> Binder
    | expression() -> Expression
);

has_child!(
    DoStatementLet
    | statements() -> LetBindingStatements
);

has_child!(
    DoStatementDiscard
    | expression() -> Expression
);

has_child!(
    ExpressionAdo
    | qualifier() -> Qualifier
    | statements() -> DoStatements
    | expression() -> Expression
);

has_child!(
    ExpressionConstructor
    | name() -> QualifiedName
);

has_child!(
    ExpressionVariable
    | name() -> QualifiedName
);

has_child!(
    ExpressionOperatorName
    | name() -> QualifiedName
);

has_children!(
    ExpressionArray
    | children() -> Expression
);

has_children!(
    ExpressionRecord
    | children() -> RecordItem
);

has_child!(
    RecordField
    | name() -> LabelName
    | expression() -> Expression
    | binder() -> Binder
    | signature() -> Type
);

has_child!(
    RecordPun
    | name() -> LabelName
);

has_child!(
    ExpressionParenthesized
    | expression() -> Expression
);

has_child!(
    ExpressionRecordAccess
    | expression() -> Expression
);

has_children!(
    ExpressionRecordAccess
    | children() -> LabelName
);

has_child!(
    ExpressionRecordUpdate
    | record_updates() -> RecordUpdates
);

has_children!(
    RecordUpdates
    | children() -> RecordUpdate
);

has_child!(
    RecordUpdateLeaf
    | name() -> LabelName
    | expression() -> Expression
);

has_child!(
    RecordUpdateBranch
    | name() -> LabelName
    | record_updates() -> RecordUpdates
);

has_child!(
    BinderTyped
    | binder() -> Binder
    | r#type() -> Type
);

has_child!(
    BinderOperatorChain
    | binder() -> Binder
);

has_children!(
    BinderOperatorChain
    | children() -> BinderOperatorPair
);

has_child!(
    BinderOperatorPair
    | qualified() -> QualifiedName
    | binder() -> Binder
);

has_child!(
    BinderConstructor
    | name() -> QualifiedName
);

has_children!(
    BinderConstructor
    | children() -> Binder
);

has_token!(
    BinderVariable
    | name_token() -> LOWER
);

has_token!(
    BinderNamed
    | name_token() -> LOWER
);

has_child!(
    BinderNamed
    | binder() -> Binder
);

has_children!(
    BinderArray
    | children() -> Binder
);

has_children!(
    BinderRecord
    | children() -> RecordItem
);

has_child!(
    BinderParenthesized
    | binder() -> Binder
);

has_children!(
    TypeApplicationChain
    | children() -> Type
);

has_children!(
    TypeArrow
    | children() -> Type
);

has_children!(
    TypeConstrained
    | children() -> Type
);

has_child!(
    TypeConstructor
    | name() -> QualifiedName
);

has_child!(
    TypeForall
    | r#type() -> Type
);

has_children!(
    TypeForall
    | children() -> TypeVariableBinding
);

has_token!(
    TypeVariableBinding
    | at() -> AT
    | name() -> LOWER
);

has_child!(
    TypeVariableBinding
    | kind() -> Type
);

has_children!(
    TypeKinded
    | children() -> Type
);

has_child!(
    TypeOperator
    | name() -> QualifiedName
);

has_child!(
    TypeOperatorChain
    | r#type() -> Type
);

has_children!(
    TypeOperatorChain
    | children() -> TypeOperatorPair
);

has_child!(
    TypeOperatorPair
    | qualified() -> QualifiedName
    | r#type() -> Type
);

has_token!(
    TypeVariable
    | name_token() -> LOWER
);

has_children!(
    TypeRecord
    | children() -> TypeRowItem
);

has_children!(
    TypeRow
    | children() -> TypeRowItem
);

has_child!(
    TypeRowItem
    | name() -> LabelName
    | r#type() -> Type
);

has_child!(
    TypeRecord
    | tail() -> TypeRowTail
);

has_child!(
    TypeRow
    | tail() -> TypeRowTail
);

has_child!(
    TypeRowTail
    | r#type() -> Type
);

has_child!(
    TypeParenthesized
    | r#type() -> Type
);

has_child!(
    ForeignImportValueDeclaration
    | signature() -> Type
);
