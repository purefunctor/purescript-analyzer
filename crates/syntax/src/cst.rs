use rowan::ast::AstNode;

use crate::{SyntaxKind, SyntaxToken};

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

impl Module {
    pub fn statements(&self) -> Option<ModuleStatements> {
        self.syntax().children().find_map(ModuleStatements::cast)
    }
}

impl ModuleStatements {
    pub fn children(&self) -> impl Iterator<Item = Declaration> {
        self.syntax().children().filter_map(Declaration::cast)
    }
}

impl ValueSignature {
    pub fn name_token(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::LOWER)
    }
}

impl ValueEquation {
    pub fn name_token(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::LOWER)
    }
}

mod support {
    use crate::{SyntaxKind, SyntaxNode, SyntaxToken};

    pub(super) fn token(parent: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
        parent
            .children_with_tokens()
            .filter_map(|element| element.into_token())
            .find(|token| token.kind() == kind)
    }
}
