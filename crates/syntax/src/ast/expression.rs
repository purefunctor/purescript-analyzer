use rowan::ast::AstNode;

use crate::{PureScript, SyntaxToken};

use super::{LetBinding, Name, NameRef, OneOrMore, QualifiedName, Type};

_create_ast_v!(
    Expression,
    AdoExpression(AdoExpression),
    ApplicationExpression(ApplicationExpression),
    CaseExpression(CaseExpression),
    ConstructorExpression(ConstructorExpression),
    DoExpression(DoExpression),
    ExpressionInfixChain(ExpressionInfixChain),
    ExpressionOperatorChain(ExpressionOperatorChain),
    IfThenElseExpression(IfThenElseExpression),
    LetInExpression(LetInExpression),
    LiteralExpression(LiteralExpression),
    OperatorNameExpression(OperatorNameExpression),
    ParenthesizedExpression(ParenthesizedExpression),
    RecordAccessExpression(RecordAccessExpression),
    RecordUpdateExpression(RecordUpdateExpression),
    TypedExpression(TypedExpression),
    VariableExpression(VariableExpression)
);

_create_ast_v!(Argument, TermArgument(TermArgument), TypeArgument(TypeArgument));

_create_ast_v!(RecordItem, RecordField(RecordField), RecordPun(RecordPun));

impl ApplicationExpression {
    pub fn head(&self) -> Option<Expression> {
        Expression::cast(self.node.first_child()?)
    }

    pub fn spine(&self) -> Option<OneOrMore<Argument>> {
        OneOrMore::cast(self.node.last_child()?)
    }
}

impl TermArgument {
    pub fn expression(&self) -> Option<Expression> {
        Expression::cast(self.node.first_child()?)
    }
}

impl TypeArgument {
    pub fn at(&self) -> Option<SyntaxToken> {
        self.node.first_token()
    }

    pub fn ty(&self) -> Option<Type> {
        Type::cast(self.node.first_child()?)
    }
}

impl LetInExpression {
    pub fn bindings(&self) -> Option<OneOrMore<LetBinding>> {
        OneOrMore::cast(self.node.first_child()?)
    }

    pub fn body(&self) -> Option<Expression> {
        Expression::cast(self.node.last_child()?)
    }
}

impl RecordField {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }

    pub fn value<T>(&self) -> Option<T>
    where
        T: AstNode<Language = PureScript>,
    {
        T::cast(self.node.last_child()?)
    }
}

impl RecordPun {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }

    pub fn name_ref(&self) -> Option<NameRef> {
        NameRef::cast(self.node.first_child()?)
    }
}

impl VariableExpression {
    pub fn qualified_name(&self) -> Option<QualifiedName> {
        QualifiedName::cast(self.node.first_child()?)
    }
}
