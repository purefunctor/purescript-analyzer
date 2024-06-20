use rowan::ast::{support, AstChildren, AstNode};

use crate::{PureScript, SyntaxToken};

use super::{
    ArgumentList, BinderList, LayoutList, LetBinding, Name, NameRef, OperatorPair, QualifiedName,
    SymbolOperator, TickOperator, Type,
};

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
    LambdaExpression(LambdaExpression),
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

    pub fn spine(&self) -> Option<ArgumentList<Argument>> {
        ArgumentList::cast(self.node.last_child()?)
    }
}

impl ConstructorExpression {
    pub fn qualified_name(&self) -> Option<QualifiedName> {
        QualifiedName::cast(self.node.first_child()?)
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

impl ExpressionInfixChain {
    pub fn head(&self) -> Option<Expression> {
        Expression::cast(self.node.first_child()?)
    }

    pub fn tail(&self) -> AstChildren<OperatorPair<TickOperator<Expression>, Expression>> {
        support::children(&self.node)
    }
}

impl ExpressionOperatorChain {
    pub fn head(&self) -> Option<Expression> {
        Expression::cast(self.node.first_child()?)
    }

    pub fn tail(&self) -> AstChildren<OperatorPair<SymbolOperator, Expression>> {
        support::children(&self.node)
    }
}

impl IfThenElseExpression {
    pub fn condition(&self) -> Option<Expression> {
        Expression::cast(self.node.first_child()?)
    }

    pub fn then(&self) -> Option<Expression> {
        Expression::cast(self.node.children().nth(1)?)
    }

    pub fn unless(&self) -> Option<Expression> {
        Expression::cast(self.node.last_child()?)
    }
}

impl LambdaExpression {
    pub fn binders(&self) -> Option<BinderList> {
        BinderList::cast(self.node.first_child()?)
    }

    pub fn body(&self) -> Option<Expression> {
        Expression::cast(self.node.last_child()?)
    }
}

impl LetInExpression {
    pub fn let_bindings(&self) -> Option<LayoutList<LetBinding>> {
        LayoutList::cast(self.node.first_child()?)
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

impl ParenthesizedExpression {
    pub fn expression(&self) -> Option<Expression> {
        support::child(&self.node)
    }
}

impl VariableExpression {
    pub fn qualified_name(&self) -> Option<QualifiedName> {
        QualifiedName::cast(self.node.first_child()?)
    }
}
