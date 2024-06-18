use rowan::ast::{support, AstChildren, AstNode};

use super::{Expression, LayoutList, LetBinding};

_create_ast_v!(Binding, UnconditionalBinding(UnconditionalBinding), GuardedBinding(GuardedBinding));

_create_ast!(GuardedExpression, WhereExpression);

impl UnconditionalBinding {
    pub fn where_expression(&self) -> Option<WhereExpression> {
        WhereExpression::cast(self.node.first_child()?)
    }
}

impl GuardedBinding {
    pub fn guarded_expressions(&self) -> AstChildren<GuardedExpression> {
        support::children(&self.node)
    }
}

impl GuardedExpression {
    pub fn where_expression(&self) -> Option<WhereExpression> {
        WhereExpression::cast(self.node.last_child()?)
    }
}

impl WhereExpression {
    pub fn expression(&self) -> Option<Expression> {
        Expression::cast(self.node.first_child()?)
    }

    pub fn let_bindings(&self) -> Option<LayoutList<LetBinding>> {
        LayoutList::cast(self.node.last_child()?)
    }
}
