use rowan::ast::{support, AstChildren, AstNode};

use super::{Binder, Expression, LayoutList, LetBinding};

_create_ast_v!(Binding, UnconditionalBinding(UnconditionalBinding), GuardedBinding(GuardedBinding));

_create_ast!(PatternGuard, PatternGuardList, GuardedExpression, WhereExpression);

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

impl PatternGuard {
    pub fn binder(&self) -> Option<Binder> {
        Binder::cast(self.node.first_child()?)
    }

    pub fn expression(&self) -> Option<Expression> {
        Expression::cast(self.node.last_child()?)
    }
}

impl PatternGuardList {
    pub fn children(&self) -> AstChildren<PatternGuard> {
        support::children(&self.node)
    }
}

impl GuardedExpression {
    pub fn pattern_guard_list(&self) -> Option<PatternGuardList> {
        PatternGuardList::cast(self.node.first_child()?)
    }

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
