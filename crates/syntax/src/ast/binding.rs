use rowan::ast::AstNode;

use super::OneOrMore;

_create_ast_v!(Binding, UnconditionalBinding(UnconditionalBinding), GuardedBinding(GuardedBinding));

_create_ast!(GuardedExpression, WhereExpression);

impl UnconditionalBinding {
    pub fn where_expression(&self) -> Option<WhereExpression> {
        WhereExpression::cast(self.node.first_child()?)
    }
}

impl GuardedBinding {
    pub fn guarded_expressions(&self) -> Option<OneOrMore<GuardedExpression>> {
        OneOrMore::cast(self.node.first_child()?)
    }
}

impl GuardedExpression {
    pub fn where_expression(&self) -> Option<WhereExpression> {
        WhereExpression::cast(self.node.last_child()?)
    }
}
