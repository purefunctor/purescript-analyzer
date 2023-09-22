use rowan::ast::AstNode;

use crate::SyntaxToken;

use super::Name;

_create_ast_v!(
    Binder,
    ConstructorBinder(ConstructorBinder),
    LiteralBinder(LiteralBinder),
    NegativeBinder(NegativeBinder),
    ParenthesizedBinder(ParenthesizedBinder),
    TypedBinder(TypedBinder),
    VariableBinder(VariableBinder),
    WildcardBinder(WildcardBinder)
);

impl ParenthesizedBinder {
    pub fn binder(&self) -> Option<Binder> {
        Binder::cast(self.node.first_child()?)
    }
}

impl NegativeBinder {
    pub fn minus(&self) -> Option<SyntaxToken> {
        self.node.first_token()
    }

    pub fn literal(&self) -> Option<LiteralBinder> {
        LiteralBinder::cast(self.node.first_child()?)
    }
}

impl VariableBinder {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }
}
