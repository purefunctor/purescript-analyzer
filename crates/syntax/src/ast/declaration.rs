use rowan::ast::AstNode;

use super::{
    Binder, Binding, Name, OneOrMore, Separated, Type, TypeVariableBinding, WhereExpression,
    ZeroOrMore,
};

_create_ast_v!(
    Declaration,
    ClassDeclaration(ClassDeclaration),
    ClassSignature(ClassSignature),
    DataAnnotation(DataAnnotation),
    DataDeclaration(DataDeclaration),
    ForeignDataDeclaration(ForeignDataDeclaration),
    ValueAnnotationDeclaration(ValueAnnotationDeclaration),
    ValueEquationDeclaration(ValueEquationDeclaration)
);

_create_ast!(ClassMember);
_create_ast!(DataConstructor);

_create_ast_v!(
    LetBinding,
    LetBindingName(LetBindingName),
    LetBindingPattern(LetBindingPattern),
    LetBindingSignature(LetBindingSignature)
);

impl ClassDeclaration {
    pub fn name(&self) -> Option<Name> {
        self.node.children().find_map(Name::cast)
    }

    pub fn members(&self) -> Option<OneOrMore<ClassMember>> {
        OneOrMore::cast(self.node.last_child()?)
    }
}

impl ClassMember {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }

    pub fn ty(&self) -> Option<Type> {
        Type::cast(self.node.last_child()?)
    }
}

impl ClassSignature {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }
}

impl DataAnnotation {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }

    pub fn kind(&self) -> Option<Type> {
        Type::cast(self.node.last_child()?)
    }
}

impl DataDeclaration {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }

    pub fn variables(&self) -> Option<ZeroOrMore<TypeVariableBinding>> {
        ZeroOrMore::cast(self.node.children().nth(1)?)
    }

    pub fn constructors(&self) -> Option<Separated<DataConstructor>> {
        Separated::cast(self.node.last_child()?)
    }
}

impl DataConstructor {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }

    pub fn fields(&self) -> Option<ZeroOrMore<Type>> {
        ZeroOrMore::cast(self.node.last_child()?)
    }
}

impl ForeignDataDeclaration {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }

    pub fn ty(&self) -> Option<Type> {
        Type::cast(self.node.last_child()?)
    }
}

impl ValueEquationDeclaration {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }

    pub fn binders(&self) -> Option<ZeroOrMore<Binder>> {
        ZeroOrMore::cast(self.node.children().nth(1)?)
    }

    pub fn binding(&self) -> Option<Binding> {
        Binding::cast(self.node.last_child()?)
    }
}

impl ValueAnnotationDeclaration {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }

    pub fn ty(&self) -> Option<Type> {
        Type::cast(self.node.last_child()?)
    }
}

impl LetBindingName {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }

    pub fn binders(&self) -> Option<ZeroOrMore<Binder>> {
        ZeroOrMore::cast(self.node.children().nth(1)?)
    }

    pub fn binding(&self) -> Option<Binding> {
        Binding::cast(self.node.last_child()?)
    }
}

impl LetBindingPattern {
    pub fn binder(&self) -> Option<Binder> {
        Binder::cast(self.node.first_child()?)
    }

    pub fn where_expr(&self) -> Option<WhereExpression> {
        WhereExpression::cast(self.node.last_child()?)
    }
}

impl LetBindingSignature {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }

    pub fn ty(&self) -> Option<Type> {
        Type::cast(self.node.last_child()?)
    }
}
