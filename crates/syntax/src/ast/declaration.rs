use rowan::ast::{support, AstChildren, AstNode};

use super::{
    Binder, Binding, Name, OneOrMore, QualifiedName, Separated, Type, TypeVariableBinding,
    WhereExpression, ZeroOrMore,
};

_create_ast_v!(
    Declaration,
    ClassDeclaration(ClassDeclaration),
    ClassSignature(ClassSignature),
    DataAnnotation(DataAnnotation),
    DataDeclaration(DataDeclaration),
    ForeignDataDeclaration(ForeignDataDeclaration),
    InstanceChain(InstanceChain),
    ValueAnnotationDeclaration(ValueAnnotationDeclaration),
    ValueEquationDeclaration(ValueEquationDeclaration)
);

_create_ast!(ClassConstraints, ClassFundeps, ClassMember, DataConstructor, InstanceDeclaration);

_create_ast_v!(Fundep, Determined(FundepDetermined), Determines(FundepDetermines));

_create_ast_v!(
    InstanceMember,
    InstanceMemberEquation(InstanceMemberEquation),
    InstanceMemberSignature(InstanceMemberSignature)
);

_create_ast_v!(
    LetBinding,
    LetBindingName(LetBindingName),
    LetBindingPattern(LetBindingPattern),
    LetBindingSignature(LetBindingSignature)
);

impl ClassDeclaration {
    pub fn constraints(&self) -> Option<ClassConstraints> {
        ClassConstraints::cast(self.node.first_child()?)
    }

    pub fn name(&self) -> Option<Name> {
        self.node.children().find_map(Name::cast)
    }
    pub fn variables(&self) -> Option<ZeroOrMore<TypeVariableBinding>> {
        self.node.children().find_map(ZeroOrMore::cast)
    }

    pub fn fundeps(&self) -> Option<ClassFundeps> {
        self.node.children().find_map(ClassFundeps::cast)
    }

    pub fn members(&self) -> Option<OneOrMore<ClassMember>> {
        OneOrMore::cast(self.node.last_child()?)
    }
}

impl ClassSignature {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }

    pub fn kind(&self) -> Option<Type> {
        Type::cast(self.node.last_child()?)
    }
}

impl ClassConstraints {
    pub fn children(&self) -> AstChildren<Type> {
        support::children(&self.node)
    }
}

impl ClassFundeps {
    pub fn fundeps(&self) -> Option<Separated<Fundep>> {
        Separated::cast(self.node.last_child()?)
    }
}

impl FundepDetermined {
    pub fn rhs(&self) -> Option<OneOrMore<Name>> {
        OneOrMore::cast(self.node.last_child()?)
    }
}

impl FundepDetermines {
    pub fn lhs(&self) -> Option<OneOrMore<Name>> {
        OneOrMore::cast(self.node.first_child()?)
    }

    pub fn rhs(&self) -> Option<OneOrMore<Name>> {
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

impl InstanceChain {
    pub fn declarations(&self) -> AstChildren<InstanceDeclaration> {
        support::children(&self.node)
    }
}

impl InstanceDeclaration {
    pub fn class_name(&self) -> Option<QualifiedName> {
        self.node.children().find_map(QualifiedName::cast)
    }

    pub fn members(&self) -> Option<OneOrMore<InstanceMember>> {
        OneOrMore::cast(self.node.last_child()?)
    }
}

impl InstanceMemberEquation {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
    }
}

impl InstanceMemberSignature {
    pub fn name(&self) -> Option<Name> {
        Name::cast(self.node.first_child()?)
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
