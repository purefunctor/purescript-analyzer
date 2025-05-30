syntax::create_source! {
    pub struct LoweringSource {
        bd: syntax::cst::Binder as Binder,
        ex: syntax::cst::Expression as Expression,
        ty: syntax::cst::Type as Type,
        tv: syntax::cst::TypeVariableBinding as TypeVariableBinding,
        ds: syntax::cst::DoStatement as DoStatement,
        ls: syntax::cst::LetBindingSignature as LetBindingSignature,
        le: syntax::cst::LetBindingEquation as LetBindingEquation,
        is: syntax::cst::InstanceSignatureStatement as InstanceSignature,
        ie: syntax::cst::InstanceEquationStatement as InstanceEquation,
    }
}
