//! Offset-independent source mapping.
use syntax::cst;

syntax::create_source! {
    /// Offset-independent indices for lowering.
    ///
    /// See also: [`indexing::IndexingSource`]
    pub struct LoweringSource {
        bd: cst::Binder as Binder,
        ex: cst::Expression as Expression,
        ty: cst::Type as Type,
        tv: cst::TypeVariableBinding as TypeVariableBinding,
        ds: cst::DoStatement as DoStatement,
        ls: cst::LetBindingSignature as LetBindingSignature,
        le: cst::LetBindingEquation as LetBindingEquation,
        is: cst::InstanceSignatureStatement as InstanceSignature,
        ie: cst::InstanceEquationStatement as InstanceEquation,
        qualified_name: syntax::cst::QualifiedName as QualifiedName,
        term_operator: cst::TermOperator as TermOperator,
        type_operator: cst::TypeOperator as TypeOperator,
    }
}
