syntax::create_source! {
    pub struct LoweringSource {
        bd: syntax::cst::Binder as Binder,
        ex: syntax::cst::Expression as Expression,
        ty: syntax::cst::Type as Type,
        tv: syntax::cst::TypeVariableBinding as TypeVariableBinding,
        ds: syntax::cst::DoStatement as DoStatement,
        lb: syntax::cst::LetBinding as LetBinding,
    }
}
