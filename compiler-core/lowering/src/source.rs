use stabilize::AstId;
use syntax::cst;

pub type BinderId = AstId<cst::Binder>;
pub type ExpressionId = AstId<cst::Expression>;
pub type TypeId = AstId<cst::Type>;
pub type TypeVariableBindingId = AstId<cst::TypeVariableBinding>;
pub type DoStatementId = AstId<cst::DoStatement>;
pub type LetBindingSignatureId = AstId<cst::LetBindingSignature>;
pub type LetBindingEquationId = AstId<cst::LetBindingEquation>;
pub type InstanceSignatureId = AstId<cst::InstanceSignatureStatement>;
pub type InstanceEquationId = AstId<cst::InstanceEquationStatement>;
pub type TermOperatorId = AstId<cst::TermOperator>;
pub type TypeOperatorId = AstId<cst::TypeOperator>;
