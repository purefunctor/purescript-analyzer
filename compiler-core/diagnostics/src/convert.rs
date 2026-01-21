use checking::error::{CheckError, ErrorKind};
use indexing::TypeItemKind;
use itertools::Itertools;
use lowering::LoweringError;
use resolving::ResolvingError;
use rowan::ast::AstNode;

use crate::{Diagnostic, DiagnosticsContext, Severity};

pub trait ToDiagnostics {
    fn to_diagnostics(&self, ctx: &DiagnosticsContext<'_>) -> Vec<Diagnostic>;
}

impl ToDiagnostics for LoweringError {
    fn to_diagnostics(&self, ctx: &DiagnosticsContext<'_>) -> Vec<Diagnostic> {
        match self {
            LoweringError::NotInScope(not_in_scope) => {
                let (ptr, name) = match not_in_scope {
                    lowering::NotInScope::ExprConstructor { id } => {
                        (ctx.stabilized.syntax_ptr(*id), None)
                    }
                    lowering::NotInScope::ExprVariable { id } => {
                        (ctx.stabilized.syntax_ptr(*id), None)
                    }
                    lowering::NotInScope::ExprOperatorName { id } => {
                        (ctx.stabilized.syntax_ptr(*id), None)
                    }
                    lowering::NotInScope::TypeConstructor { id } => {
                        (ctx.stabilized.syntax_ptr(*id), None)
                    }
                    lowering::NotInScope::TypeVariable { id } => {
                        (ctx.stabilized.syntax_ptr(*id), None)
                    }
                    lowering::NotInScope::TypeOperatorName { id } => {
                        (ctx.stabilized.syntax_ptr(*id), None)
                    }
                    lowering::NotInScope::NegateFn { id } => {
                        (ctx.stabilized.syntax_ptr(*id), Some("negate"))
                    }
                    lowering::NotInScope::DoFn { kind, id } => (
                        ctx.stabilized.syntax_ptr(*id),
                        match kind {
                            lowering::DoFn::Bind => Some("bind"),
                            lowering::DoFn::Discard => Some("discard"),
                        },
                    ),
                    lowering::NotInScope::AdoFn { kind, id } => (
                        ctx.stabilized.syntax_ptr(*id),
                        match kind {
                            lowering::AdoFn::Map => Some("map"),
                            lowering::AdoFn::Apply => Some("apply"),
                            lowering::AdoFn::Pure => Some("pure"),
                        },
                    ),
                    lowering::NotInScope::TermOperator { id } => {
                        (ctx.stabilized.syntax_ptr(*id), None)
                    }
                    lowering::NotInScope::TypeOperator { id } => {
                        (ctx.stabilized.syntax_ptr(*id), None)
                    }
                };

                let Some(ptr) = ptr else { return vec![] };
                let Some(span) = ctx.span_from_syntax_ptr(&ptr) else { return vec![] };

                let message = if let Some(name) = name {
                    format!("'{name}' is not in scope")
                } else {
                    let text = ctx.text_of(span).trim();
                    format!("'{text}' is not in scope")
                };

                vec![Diagnostic::error("NotInScope", message, span, "lowering")]
            }

            LoweringError::RecursiveSynonym(group) => convert_recursive_group(
                ctx,
                &group.group,
                "RecursiveSynonym",
                "Invalid type synonym cycle",
            ),

            LoweringError::RecursiveKinds(group) => {
                convert_recursive_group(ctx, &group.group, "RecursiveKinds", "Invalid kind cycle")
            }
        }
    }
}

fn convert_recursive_group(
    ctx: &DiagnosticsContext<'_>,
    group: &[indexing::TypeItemId],
    code: &'static str,
    message: &'static str,
) -> Vec<Diagnostic> {
    let spans = group.iter().filter_map(|id| {
        let ptr = match ctx.indexed.items[*id].kind {
            TypeItemKind::Synonym { equation, .. } => ctx.stabilized.syntax_ptr(equation?)?,
            TypeItemKind::Data { equation, .. } => ctx.stabilized.syntax_ptr(equation?)?,
            TypeItemKind::Newtype { equation, .. } => ctx.stabilized.syntax_ptr(equation?)?,
            _ => return None,
        };
        ctx.span_from_syntax_ptr(&ptr)
    });

    let spans = spans.collect_vec();

    let Some(&primary) = spans.first() else { return vec![] };

    let mut diagnostic = Diagnostic::error(code, message, primary, "lowering");

    for &span in &spans[1..] {
        diagnostic = diagnostic.with_related(span, "Includes this type");
    }

    vec![diagnostic]
}

impl ToDiagnostics for ResolvingError {
    fn to_diagnostics(&self, ctx: &DiagnosticsContext<'_>) -> Vec<Diagnostic> {
        match self {
            ResolvingError::TermImportConflict { .. }
            | ResolvingError::TypeImportConflict { .. }
            | ResolvingError::TermExportConflict { .. }
            | ResolvingError::TypeExportConflict { .. }
            | ResolvingError::ExistingTerm { .. }
            | ResolvingError::ExistingType { .. } => {
                vec![]
            }

            ResolvingError::InvalidImportStatement { id } => {
                let Some(ptr) = ctx.stabilized.ast_ptr(*id) else { return vec![] };

                let message = {
                    let cst = ptr.to_node(ctx.root);
                    let name = cst.module_name().map(|cst| {
                        let range = cst.syntax().text_range();
                        ctx.content[range].trim()
                    });
                    let name = name.unwrap_or("<ParseError>");
                    format!("Cannot import module '{name}'")
                };

                let Some(span) = ctx.span_from_ast_ptr(&ptr) else { return vec![] };

                vec![Diagnostic::error("InvalidImportStatement", message, span, "resolving")]
            }

            ResolvingError::InvalidImportItem { id } => {
                let Some(ptr) = ctx.stabilized.syntax_ptr(*id) else { return vec![] };
                let Some(span) = ctx.span_from_syntax_ptr(&ptr) else { return vec![] };

                let text = ctx.text_of(span).trim();
                let message = format!("Cannot import item '{text}'");

                vec![Diagnostic::error("InvalidImportItem", message, span, "resolving")]
            }
        }
    }
}

impl ToDiagnostics for CheckError {
    fn to_diagnostics(&self, context: &DiagnosticsContext<'_>) -> Vec<Diagnostic> {
        let Some(primary) = context.primary_span_from_steps(&self.step) else {
            return vec![];
        };

        let lookup_message = |id| context.checked.error_messages[id].as_str();

        let (severity, code, message) = match &self.kind {
            ErrorKind::AmbiguousConstraint { constraint } => {
                let msg = lookup_message(*constraint);
                (Severity::Error, "AmbiguousConstraint", format!("Ambiguous constraint: {msg}"))
            }
            ErrorKind::CannotDeriveClass { .. } => {
                (Severity::Error, "CannotDeriveClass", "Cannot derive this class".to_string())
            }
            ErrorKind::CannotDeriveForType { type_message } => {
                let msg = lookup_message(*type_message);
                (Severity::Error, "CannotDeriveForType", format!("Cannot derive for type: {msg}"))
            }
            ErrorKind::ContravariantOccurrence { type_message } => {
                let msg = lookup_message(*type_message);
                (
                    Severity::Error,
                    "ContravariantOccurrence",
                    format!("Type variable occurs in contravariant position: {msg}"),
                )
            }
            ErrorKind::CovariantOccurrence { type_message } => {
                let msg = lookup_message(*type_message);
                (
                    Severity::Error,
                    "CovariantOccurrence",
                    format!("Type variable occurs in covariant position: {msg}"),
                )
            }
            ErrorKind::CannotUnify { t1, t2 } => {
                let t1 = lookup_message(*t1);
                let t2 = lookup_message(*t2);
                (Severity::Error, "CannotUnify", format!("Cannot unify '{t1}' with '{t2}'"))
            }
            ErrorKind::DeriveInvalidArity { expected, actual, .. } => (
                Severity::Error,
                "DeriveInvalidArity",
                format!("Invalid arity for derive: expected {expected}, got {actual}"),
            ),
            ErrorKind::DeriveMissingFunctor => (
                Severity::Error,
                "DeriveMissingFunctor",
                "Deriving Functor requires Data.Functor to be in scope".to_string(),
            ),
            ErrorKind::EmptyAdoBlock => {
                (Severity::Error, "EmptyAdoBlock", "Empty ado block".to_string())
            }
            ErrorKind::EmptyDoBlock => {
                (Severity::Error, "EmptyDoBlock", "Empty do block".to_string())
            }
            ErrorKind::InstanceHeadMismatch { expected, actual, .. } => (
                Severity::Error,
                "InstanceHeadMismatch",
                format!("Instance head mismatch: expected {expected} arguments, got {actual}"),
            ),
            ErrorKind::InstanceMemberTypeMismatch { expected, actual } => {
                let expected = lookup_message(*expected);
                let actual = lookup_message(*actual);
                (
                    Severity::Error,
                    "InstanceMemberTypeMismatch",
                    format!("Instance member type mismatch: expected '{expected}', got '{actual}'"),
                )
            }
            ErrorKind::InvalidTypeOperator { kind_message } => {
                let msg = lookup_message(*kind_message);
                (
                    Severity::Error,
                    "InvalidTypeOperator",
                    format!("Invalid type operator kind: {msg}"),
                )
            }
            ErrorKind::ExpectedNewtype { type_message } => {
                let msg = lookup_message(*type_message);
                (Severity::Error, "ExpectedNewtype", format!("Expected a newtype, got: {msg}"))
            }
            ErrorKind::NoInstanceFound { constraint } => {
                let msg = lookup_message(*constraint);
                (Severity::Error, "NoInstanceFound", format!("No instance found for: {msg}"))
            }
            ErrorKind::PartialSynonymApplication { .. } => (
                Severity::Error,
                "PartialSynonymApplication",
                "Partial type synonym application".to_string(),
            ),
            ErrorKind::RecursiveSynonymExpansion { .. } => (
                Severity::Error,
                "RecursiveSynonymExpansion",
                "Recursive type synonym expansion".to_string(),
            ),
            ErrorKind::TooManyBinders { expected, actual, .. } => (
                Severity::Error,
                "TooManyBinders",
                format!("Too many binders: expected {expected}, got {actual}"),
            ),
            ErrorKind::TypeSignatureVariableMismatch { expected, actual, .. } => (
                Severity::Error,
                "TypeSignatureVariableMismatch",
                format!(
                    "Type signature variable mismatch: expected {expected} variables, got {actual}"
                ),
            ),
            ErrorKind::InvalidRoleDeclaration { declared, inferred, .. } => (
                Severity::Error,
                "InvalidRoleDeclaration",
                format!("Invalid role declaration: declared {declared:?}, inferred {inferred:?}"),
            ),
            ErrorKind::CoercibleConstructorNotInScope { .. } => (
                Severity::Error,
                "CoercibleConstructorNotInScope",
                "Constructor not in scope for Coercible".to_string(),
            ),
            ErrorKind::CustomWarning { message_id } => {
                let msg = lookup_message(*message_id);
                (Severity::Warning, "CustomWarning", msg.to_string())
            }
            ErrorKind::CustomFailure { message_id } => {
                let msg = lookup_message(*message_id);
                (Severity::Error, "CustomFailure", msg.to_string())
            }
        };

        vec![Diagnostic {
            severity,
            code: crate::DiagnosticCode::new(code),
            message,
            primary,
            related: vec![],
            source: "checking",
        }]
    }
}
