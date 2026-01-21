//! Implements derive for Eq1 and Ord1.
//!
//! Eq1 and Ord1 derivation is simple: it delegates to the base class.
//! The derived implementations are `eq1 = eq` and `compare1 = compare`.
//!
//! For `derive instance Eq1 Identity` to work, there must be an instance
//! available for `Eq (Identity a)`. The derivation algorithm:
//!
//! 1. Creates a fresh skolem `a` and the given constraint `Eq a`
//! 2. Creates a wanted constraint for `Eq (Identity a)`
//! 3. Solves the constraints to determine if it's satisfiable

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;

use crate::ExternalQueries;
use crate::algorithm::derive::{self, tools};
use crate::algorithm::state::{CheckContext, CheckState};
use crate::core::{Type, Variable, debruijn};
use crate::error::ErrorKind;

/// Checks a derive instance for Eq1.
pub fn check_derive_eq1<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(eq) = context.known_types.eq else {
        state.insert_error(ErrorKind::CannotDeriveClass {
            class_file: input.class_file,
            class_id: input.class_id,
        });
        return Ok(());
    };

    check_derive_class1(state, context, input, eq)
}

/// Checks a derive instance for Ord1.
pub fn check_derive_ord1<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(ord) = context.known_types.ord else {
        state.insert_error(ErrorKind::CannotDeriveClass {
            class_file: input.class_file,
            class_id: input.class_id,
        });
        return Ok(());
    };

    check_derive_class1(state, context, input, ord)
}

/// Shared implementation for Eq1 and Ord1 derivation.
fn check_derive_class1<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
    class: (FileId, TypeItemId),
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let [(derived_type, _)] = input.arguments[..] else {
        state.insert_error(ErrorKind::DeriveInvalidArity {
            class_file: input.class_file,
            class_id: input.class_id,
            expected: 1,
            actual: input.arguments.len(),
        });
        return Ok(());
    };

    if derive::extract_type_constructor(state, derived_type).is_none() {
        let type_message = state.render_local_type(context, derived_type);
        state.insert_error(ErrorKind::CannotDeriveForType { type_message });
        return Ok(());
    };

    tools::push_given_constraints(state, &input.constraints);
    tools::emit_superclass_constraints(state, context, &input)?;
    tools::register_derived_instance(state, context, input);

    // Create a fresh skolem for the last type parameter.
    let skolem_level = state.type_scope.size().0;
    let skolem_level = debruijn::Level(skolem_level);

    let skolem_type = Variable::Skolem(skolem_level, context.prim.t);
    let skolem_type = state.storage.intern(Type::Variable(skolem_type));

    // Build the fully-applied type e.g. `Identity` -> `Identity a`
    let applied_type = state.storage.intern(Type::Application(derived_type, skolem_type));

    // Insert the given constraint `Eq a`
    let class_type = state.storage.intern(Type::Constructor(class.0, class.1));
    let given_constraint = state.storage.intern(Type::Application(class_type, skolem_type));
    state.constraints.push_given(given_constraint);

    // Emit the wanted constraint `Eq (Identity a)`
    let wanted_constraint = state.storage.intern(Type::Application(class_type, applied_type));
    state.constraints.push_wanted(wanted_constraint);

    tools::solve_and_report_constraints(state, context)
}
