//! Implements derive for `Data.Generic.Rep.Generic`.
//!
//! Unlike other derivable classes that emit constraints, Generic generates
//! a type for the `rep` wildcard based on the provided type.
//!
//! ```purescript
//! data Either a b = Left a | Right b
//! -- Sum (Constructor "Left" (Argument a)) (Constructor "Right" (Argument b))
//! ```

use building_types::QueryResult;
use files::FileId;
use indexing::{IndexedModule, TermItemId};
use lowering::StringKind;
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::algorithm::derive::{self, tools};
use crate::algorithm::state::{CheckContext, CheckState, KnownGeneric};
use crate::algorithm::{toolkit, transfer, unification};
use crate::core::{Type, TypeId};
use crate::error::ErrorKind;

pub fn check_derive_generic<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let [(derived_type, _), (wildcard_type, _)] = input.arguments[..] else {
        state.insert_error(ErrorKind::DeriveInvalidArity {
            class_file: input.class_file,
            class_id: input.class_id,
            expected: 2,
            actual: input.arguments.len(),
        });
        return Ok(());
    };

    let Some((data_file, data_id)) = derive::extract_type_constructor(state, derived_type) else {
        let global_type = transfer::globalize(state, context, derived_type);
        state.insert_error(ErrorKind::CannotDeriveForType { type_id: global_type });
        return Ok(());
    };

    let Some(ref known_generic) = context.known_generic else {
        state.insert_error(ErrorKind::CannotDeriveClass {
            class_file: input.class_file,
            class_id: input.class_id,
        });
        return Ok(());
    };

    let constructors = tools::lookup_data_constructors(context, data_file, data_id)?;

    let generic_rep =
        build_generic_rep(state, context, known_generic, data_file, derived_type, &constructors)?;

    let _ = unification::unify(state, context, wildcard_type, generic_rep)?;

    tools::push_given_constraints(state, &input.constraints);
    tools::emit_superclass_constraints(state, context, &input)?;
    tools::register_derived_instance(state, context, input);

    Ok(())
}

fn build_generic_rep<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    known_generic: &KnownGeneric,
    data_file: FileId,
    derived_type: TypeId,
    constructors: &[TermItemId],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let [ref rest @ .., last] = constructors[..] else {
        return Ok(known_generic.no_constructors);
    };

    let arguments = toolkit::extract_all_applications(state, derived_type);

    let last =
        build_generic_constructor(state, context, known_generic, data_file, &arguments, last)?;

    rest.iter().rev().try_fold(last, |accumulator, &constructor_id| {
        let constructor = build_generic_constructor(
            state,
            context,
            known_generic,
            data_file,
            &arguments,
            constructor_id,
        )?;
        let applied = state.storage.intern(Type::Application(known_generic.sum, constructor));
        Ok(state.storage.intern(Type::Application(applied, accumulator)))
    })
}

/// Builds a Constructor rep: `Constructor "Name" fields_rep`
fn build_generic_constructor<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    known_generic: &KnownGeneric,
    data_file: FileId,
    arguments: &[TypeId],
    constructor_id: TermItemId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let constructor_name = |indexed: &IndexedModule| {
        const UNKNOWN_NAME: SmolStr = SmolStr::new_static("<Unknown>");
        let constructor_name = indexed.items[constructor_id].name.clone();
        constructor_name.unwrap_or(UNKNOWN_NAME)
    };

    let constructor_name = if data_file == context.id {
        let indexed = &context.indexed;
        constructor_name(indexed)
    } else {
        let indexed = context.queries.indexed(data_file)?;
        constructor_name(&indexed)
    };

    let constructor_type =
        derive::lookup_local_term_type(state, context, data_file, constructor_id)?;

    let field_types = if let Some(constructor_type) = constructor_type {
        derive::instantiate_constructor_fields(state, constructor_type, arguments)
    } else {
        vec![]
    };

    let fields_rep = build_fields_rep(state, known_generic, &field_types);

    let name = state.storage.intern(Type::String(StringKind::String, constructor_name));
    let constructor = state.storage.intern(Type::Application(known_generic.constructor, name));
    Ok(state.storage.intern(Type::Application(constructor, fields_rep)))
}

/// Builds field rep: Product of Arguments, or NoArguments if empty.
fn build_fields_rep(
    state: &mut CheckState,
    known_generic: &KnownGeneric,
    field_types: &[TypeId],
) -> TypeId {
    let [ref rest @ .., last] = field_types[..] else {
        return known_generic.no_arguments;
    };

    let last = state.storage.intern(Type::Application(known_generic.argument, last));
    rest.iter().rev().fold(last, |accumulator, &field| {
        let argument = state.storage.intern(Type::Application(known_generic.argument, field));
        let product = state.storage.intern(Type::Application(known_generic.product, argument));
        state.storage.intern(Type::Application(product, accumulator))
    })
}
