//! Implements type class deriving for PureScript.

mod contravariant;
mod eq1;
mod foldable;
mod functor;
mod higher_kinded;
mod tools;
mod traversable;
mod variance;

use building_types::QueryResult;
use files::FileId;
use indexing::{DeriveId, TermItemId, TypeItemId};

use crate::ExternalQueries;
use crate::algorithm::safety::safe_loop;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{kind, substitute, term_item, transfer};
use crate::core::{Type, TypeId, Variable, debruijn};
use crate::error::{ErrorKind, ErrorStep};

/// Input fields for [`check_derive`].
pub struct CheckDerive<'a> {
    pub item_id: TermItemId,
    pub derive_id: DeriveId,
    pub constraints: &'a [lowering::TypeId],
    pub arguments: &'a [lowering::TypeId],
    pub class_file: FileId,
    pub class_id: TypeItemId,
    pub is_newtype: bool,
}

/// Checks a derived instance.
pub fn check_derive<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: CheckDerive<'_>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let CheckDerive {
        item_id,
        derive_id,
        constraints,
        arguments,
        class_file,
        class_id,
        is_newtype,
    } = input;

    state.with_error_step(ErrorStep::TermDeclaration(item_id), |state| {
        // Save the current size of the environment for unbinding.
        let size = state.type_scope.size();

        let class_kind = kind::lookup_file_type(state, context, class_file, class_id)?;
        let expected_kinds = term_item::instantiate_class_kind(state, context, class_kind)?;

        if expected_kinds.len() != arguments.len() {
            state.insert_error(ErrorKind::InstanceHeadMismatch {
                class_file,
                class_item: class_id,
                expected: expected_kinds.len(),
                actual: arguments.len(),
            });
        }

        let mut core_arguments = vec![];
        for (argument, expected_kind) in arguments.iter().zip(expected_kinds) {
            let (inferred_type, inferred_kind) =
                kind::check_surface_kind(state, context, *argument, expected_kind)?;
            core_arguments.push((inferred_type, inferred_kind));
        }

        let mut core_constraints = vec![];
        for constraint in constraints.iter() {
            let (inferred_type, inferred_kind) =
                kind::infer_surface_kind(state, context, *constraint)?;
            core_constraints.push((inferred_type, inferred_kind));
        }

        let elaborated = tools::ElaboratedDerive {
            derive_id,
            constraints: core_constraints,
            arguments: core_arguments,
            class_file,
            class_id,
        };

        if is_newtype {
            check_newtype_derive(state, context, elaborated)?;
        } else {
            let class_is = |known| Some((class_file, class_id)) == known;
            let known_types = &context.known_types;

            if class_is(known_types.eq) || class_is(known_types.ord) {
                check_derive_class(state, context, elaborated)?;
            } else if class_is(known_types.functor) {
                functor::check_derive_functor(state, context, elaborated)?;
            } else if class_is(known_types.bifunctor) {
                functor::check_derive_bifunctor(state, context, elaborated)?;
            } else if class_is(known_types.contravariant) {
                contravariant::check_derive_contravariant(state, context, elaborated)?;
            } else if class_is(known_types.profunctor) {
                contravariant::check_derive_profunctor(state, context, elaborated)?;
            } else if class_is(known_types.foldable) {
                foldable::check_derive_foldable(state, context, elaborated)?;
            } else if class_is(known_types.bifoldable) {
                foldable::check_derive_bifoldable(state, context, elaborated)?;
            } else if class_is(known_types.traversable) {
                traversable::check_derive_traversable(state, context, elaborated)?;
            } else if class_is(known_types.bitraversable) {
                traversable::check_derive_bitraversable(state, context, elaborated)?;
            } else if class_is(known_types.eq1) {
                eq1::check_derive_eq1(state, context, elaborated)?;
            } else if class_is(known_types.ord1) {
                eq1::check_derive_ord1(state, context, elaborated)?;
            } else {
                state.insert_error(ErrorKind::CannotDeriveClass { class_file, class_id });
            };
        }

        // Unbind type variables bound during elaboration.
        state.type_scope.unbind(debruijn::Level(size.0));

        Ok(())
    })
}

fn check_derive_class<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
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

    let Some((data_file, data_id)) = extract_type_constructor(state, derived_type) else {
        let global_type = transfer::globalize(state, context, derived_type);
        state.insert_error(ErrorKind::CannotDeriveForType { type_id: global_type });
        return Ok(());
    };

    let class = (input.class_file, input.class_id);
    tools::push_given_constraints(state, &input.constraints);
    tools::emit_superclass_constraints(state, context, &input)?;
    tools::register_derived_instance(state, context, input);

    generate_field_constraints(state, context, data_file, data_id, derived_type, class)?;

    tools::solve_and_report_constraints(state, context)
}

fn check_newtype_derive<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let [ref preceding_arguments @ .., (newtype_type, _)] = input.arguments[..] else {
        return Ok(());
    };

    let insert_error =
        |state: &mut CheckState, context: &CheckContext<Q>, kind: fn(TypeId) -> ErrorKind| {
            let global = transfer::globalize(state, context, newtype_type);
            state.insert_error(kind(global));
        };

    let Some((newtype_file, newtype_id)) = extract_type_constructor(state, newtype_type) else {
        insert_error(state, context, |type_id| ErrorKind::CannotDeriveForType { type_id });
        return Ok(());
    };

    if newtype_file != context.id {
        insert_error(state, context, |type_id| ErrorKind::CannotDeriveForType { type_id });
        return Ok(());
    }

    if !is_newtype(context, newtype_file, newtype_id)? {
        insert_error(state, context, |type_id| ErrorKind::ExpectedNewtype { type_id });
        return Ok(());
    }

    let inner_type = get_newtype_inner(state, context, newtype_file, newtype_id, newtype_type)?;

    // Build `Class t1 t2 Inner` given the constraint `Class t1 t2 Newtype`
    let delegate_constraint = {
        let class_type = state.storage.intern(Type::Constructor(input.class_file, input.class_id));

        let preceding_arguments =
            preceding_arguments.iter().fold(class_type, |function, (argument, _)| {
                state.storage.intern(Type::Application(function, *argument))
            });

        state.storage.intern(Type::Application(preceding_arguments, inner_type))
    };

    tools::push_given_constraints(state, &input.constraints);
    tools::register_derived_instance(state, context, input);

    state.constraints.push_wanted(delegate_constraint);

    tools::solve_and_report_constraints(state, context)
}

pub(crate) fn extract_type_constructor(
    state: &mut CheckState,
    mut type_id: TypeId,
) -> Option<(FileId, TypeItemId)> {
    safe_loop! {
        type_id = state.normalize_type(type_id);
        match state.storage[type_id] {
            Type::Constructor(file, id) => return Some((file, id)),
            Type::Application(function, _) => type_id = function,
            Type::KindApplication(function, _) => type_id = function,
            _ => return None,
        }
    }
}

/// Checks if a type item is a newtype by examining its indexed kind.
fn is_newtype<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let is_newtype = if file_id == context.id {
        matches!(context.indexed.items[type_id].kind, indexing::TypeItemKind::Newtype { .. })
    } else {
        let indexed = context.queries.indexed(file_id)?;
        matches!(indexed.items[type_id].kind, indexing::TypeItemKind::Newtype { .. })
    };
    Ok(is_newtype)
}

/// Extracts type and kind arguments from an application.
///
/// This function returns both type and kind arguments as constructors
/// have both. These are used to instantiate the constructor type with
/// arguments from the instance head.
pub(crate) fn extract_type_arguments(state: &mut CheckState, applied_type: TypeId) -> Vec<TypeId> {
    let mut arguments = vec![];
    let mut current_id = applied_type;

    safe_loop! {
        current_id = state.normalize_type(current_id);
        match state.storage[current_id] {
            Type::Application(function, argument) => {
                arguments.push(argument);
                current_id = function;
            }
            Type::KindApplication(function, argument) => {
                arguments.push(argument);
                current_id = function;
            }
            _ => break,
        }
    }

    arguments.reverse();
    arguments
}

pub(crate) fn lookup_local_term_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    term_id: TermItemId,
) -> QueryResult<Option<TypeId>>
where
    Q: ExternalQueries,
{
    let global_type = if file_id == context.id {
        state.checked.terms.get(&term_id).copied()
    } else {
        let checked = context.queries.checked(file_id)?;
        checked.terms.get(&term_id).copied()
    };
    Ok(global_type.map(|global_type| transfer::localize(state, context, global_type)))
}

/// Gets the inner type for a newtype, specialized with type arguments.
///
/// Newtypes have exactly one constructor with exactly one field.
/// This function extracts that field type, substituting any type parameters.
fn get_newtype_inner<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    newtype_file: FileId,
    newtype_id: TypeItemId,
    newtype_type: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let constructors = tools::lookup_data_constructors(context, newtype_file, newtype_id)?;

    let [constructor_id] = constructors[..] else {
        return Ok(context.prim.unknown);
    };

    let constructor_type = lookup_local_term_type(state, context, newtype_file, constructor_id)?;
    let Some(constructor_type) = constructor_type else {
        return Ok(context.prim.unknown);
    };

    let fields = extract_constructor_fields(state, constructor_type, newtype_type);
    Ok(fields.into_iter().next().unwrap_or(context.prim.unknown))
}

/// Generates constraints for all fields of across all constructors.
///
/// For Eq/Ord, this function uses special handling to emit `Eq1` and `Ord1` for
/// higher-kinded type variables of kind `Type -> Type` that appear applied in
/// the constructor fields.
fn generate_field_constraints<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    data_file: FileId,
    data_id: TypeItemId,
    derived_type: TypeId,
    class: (FileId, TypeItemId),
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let constructors = tools::lookup_data_constructors(context, data_file, data_id)?;

    let class1 = if context.known_types.eq == Some(class) {
        context.known_types.eq1
    } else if context.known_types.ord == Some(class) {
        context.known_types.ord1
    } else {
        None
    };

    for constructor_id in constructors {
        let constructor_type = lookup_local_term_type(state, context, data_file, constructor_id)?;
        let Some(constructor_type) = constructor_type else { continue };

        let field_types = extract_constructor_fields(state, constructor_type, derived_type);
        for field_type in field_types {
            higher_kinded::generate_constraint(state, context, field_type, class, class1);
        }
    }

    Ok(())
}

/// Extracts constructor fields from a constructor.
///
/// This function uses [`extract_type_arguments`] to deconstruct the instance
/// head, then uses [`substitute::SubstituteBound`] to effectively specialise
/// the constructor type for the instance head in particular. Consider the ff:
///
/// ```purescript
/// data Either a b = Left a | Right b
///
/// derive instance Eq (Either Int b)
/// -- Left :: Int -> Either Int b
/// -- Right :: b -> Either Int b
///
/// data Proxy a = Proxy
///
/// derive instance Eq (Proxy @Type Int)
/// -- Proxy :: Proxy @Type Int
/// ```
fn extract_constructor_fields(
    state: &mut CheckState,
    constructor_type: TypeId,
    derived_type: TypeId,
) -> Vec<TypeId> {
    let type_arguments = extract_type_arguments(state, derived_type);
    let mut arguments_iter = type_arguments.into_iter();
    let mut current_id = constructor_type;

    safe_loop! {
        current_id = state.normalize_type(current_id);
        match &state.storage[current_id] {
            Type::Forall(binder, inner) => {
                let binder_level = binder.level;
                let binder_kind = binder.kind;
                let inner = *inner;

                let argument_type = arguments_iter.next().unwrap_or_else(|| {
                    let skolem = Variable::Skolem(binder_level, binder_kind);
                    state.storage.intern(Type::Variable(skolem))
                });

                current_id = substitute::SubstituteBound::on(state, binder_level, argument_type, inner);
            }
            _ => break,
        }
    }

    let mut fields = vec![];

    safe_loop! {
        current_id = state.normalize_type(current_id);
        match state.storage[current_id] {
            Type::Function(argument, result) => {
                fields.push(argument);
                current_id = result;
            }
            _ => break,
        }
    }

    fields
}
