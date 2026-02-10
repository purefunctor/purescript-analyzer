//! Implements type class deriving for PureScript.

mod contravariant;
mod eq1;
mod foldable;
mod functor;
mod generic;
mod higher_kinded;
mod newtype;
mod tools;
mod traversable;
mod variance;

use building_types::QueryResult;
use files::FileId;
use indexing::{DeriveId, TermItemId, TypeItemId};
use itertools::Itertools;

use crate::ExternalQueries;
use crate::algorithm::derive::variance::VarianceConfig;
use crate::algorithm::safety::safe_loop;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{kind, normalise, term_item, toolkit, transfer};
use crate::core::{Type, TypeId, Variable, debruijn};
use crate::error::{ErrorKind, ErrorStep};

/// Input fields for [`check_derive_head`].
pub struct CheckDerive<'a> {
    pub item_id: TermItemId,
    pub derive_id: DeriveId,
    pub constraints: &'a [lowering::TypeId],
    pub arguments: &'a [lowering::TypeId],
    pub class_file: FileId,
    pub class_id: TypeItemId,
    pub is_newtype: bool,
}

/// Determines how [`check_derive_member`] generates constraints.
enum DeriveStrategy {
    /// [`generate_field_constraints`] strategy.
    ///
    /// * Eq
    /// * Ord
    FieldConstraints {
        data_file: FileId,
        data_id: TypeItemId,
        derived_type: TypeId,
        class: (FileId, TypeItemId),
    },
    /// [`generate_variance_constraints`] strategy.
    ///
    /// * Functor, Bifunctor
    /// * Contravariant, Profunctor
    /// * Foldable, Bifoldable
    /// * Traversable, Bitraversable
    ///
    /// [`generate_variance_constraints`]: variance::generate_variance_constraints
    VarianceConstraints {
        data_file: FileId,
        data_id: TypeItemId,
        derived_type: TypeId,
        config: VarianceConfig,
    },
    /// [`generate_delegate_constraint`] strategy.
    ///
    /// * Eq1
    /// * Ord1
    DelegateConstraint { derived_type: TypeId, class: (FileId, TypeItemId) },
    /// `derive newtype instance`
    NewtypeDeriveConstraint { delegate_constraint: TypeId },
    /// The instance head was sufficient.
    ///
    /// * Generic
    /// * Newtype
    HeadOnly,
}

/// Carries state from [`check_derive_head`] to [`check_derive_member`].
pub struct DeriveHeadResult {
    item_id: TermItemId,
    constraints: Vec<TypeId>,
    class_file: FileId,
    class_id: TypeItemId,
    arguments: Vec<(TypeId, TypeId)>,
    strategy: DeriveStrategy,
}

pub fn check_derive_head<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: CheckDerive<'_>,
) -> QueryResult<Option<DeriveHeadResult>>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::TermDeclaration(input.item_id), |state| {
        state.with_implication(|state| check_derive_head_core(state, context, input))
    })
}

fn check_derive_head_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: CheckDerive<'_>,
) -> QueryResult<Option<DeriveHeadResult>>
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
        let (inferred_type, inferred_kind) = kind::infer_surface_kind(state, context, *constraint)?;
        core_constraints.push((inferred_type, inferred_kind));
    }

    let constraints = core_constraints.iter().map(|&(t, _)| t).collect_vec();
    let arguments = core_arguments.clone();

    let elaborated = tools::ElaboratedDerive {
        derive_id,
        constraints: core_constraints,
        arguments: core_arguments,
        class_file,
        class_id,
    };

    let strategy = if is_newtype {
        check_newtype_derive(state, context, elaborated)?
    } else {
        let class_is = |known| Some((class_file, class_id)) == known;
        let known_types = &context.known_types;

        macro_rules! dispatch {
            ($($($known:ident)|+ => $handler:path),+ $(,)?) => {
                $(if $(class_is(known_types.$known))||+ {
                    $handler(state, context, elaborated)?
                } else)+ {
                    state.insert_error(ErrorKind::CannotDeriveClass { class_file, class_id });
                    None
                }
            };
        }

        dispatch! {
            eq | ord => check_derive_class,
            functor => functor::check_derive_functor,
            bifunctor => functor::check_derive_bifunctor,
            contravariant => contravariant::check_derive_contravariant,
            profunctor => contravariant::check_derive_profunctor,
            foldable => foldable::check_derive_foldable,
            bifoldable => foldable::check_derive_bifoldable,
            traversable => traversable::check_derive_traversable,
            bitraversable => traversable::check_derive_bitraversable,
            eq1 => eq1::check_derive_eq1,
            ord1 => eq1::check_derive_ord1,
            newtype => newtype::check_derive_newtype,
            generic => generic::check_derive_generic,
        }
    };

    state.type_scope.unbind(debruijn::Level(size.0));

    Ok(strategy.map(|strategy| DeriveHeadResult {
        item_id,
        constraints,
        class_file,
        class_id,
        arguments,
        strategy,
    }))
}

fn check_derive_class<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
) -> QueryResult<Option<DeriveStrategy>>
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
        return Ok(None);
    };

    let Some((data_file, data_id)) = extract_type_constructor(state, derived_type) else {
        let type_message = state.render_local_type(context, derived_type);
        state.insert_error(ErrorKind::CannotDeriveForType { type_message });
        return Ok(None);
    };

    let class = (input.class_file, input.class_id);
    tools::register_derived_instance(state, context, input)?;

    Ok(Some(DeriveStrategy::FieldConstraints { data_file, data_id, derived_type, class }))
}

fn check_newtype_derive<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
) -> QueryResult<Option<DeriveStrategy>>
where
    Q: ExternalQueries,
{
    let [ref preceding_arguments @ .., (newtype_type, _)] = input.arguments[..] else {
        return Ok(None);
    };

    let Some((newtype_file, newtype_id)) = extract_type_constructor(state, newtype_type) else {
        let type_message = state.render_local_type(context, newtype_type);
        state.insert_error(ErrorKind::CannotDeriveForType { type_message });
        return Ok(None);
    };

    if newtype_file != context.id {
        let type_message = state.render_local_type(context, newtype_type);
        state.insert_error(ErrorKind::CannotDeriveForType { type_message });
        return Ok(None);
    }

    if !is_newtype(context, newtype_file, newtype_id)? {
        let type_message = state.render_local_type(context, newtype_type);
        state.insert_error(ErrorKind::ExpectedNewtype { type_message });
        return Ok(None);
    }

    let (inner_type, skolem_count) =
        get_newtype_inner(state, context, newtype_file, newtype_id, newtype_type)?;

    let inner_type = if skolem_count == 0 {
        inner_type
    } else if let Some(inner_type) =
        try_peel_trailing_skolems(state, context, inner_type, skolem_count)
    {
        inner_type
    } else {
        state.insert_error(ErrorKind::InvalidNewtypeDeriveSkolemArguments);
        return Ok(None);
    };

    // Make sure that the constraint solver sees the synonym expansion.
    let inner_type = normalise::normalise_expand_type(state, context, inner_type)?;

    let delegate_constraint = {
        let class_type = state.storage.intern(Type::Constructor(input.class_file, input.class_id));

        let preceding_arguments =
            preceding_arguments.iter().fold(class_type, |function, (argument, _)| {
                state.storage.intern(Type::Application(function, *argument))
            });

        state.storage.intern(Type::Application(preceding_arguments, inner_type))
    };

    tools::register_derived_instance(state, context, input)?;

    Ok(Some(DeriveStrategy::NewtypeDeriveConstraint { delegate_constraint }))
}

pub fn check_derive_member<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    result: &DeriveHeadResult,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::TermDeclaration(result.item_id), |state| {
        state.with_implication(|state| check_derive_member_core(state, context, result))
    })
}

fn check_derive_member_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    result: &DeriveHeadResult,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for &constraint in &result.constraints {
        state.push_given(constraint);
    }

    match &result.strategy {
        DeriveStrategy::FieldConstraints { data_file, data_id, derived_type, class } => {
            tools::emit_superclass_constraints(
                state,
                context,
                result.class_file,
                result.class_id,
                &result.arguments,
            )?;
            generate_field_constraints(
                state,
                context,
                *data_file,
                *data_id,
                *derived_type,
                *class,
            )?;
        }
        DeriveStrategy::VarianceConstraints { data_file, data_id, derived_type, config } => {
            tools::emit_superclass_constraints(
                state,
                context,
                result.class_file,
                result.class_id,
                &result.arguments,
            )?;
            variance::generate_variance_constraints(
                state,
                context,
                *data_file,
                *data_id,
                *derived_type,
                *config,
            )?;
        }
        DeriveStrategy::DelegateConstraint { derived_type, class } => {
            tools::emit_superclass_constraints(
                state,
                context,
                result.class_file,
                result.class_id,
                &result.arguments,
            )?;
            generate_delegate_constraint(state, context.prim.t, *derived_type, *class);
        }
        DeriveStrategy::NewtypeDeriveConstraint { delegate_constraint } => {
            state.push_wanted(*delegate_constraint);
        }
        DeriveStrategy::HeadOnly => {
            tools::emit_superclass_constraints(
                state,
                context,
                result.class_file,
                result.class_id,
                &result.arguments,
            )?;
        }
    }

    tools::solve_and_report_constraints(state, context)
}

fn generate_delegate_constraint(
    state: &mut CheckState,
    prim_type: TypeId,
    derived_type: TypeId,
    class: (FileId, TypeItemId),
) {
    // Introduce a fresh skolem `~a` for the last type parameter.
    let skolem_type = state.fresh_skolem_kinded(prim_type);

    // Given `Eq ~a`, prove `Eq (Identity ~a)`.
    let applied_type = state.storage.intern(Type::Application(derived_type, skolem_type));

    let class_type = state.storage.intern(Type::Constructor(class.0, class.1));
    let given_constraint = state.storage.intern(Type::Application(class_type, skolem_type));
    state.push_given(given_constraint);

    let wanted_constraint = state.storage.intern(Type::Application(class_type, applied_type));
    state.push_wanted(wanted_constraint);
}

fn try_peel_trailing_skolems<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut type_id: TypeId,
    mut count: usize,
) -> Option<TypeId>
where
    Q: ExternalQueries,
{
    safe_loop! {
        if count == 0 {
            break Some(type_id);
        }
        type_id = state.normalize_type(type_id);
        if let Type::Application(function, argument) | Type::KindApplication(function, argument) =
            state.storage[type_id]
        {
            let argument = state.normalize_type(argument);
            if matches!(state.storage[argument], Type::Variable(Variable::Skolem(_, _))) {
                count -= 1;
                type_id = function;
            } else {
                break None;
            }
        } else if let Type::Function(argument, result) = state.storage[type_id] {
            let result = state.normalize_type(result);
            if matches!(state.storage[result], Type::Variable(Variable::Skolem(_, _))) {
                count -= 1;
                type_id = state.storage.intern(Type::Application(context.prim.function, argument));
            } else {
                break None;
            }
        } else {
            break None;
        }
    }
}

pub fn extract_type_constructor(
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

/// Gets the inner type for a newtype, specialised with type arguments.
///
/// Newtypes have exactly one constructor with exactly one field.
/// This function extracts that field type, substituting any type parameters.
/// If not enough type arguments are supplied, it skolemises the remaining
/// binders and returns the skolem count.
pub fn get_newtype_inner<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    newtype_file: FileId,
    newtype_id: TypeItemId,
    newtype_type: TypeId,
) -> QueryResult<(TypeId, usize)>
where
    Q: ExternalQueries,
{
    let constructors = tools::lookup_data_constructors(context, newtype_file, newtype_id)?;

    let [constructor_id] = constructors[..] else {
        return Ok((context.prim.unknown, 0));
    };

    let constructor_type = lookup_local_term_type(state, context, newtype_file, constructor_id)?;
    let Some(constructor_type) = constructor_type else {
        return Ok((context.prim.unknown, 0));
    };

    let arguments = toolkit::extract_all_applications(state, newtype_type);
    let (fields, skolem_count) =
        instantiate_constructor_fields(state, constructor_type, &arguments);
    Ok((fields.into_iter().next().unwrap_or(context.prim.unknown), skolem_count))
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

    let arguments = toolkit::extract_all_applications(state, derived_type);

    for constructor_id in constructors {
        let constructor_type = lookup_local_term_type(state, context, data_file, constructor_id)?;
        let Some(constructor_type) = constructor_type else { continue };

        let (field_types, _) = instantiate_constructor_fields(state, constructor_type, &arguments);
        for field_type in field_types {
            higher_kinded::generate_constraint(state, context, field_type, class, class1);
        }
    }

    Ok(())
}

/// Instantiates and extracts constructor fields from a constructor type.
///
/// This function uses [`toolkit::instantiate_with_arguments`] to specialise
/// the constructor type with the given type arguments, then extracts the
/// function arguments, returning the fields and the number of skolems that
/// were introduced for the remaining arguments. Consider the ff:
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
///
/// derive instance Eq1 (Vector n)
/// -- Vector :: Vector n ~a
/// -- skolem_count := 1
/// ```
///
/// The `arguments` parameter should be obtained by calling
/// [`toolkit::extract_all_applications`] on the derived type once,
/// then passed to this function for each constructor.
fn instantiate_constructor_fields(
    state: &mut CheckState,
    constructor_type: TypeId,
    arguments: &[TypeId],
) -> (Vec<TypeId>, usize) {
    let (constructor, skolem_count) =
        toolkit::instantiate_with_arguments(state, constructor_type, arguments);
    let (fields, _) = toolkit::extract_function_arguments(state, constructor);
    (fields, skolem_count)
}
