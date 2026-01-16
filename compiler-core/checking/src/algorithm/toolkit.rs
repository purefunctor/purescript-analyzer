use crate::algorithm::safety::safe_loop;
use crate::algorithm::state::CheckState;
use crate::algorithm::substitute;
use crate::core::{Type, TypeId, Variable};

/// Extracts type and kind arguments from a type application.
///
/// Peels off [`Type::Application`] and [`Type::KindApplication`] layers,
/// collecting both type and kind application arguments.
///
/// # Example
///
/// Given `Proxy @Type Int`, returns `[Type, Int]`.
pub fn extract_all_applications(state: &mut CheckState, applied_type: TypeId) -> Vec<TypeId> {
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

/// Decomposes a type application into its head type and arguments.
///
/// Peels off [`Type::Application`] and [`Type::KindApplication`],
/// collecting type application arguments only.
///
/// # Example
///
/// Given `Maybe Int`, returns `(Maybe, [Int])`.
pub fn extract_type_application(
    state: &mut CheckState,
    mut type_id: TypeId,
) -> (TypeId, Vec<TypeId>) {
    let mut arguments = vec![];

    safe_loop! {
        type_id = state.normalize_type(type_id);
        match state.storage[type_id] {
            Type::Application(function, argument) => {
                arguments.push(argument);
                type_id = function;
            }
            Type::KindApplication(function, _) => {
                type_id = function;
            }
            _ => break,
        }
    }

    arguments.reverse();
    (type_id, arguments)
}

/// Extracts function arguments and the return type.
///
/// Peels off `Function` layers, collecting argument types.
///
/// # Example
///
/// Given `Int -> String -> Bool`, returns `([Int, String], Bool)`.
pub fn extract_function_arguments(
    state: &mut CheckState,
    mut type_id: TypeId,
) -> (Vec<TypeId>, TypeId) {
    let mut arguments = vec![];

    safe_loop! {
        type_id = state.normalize_type(type_id);
        match state.storage[type_id] {
            Type::Function(argument, result) => {
                arguments.push(argument);
                type_id = result;
            }
            _ => break,
        }
    }

    (arguments, type_id)
}

/// Instantiates [`Type::Forall`] with fresh unification variables.
pub fn instantiate_forall(state: &mut CheckState, mut type_id: TypeId) -> TypeId {
    safe_loop! {
        type_id = state.normalize_type(type_id);
        if let Type::Forall(ref binder, inner) = state.storage[type_id] {
            let binder_level = binder.level;
            let binder_kind = binder.kind;

            let unification = state.fresh_unification_kinded(binder_kind);
            type_id = substitute::SubstituteBound::on(state, binder_level, unification, inner);
        } else {
            break type_id;
        }
    }
}

/// Instantiates [`Type::Forall`] with the provided arguments.
///
/// This function falls back to constructing skolem variables if there's
/// not enough arguments provided. This is primarily used to specialise
/// constructor types based on the [`Type::Application`] and [`Type::KindApplication`]
/// used in an instance head. For example:
///
/// ```purescript
/// -- Proxy @Type Int
/// Proxy :: forall (k :: Type) (a :: k). Proxy @k a
///
/// -- instantiate_with_arguments(Proxy, [Type, Int])
/// Proxy :: Proxy Type Int
/// ```
pub fn instantiate_with_arguments(
    state: &mut CheckState,
    mut type_id: TypeId,
    arguments: impl IntoIterator<Item = TypeId>,
) -> TypeId {
    let mut arguments_iter = arguments.into_iter();

    safe_loop! {
        type_id = state.normalize_type(type_id);
        match &state.storage[type_id] {
            Type::Forall(binder, inner) => {
                let binder_level = binder.level;
                let binder_kind = binder.kind;
                let inner = *inner;

                let argument_type = arguments_iter.next().unwrap_or_else(|| {
                    let skolem = Variable::Skolem(binder_level, binder_kind);
                    state.storage.intern(Type::Variable(skolem))
                });

                type_id = substitute::SubstituteBound::on(state, binder_level, argument_type, inner);
            }
            _ => break,
        }
    }

    type_id
}
