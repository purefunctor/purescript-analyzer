//! Implements syntax-driven elaboration for declarations.

use building_types::QueryResult;
use indexing::{TermItemId, TypeItemId};
use itertools::Itertools;
use lowering::{
    ClassIr, DataIr, NewtypeIr, SynonymIr, TermItemIr, TypeItemIr, TypeVariableBinding,
};
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::algorithm::kind::infer_surface_kind;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{inspect, kind, term, transfer, unification};
use crate::core::{ForallBinder, Instance, Operator, Synonym, Type, TypeId, Variable, debruijn};
use crate::error::{ErrorKind, ErrorStep};

const MISSING_NAME: SmolStr = SmolStr::new_static("<MissingName>");

pub fn check_type_item<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::TypeDeclaration(item_id), |state| {
        let Some(item) = context.lowered.info.get_type_item(item_id) else {
            return Ok(());
        };

        match item {
            TypeItemIr::DataGroup { signature, data, .. } => {
                let Some(DataIr { variables }) = data else {
                    return Ok(());
                };
                check_data_like(state, context, item_id, *signature, variables)
            }

            TypeItemIr::NewtypeGroup { signature, newtype, .. } => {
                let Some(NewtypeIr { variables }) = newtype else {
                    return Ok(());
                };
                check_data_like(state, context, item_id, *signature, variables)
            }

            TypeItemIr::SynonymGroup { signature, synonym } => {
                let Some(SynonymIr { variables, synonym: Some(synonym) }) = synonym else {
                    return Ok(());
                };
                check_synonym(state, context, item_id, *signature, variables, *synonym)
            }

            TypeItemIr::ClassGroup { signature, class } => {
                let Some(class) = class else {
                    return Ok(());
                };
                check_class(state, context, item_id, *signature, class)
            }

            TypeItemIr::Foreign { .. } => Ok(()),

            TypeItemIr::Operator { associativity, precedence, resolution } => {
                let Some(associativity) = *associativity else { return Ok(()) };
                let Some(precedence) = *precedence else { return Ok(()) };
                let Some((file_id, type_id)) = *resolution else { return Ok(()) };

                let operator = Operator { associativity, precedence, file_id, type_id };
                state.checked.operators.insert(item_id, operator);

                let id = kind::lookup_file_type(state, context, file_id, type_id)?;

                if !is_binary_operator_type(state, id) {
                    state.insert_error(ErrorKind::InvalidTypeOperator { id });
                }

                state.binding_group.types.insert(item_id, id);

                Ok(())
            }
        }
    })
}

pub fn check_type_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::TypeDeclaration(item_id), |state| {
        let Some(item) = context.lowered.info.get_type_item(item_id) else {
            return Ok(());
        };

        match item {
            TypeItemIr::DataGroup { signature, .. }
            | TypeItemIr::NewtypeGroup { signature, .. }
            | TypeItemIr::SynonymGroup { signature, .. }
            | TypeItemIr::ClassGroup { signature, .. }
            | TypeItemIr::Foreign { signature, .. } => {
                let Some(signature) = signature else { return Ok(()) };

                let signature_variables = inspect::collect_signature_variables(context, *signature);
                state.type_signature_variables.insert(item_id, signature_variables);

                let (inferred_type, _) =
                    kind::check_surface_kind(state, context, *signature, context.prim.t)?;
                state.binding_group.types.insert(item_id, inferred_type);
            }

            TypeItemIr::Operator { .. } => {}
        }

        Ok(())
    })
}

struct SignatureLike {
    kind_variables: Vec<ForallBinder>,
    type_variables: Vec<ForallBinder>,
    result_kind: TypeId,
}

fn check_signature_like<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    variables: &[TypeVariableBinding],
    infer_result: impl FnOnce(&mut CheckState) -> TypeId,
) -> QueryResult<Option<SignatureLike>>
where
    Q: ExternalQueries,
{
    let signature = if let Some(signature_id) = signature {
        let stored_kind = kind::lookup_file_type(state, context, context.id, item_id)?;

        let surface_bindings = state.type_signature_variables.get(&item_id).cloned();
        let surface_bindings = surface_bindings.as_deref().unwrap_or_default();

        let signature =
            inspect::inspect_signature_core(state, context, stored_kind, surface_bindings)?;

        if variables.len() != signature.arguments.len() {
            state.insert_error(ErrorKind::TypeSignatureVariableMismatch {
                id: signature_id,
                expected: 0,
                actual: 0,
            });

            if let Some(variable) = signature.variables.first() {
                state.unbind(variable.level);
            }

            return Ok(None);
        };

        let variables = variables.iter();
        let arguments = signature.arguments.iter();

        let kinds = variables
            .zip(arguments)
            .map(|(variable, &argument)| {
                // Use contravariant subtyping for type variables:
                //
                // data Example :: Argument -> Type
                // data Example (a :: Variable) = Example
                //
                // Signature: Argument -> Type
                // Inferred: Variable -> Type
                //
                // Given
                //   Variable -> Type <: Argument -> Type
                //
                // Therefore
                //   [Argument <: Variable, Type <: Type]
                let kind = if let Some(kind_id) = variable.kind {
                    let (kind, _) = kind::infer_surface_kind(state, context, kind_id)?;
                    let valid = unification::subtype(state, context, argument, kind)?;
                    if valid { kind } else { context.prim.unknown }
                } else {
                    argument
                };

                let name = variable.name.clone().unwrap_or(MISSING_NAME);
                Ok((variable.id, variable.visible, name, kind))
            })
            .collect::<QueryResult<Vec<_>>>()?;

        let kind_variables = signature.variables;
        let result_kind = signature.result;
        let type_variables = kinds.into_iter().map(|(id, visible, name, kind)| {
            let level = state.bind_forall(id, kind);
            ForallBinder { visible, name, level, kind }
        });

        let type_variables = type_variables.collect_vec();

        SignatureLike { kind_variables, type_variables, result_kind }
    } else {
        let kind_variables = vec![];
        let result_kind = infer_result(state);
        let type_variables = variables.iter().map(|variable| {
            let kind = if let Some(id) = variable.kind {
                let (kind, _) = kind::check_surface_kind(state, context, id, context.prim.t)?;
                kind
            } else {
                state.fresh_unification_type(context)
            };

            let visible = variable.visible;
            let name = variable.name.clone().unwrap_or(MISSING_NAME);
            let level = state.bind_forall(variable.id, kind);
            Ok(ForallBinder { visible, name, level, kind })
        });

        let type_variables = type_variables.collect::<QueryResult<Vec<_>>>()?;

        SignatureLike { kind_variables, type_variables, result_kind }
    };

    Ok(Some(signature))
}

fn check_data_like<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    variables: &[TypeVariableBinding],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(SignatureLike { kind_variables, type_variables, result_kind }) =
        check_signature_like(state, context, item_id, signature, variables, |_| context.prim.t)?
    else {
        return Ok(());
    };

    let data_reference = {
        let reference_type = state.storage.intern(Type::Constructor(context.id, item_id));
        type_variables.iter().cloned().fold(reference_type, |reference_type, binder| {
            let variable = Variable::Bound(binder.level);
            let variable = state.storage.intern(Type::Variable(variable));

            state.storage.intern(Type::Application(reference_type, variable))
        })
    };

    let type_kind = type_variables.iter().rfold(result_kind, |result, variable| {
        state.storage.intern(Type::Function(variable.kind, result))
    });

    if signature.is_none() {
        unify_pending_kind(state, context, item_id, type_kind)?;
    }

    check_data_constructors(
        state,
        context,
        item_id,
        &kind_variables,
        &type_variables,
        data_reference,
    )?;

    if let Some(variable) = type_variables.first() {
        state.unbind(variable.level);
    }

    if let Some(variable) = kind_variables.first() {
        state.unbind(variable.level);
    }

    Ok(())
}

fn check_synonym<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    variables: &[TypeVariableBinding],
    synonym: lowering::TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(SignatureLike { kind_variables, type_variables, result_kind }) =
        check_signature_like(state, context, item_id, signature, variables, |state| {
            state.fresh_unification_type(context)
        })?
    else {
        return Ok(());
    };

    let (synonym_type, _) = kind::check_surface_kind(state, context, synonym, result_kind)?;

    let type_kind = type_variables.iter().rfold(result_kind, |result, binder| {
        state.storage.intern(Type::Function(binder.kind, result))
    });

    if signature.is_none() {
        unify_pending_kind(state, context, item_id, type_kind)?;
    }

    if let Some(variable) = type_variables.first() {
        state.unbind(variable.level);
    }

    if let Some(variable) = kind_variables.first() {
        state.unbind(variable.level);
    }

    insert_type_synonym(state, item_id, kind_variables, type_variables, synonym_type);

    Ok(())
}

fn check_class<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    ClassIr { constraints, variables, .. }: &ClassIr,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(SignatureLike { kind_variables, type_variables, result_kind }) =
        check_signature_like(state, context, item_id, signature, variables, |_| {
            context.prim.constraint
        })?
    else {
        return Ok(());
    };

    let constraints = constraints.iter().map(|&constraint| {
        let (constraint_type, _) =
            kind::check_surface_kind(state, context, constraint, context.prim.constraint)?;
        Ok(constraint_type)
    });

    let _constraints = constraints.collect::<QueryResult<Vec<_>>>()?;

    let class_reference = {
        let reference_type = state.storage.intern(Type::Constructor(context.id, item_id));
        type_variables.iter().cloned().fold(reference_type, |reference_type, binder| {
            let variable = Variable::Bound(binder.level);
            let variable = state.storage.intern(Type::Variable(variable));

            state.storage.intern(Type::Application(reference_type, variable))
        })
    };

    let class_kind = type_variables.iter().rfold(result_kind, |result, variable| {
        state.storage.intern(Type::Function(variable.kind, result))
    });

    if signature.is_none() {
        unify_pending_kind(state, context, item_id, class_kind)?;
    }

    check_class_members(
        state,
        context,
        item_id,
        &kind_variables,
        &type_variables,
        class_reference,
    )?;

    if let Some(variable) = type_variables.first() {
        state.unbind(variable.level);
    }

    if let Some(variable) = kind_variables.first() {
        state.unbind(variable.level);
    }

    Ok(())
}

fn check_class_members<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    kind_variables: &[ForallBinder],
    type_variables: &[ForallBinder],
    class_reference: TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for member_id in context.indexed.pairs.class_members(item_id) {
        let Some(TermItemIr::ClassMember { signature }) =
            context.lowered.info.get_term_item(member_id)
        else {
            continue;
        };

        let Some(signature_id) = signature else { continue };

        let (member_type, _) =
            kind::check_surface_kind(state, context, *signature_id, context.prim.t)?;

        let (member_foralls, member_inner) = collect_foralls(state, member_type);

        // With levels, no shifting is needed - levels are absolute positions
        let constrained_type =
            state.storage.intern(Type::Constrained(class_reference, member_inner));

        let all_variables = {
            let from_kind = kind_variables.iter().cloned();
            let from_type = type_variables.iter().cloned();
            let from_member = member_foralls.into_iter();
            from_kind.chain(from_type).chain(from_member)
        };

        let member_type = all_variables.rfold(constrained_type, |inner, variable| {
            state.storage.intern(Type::Forall(variable, inner))
        });

        if let Some(pending_type) = state.binding_group.terms.get(&member_id) {
            let _ = unification::unify(state, context, *pending_type, member_type)?;
        } else {
            state.binding_group.terms.insert(member_id, member_type);
        }
    }

    Ok(())
}

fn collect_foralls(state: &CheckState, mut id: TypeId) -> (Vec<ForallBinder>, TypeId) {
    let mut foralls = vec![];

    while let Type::Forall(ref binder, inner) = state.storage[id] {
        foralls.push(binder.clone());
        id = inner;
    }

    (foralls, id)
}

fn is_binary_operator_type(state: &CheckState, mut id: TypeId) -> bool {
    while let Type::Forall(_, inner_id) = state.storage[id] {
        id = inner_id;
    }

    let Type::Function(_, result_id) = state.storage[id] else {
        return false;
    };

    let Type::Function(_, result_id) = state.storage[result_id] else {
        return false;
    };

    !matches!(state.storage[result_id], Type::Function(_, _))
}

fn unify_pending_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    item_kind: TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let pending_kind = state
        .binding_group
        .lookup_type(item_id)
        .expect("invariant violated: invalid binding_group in kind inference");

    let _ = unification::subtype(state, context, item_kind, pending_kind)?;

    Ok(())
}

fn insert_type_synonym(
    state: &mut CheckState,
    item_id: TypeItemId,
    kind_variables: Vec<ForallBinder>,
    type_variables: Vec<ForallBinder>,
    synonym_type: TypeId,
) {
    let synonym_type = type_variables.iter().rfold(synonym_type, |inner, binder| {
        let binder = binder.clone();
        state.storage.intern(Type::Forall(binder, inner))
    });

    let synonym_type = kind_variables.iter().rfold(synonym_type, |inner, binder| {
        let binder = binder.clone();
        state.storage.intern(Type::Forall(binder, inner))
    });

    let quantified_variables = debruijn::Size(0);

    let kind_variables = {
        let length = kind_variables.len();
        debruijn::Size(length as u32)
    };

    let type_variables = {
        let length = type_variables.len();
        debruijn::Size(length as u32)
    };

    let group = Synonym { quantified_variables, kind_variables, type_variables, synonym_type };
    state.binding_group.synonyms.insert(item_id, group);
}

fn check_data_constructors<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    kind_variables: &[ForallBinder],
    type_variables: &[ForallBinder],
    data_reference: TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for item_id in context.indexed.pairs.data_constructors(item_id) {
        let Some(TermItemIr::Constructor { arguments }) =
            context.lowered.info.get_term_item(item_id)
        else {
            continue;
        };

        let arguments = arguments.iter().map(|&argument| {
            state.with_error_step(ErrorStep::ConstructorArgument(argument), |state| {
                let (inferred_type, _) =
                    kind::check_surface_kind(state, context, argument, context.prim.t)?;
                Ok(inferred_type)
            })
        });

        let arguments = arguments.collect::<QueryResult<Vec<_>>>()?;

        let constructor_type = arguments.into_iter().rfold(data_reference, |result, argument| {
            state.storage.intern(Type::Function(argument, result))
        });

        let all_variables = {
            let from_kind = kind_variables.iter();
            let from_type = type_variables.iter();
            from_kind.chain(from_type).cloned()
        };

        let constructor_type = all_variables.rfold(constructor_type, |inner, variable| {
            state.storage.intern(Type::Forall(variable, inner))
        });

        if let Some(pending_type) = state.binding_group.terms.get(&item_id) {
            let _ = unification::unify(state, context, *pending_type, constructor_type)?;
        } else {
            state.binding_group.terms.insert(item_id, constructor_type);
        }
    }

    Ok(())
}

pub fn check_term_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::TermDeclaration(item_id), |state| {
        let Some(item) = context.lowered.info.get_term_item(item_id) else {
            return Ok(());
        };

        match item {
            TermItemIr::Foreign { signature } | TermItemIr::ValueGroup { signature, .. } => {
                let Some(signature) = signature else { return Ok(()) };

                let signature_variables = inspect::collect_signature_variables(context, *signature);
                state.term_signature_variables.insert(item_id, signature_variables);

                let (inferred_type, _) =
                    kind::check_surface_kind(state, context, *signature, context.prim.t)?;
                let global_type = transfer::globalize(state, context, inferred_type);
                state.checked.terms.insert(item_id, global_type);
            }
            TermItemIr::Operator { resolution, .. } => {
                let Some((file_id, term_id)) = *resolution else { return Ok(()) };
                let id = term::lookup_file_term(state, context, file_id, term_id)?;
                let global_type = transfer::globalize(state, context, id);
                state.checked.terms.insert(item_id, global_type);
            }
            _ => (),
        }

        Ok(())
    })
}

pub(crate) fn check_term_item<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::TermDeclaration(item_id), |state| {
        let Some(item) = context.lowered.info.get_term_item(item_id) else {
            return Ok(());
        };
        match item {
            TermItemIr::ValueGroup { signature, equations } => {
                if let Some(signature_id) = signature {
                    let group_type = term::lookup_file_term(state, context, context.id, item_id)?;

                    let forall_bindings = state.term_signature_variables.get(&item_id).cloned();
                    let surface_bindings = forall_bindings.as_deref().unwrap_or_default();

                    let signature = inspect::inspect_signature_core(
                        state,
                        context,
                        group_type,
                        surface_bindings,
                    )?;

                    term::check_equations(state, context, *signature_id, signature, equations)
                } else {
                    term::infer_equations(state, context, item_id, equations)
                }
            }

            TermItemIr::Instance { constraints, arguments, .. } => {
                dbg!(&state.bound);
                dbg!(&state.kinds);

                let size = state.bound.size();
                let level = debruijn::Level(size.0);

                let mut core_arguments = vec![];
                for argument in arguments.iter() {
                    let (inferred_type, inferred_kind) =
                        infer_surface_kind(state, context, *argument)?;
                    println!(
                        "{:?} :: {:?}",
                        &state.storage[inferred_type], &state.storage[inferred_kind],
                    );
                    println!(
                        "{} :: {}",
                        pretty::print_local(state, context, inferred_type),
                        pretty::print_local(state, context, inferred_kind),
                    );
                    core_arguments.push((inferred_type, inferred_kind));
                }

                // This will either resolve the implicit variables or it will
                // bind more implicit variables. TODO: Add test for fundeps
                // etc.
                for constraint in constraints.iter() {
                    let (inferred_type, inferred_kind) =
                        infer_surface_kind(state, context, *constraint)?;
                    println!(
                        "{:?} :: {:?}",
                        &state.storage[inferred_type], &state.storage[inferred_kind],
                    );
                    println!(
                        "{} :: {}",
                        pretty::print_local(state, context, inferred_type),
                        pretty::print_local(state, context, inferred_kind),
                    );
                }

                // Open question:
                //
                // After 'checking' these instances, where do we store them?
                // If we globalize them, we must make sure that no unification
                // variables are left over. That goes for both arguments and
                // the super constraints.
                //
                // On retrieval, we can trust that they have been type checked,
                // but how do we actually use them in the solver? Let's rationalize
                // this, an implicit variable is like a bound variable, except that
                // it's effectful: it binds to the environment such that references
                // to it are valid e.g.
                //
                // Eq a => Eq (Array a)
                //
                // If we have the Wanted constraint, `Eq (Array Int)`, we perform
                // a match over the constraints stored in CheckedModule for the
                // `Eq` class. We find that `Array a` matches, and that the implicit
                // variable `a` can be substituted with `Int`. This discharges the
                // superconstraint `Eq Int`. On the next iteration, `Eq Int` is found
                // by a direct match against `instance eqInt :: Eq Int`
                //
                // Where do the implicit variables matter?
                // - When checking the instance? Yes
                // - When matching instances? Yes
                //      Implicit variable = bind me
                //      Bound variable = look me up
                //
                // In the example above, we know that `a` should be bound to `Int`.
                //
                // Here's another example, during matching:
                //
                // instance TypeEq a a
                //
                // Wanted: TypeEq Int Int
                //
                // The first `a` binds Int, the second `a` resolves to Int. et voila
                //
                // class MonadState m s | m -> s
                //
                // instance MonadState AppM AppState
                //
                // Wanted: `MonadState AppM ?0`
                //
                // How do we know we can match against this instance?
                //
                // m = AppM (Check)
                // s = ?0 (!!!!!)
                //
                // Solver asks: `s` is a unification variable, is it
                // determined by the closure? Where the known variables
                // are `m`.
                //
                // Solver answers: Yes, `s` is determined by `m`. This
                // instance matches now. We can unify all determined
                // type variables with the instance head.
                //
                // Another case to consider:
                //
                // instance MonadState m s => MonadState (LoggingM m) s
                //
                // Wanted: LoggingM AppM ?0
                //
                // We match against this, and we then know that
                //
                // m = AppM
                // s = ?0
                //
                // Per MonadState functional dependencies, this instance
                // matches. NOTE: we cannot backtrack after this, we've
                // already picked this instance. We then discharge more
                //
                // MonadState AppM ?0
                //
                // This can be solved by the previous case.
                //
                // New example, for instances with multiple functional
                // dependencies, we must check against all fundeps.
                //
                // class Add a b c | a b -> c, c a -> b, c b -> a
                //
                // Wanted: Add 123 ?1 123
                // We can make progress, closure is [c, a] -> b
                //
                // Wanted: Add ?1 ?2 c
                // We cannot make progress, closure is [] -> c
                // This is pushed to the back of the queue in hopes that
                // ?1 and ?2 is solved in the future.
                //
                // Matching: instance head matching is a separate
                // algorithm from unification that returns whether
                // or not a Wanted head and a Given head matches.
                //
                // e.g. `Implicit(a) == Int` and binds `a = Int`
                //
                // It also has special rules for functional dependencies,
                // where unification variables can only match on determined
                // positions. That is, `Implicit(c) == ?0`, is `c` determined
                // by a functional dependency? If yes, then Match.
                //
                // Constraint solving also has the notion of 'improvement':
                //
                // Foo a b | a -> b
                //
                // instance Foo a a
                //
                // Wanted: Foo ?0 Int
                //
                // Should we reject matching `Foo a a` immediately because ?0
                // appears in a 'determines' position? Nope! Consider:
                //
                // a := ?0
                // a := Int
                //
                // `a` is bound twice by the matching algorithm! This means that
                //
                //   ?0 := Int
                //
                // This match is improvement, it's basically telling the constraint
                // solver: "I found these types that are supposed to match, unify
                // them for me so I can try and make progress; for now, I'll put
                // this constraint at the back of the queue."
                //
                // On the next iteration of the constraint solver, ?0 would've
                // already been solved to `Int` and we'll see the following:
                //
                // Wanted: Foo Int Int
                //
                //   a := Int
                //   a := Int -- Total match!
                //
                // In constraint solving, there's also the concept of Givens:
                // constraints introduced into the environment from a type
                // signature. These act like constraints that are local to
                // the declaration being checked, for example:
                //
                // showOne :: forall a. Show a => Array a -> String
                // showOne [x] = show x
                // showOne _ = "Too many..."
                //
                // You already know how inference for this goes, `a` is
                // skolemised, it can only ever unify with itself. `x`
                // is bound to the skolem variable `a`. The environment
                // gets a Given constraint, `Show a`. When `show x` is called,
                // the function application discharges a Wanted constraint
                // _asking_ for `Show a`. Since skolem variables can only
                // unify with themselves, the only possible match is the
                // Given `Show a`.
                //
                // Where do we search instances? Per orphan rule, we can
                // only search instances in the file that defines the class
                // or the file that defines a data type.
                //
                // Checking mode should tell us "no instance found for X"
                // Inference mode should quantify the remaining constraints
                // Ambiguous variables are for when there's constraints that
                // contain unification variables that don't appear in the
                // signature, and thus cannot be quantified.
                //
                // Compiler solved instances:
                //
                // The compiler supports compiler-solved instances. Compiler-solved
                // instances have the same improvement capability as regular instances
                // for example, integer addition: `Add 21 21 ?0`
                //
                // This will produce the improvement `?0 = 42`. Any other instances
                // depending on `?0` can now be solved as the constraint solver was
                // able to unify types on behalf of other instances.

                state.unbind(level);

                dbg!(&state.bound);
                dbg!(&state.kinds);

                Ok(())
            }

            _ => Ok(()),
        }
    })
}
