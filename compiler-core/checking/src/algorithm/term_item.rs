use building_types::QueryResult;
use indexing::TermItemId;
use lowering::TermItemIr;

use crate::ExternalQueries;
use crate::algorithm::kind::infer_surface_kind;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{inspect, kind, quantify, term, transfer};
use crate::core::{Instance, debruijn};
use crate::error::ErrorStep;

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
                state.surface_bindings.insert_term(item_id, signature_variables);

                let (inferred_type, _) =
                    kind::check_surface_kind(state, context, *signature, context.prim.t)?;

                let quantified_type = quantify::quantify(state, inferred_type)
                    .map(|(quantified_type, _)| quantified_type)
                    .unwrap_or(inferred_type);

                let global_type = transfer::globalize(state, context, quantified_type);
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

pub fn check_instance<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::TermDeclaration(item_id), |state| {
        let Some(TermItemIr::Instance { constraints, resolution, arguments, .. }) =
            context.lowered.info.get_term_item(item_id)
        else {
            return Ok(());
        };

        let Some(resolution) = *resolution else {
            return Ok(());
        };

        // Save the current size of the environment for unbinding.
        let size = state.type_scope.size();

        let mut core_arguments = vec![];
        for argument in arguments.iter() {
            let (inferred_type, inferred_kind) = infer_surface_kind(state, context, *argument)?;

            let inferred_type = transfer::globalize(state, context, inferred_type);
            let inferred_kind = transfer::globalize(state, context, inferred_kind);

            core_arguments.push((inferred_type, inferred_kind));
        }

        let mut core_constraints = vec![];
        for constraint in constraints.iter() {
            let (inferred_type, inferred_kind) = infer_surface_kind(state, context, *constraint)?;

            let inferred_type = transfer::globalize(state, context, inferred_type);
            let inferred_kind = transfer::globalize(state, context, inferred_kind);

            core_constraints.push((inferred_type, inferred_kind));
        }

        state.checked.instances.push(Instance {
            arguments: core_arguments,
            constraints: core_constraints,
            resolution,
        });

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

        state.type_scope.unbind(debruijn::Level(size.0));

        Ok(())
    })
}

pub fn check_value_group<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::TermDeclaration(item_id), |state| {
        let Some(TermItemIr::ValueGroup { signature, equations }) =
            context.lowered.info.get_term_item(item_id)
        else {
            return Ok(());
        };

        if let Some(signature_id) = signature {
            let group_type = term::lookup_file_term(state, context, context.id, item_id)?;

            let surface_bindings = state.surface_bindings.get_term(item_id);
            let surface_bindings = surface_bindings.as_deref().unwrap_or_default();

            let signature =
                inspect::inspect_signature_core(state, context, group_type, surface_bindings)?;

            term::check_equations(state, context, *signature_id, signature, equations)
        } else {
            term::infer_equations(state, context, item_id, equations)
        }
    })
}
