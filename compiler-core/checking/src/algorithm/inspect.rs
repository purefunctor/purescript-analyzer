use std::sync::Arc;

use building_types::QueryResult;
use lowering::TypeVariableBindingId;

use crate::ExternalQueries;
use crate::algorithm::kind::synonym;
use crate::algorithm::safety::safe_loop;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::substitute;
use crate::core::{ForallBinder, Type, TypeId, Variable, debruijn};

pub struct InspectSignature {
    pub variables: Vec<ForallBinder>,
    pub function: TypeId,
    pub arguments: Vec<TypeId>,
    pub result: TypeId,
}

pub fn collect_signature_variables<Q>(
    context: &CheckContext<Q>,
    id: lowering::TypeId,
) -> Arc<[TypeVariableBindingId]>
where
    Q: ExternalQueries,
{
    fn aux<Q>(
        context: &CheckContext<Q>,
        id: lowering::TypeId,
        variables: &mut Vec<TypeVariableBindingId>,
    ) where
        Q: ExternalQueries,
    {
        let Some(kind) = context.lowered.info.get_type_kind(id) else {
            return;
        };

        match kind {
            lowering::TypeKind::Forall { bindings, inner } => {
                variables.extend(bindings.iter().map(|binding| binding.id));
                if let Some(inner_id) = inner {
                    aux(context, *inner_id, variables);
                }
            }
            lowering::TypeKind::Parenthesized { parenthesized: Some(parenthesized) } => {
                aux(context, *parenthesized, variables);
            }
            _ => (),
        }
    }

    let mut variables = vec![];
    aux(context, id, &mut variables);
    Arc::from(variables)
}

pub fn inspect_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
    surface_bindings: &[TypeVariableBindingId],
) -> QueryResult<InspectSignature>
where
    Q: ExternalQueries,
{
    // Consider a synonym that hides a quantifier:
    //
    //   type NaturalTransformation f g = forall a. f a -> g a
    //
    //   transform :: forall f g. NaturalTransformation f g
    //
    // With De Bruijn levels, `transform`'s type becomes:
    //
    //   transform :: forall f:0 g:1. NaturalTransformation f g
    //
    // Synonym expansion can reveal additional quantifiers:
    //
    //   transform :: forall f:0 g:1. forall a:2. f a -> g a
    //
    // The following algorithm is designed to handle level
    // adjustments relative to the current scope level.
    let mut surface_bindings = surface_bindings.iter();
    let mut variables = vec![];
    let mut current_id = type_id;

    let mut bindings = substitute::LevelToType::default();

    safe_loop! {
        current_id = synonym::normalize_expand_type(state, context, current_id)?;

        // In the example, after the Forall case has peeled f:0, g:1, and the
        // synonym-expanded a:2, the accumulated bindings are the following:
        //
        //   { 0 -> f', 1 -> g', 2 -> 'a }
        //
        // We're at a monomorphic type at this point, f:0 a:2 -> g:1 a:2, so
        // we can now proceed with applying the substitutions and continuing.
        if !matches!(state.storage[current_id], Type::Forall(..)) && !bindings.is_empty() {
            current_id = substitute::SubstituteBindings::on(state, &bindings, current_id);
            bindings.clear();
            continue;
        }

        match state.storage[current_id] {
            // Bind each ForallBinder relative to the current scope, recording the
            // level substitution for later. In the example, this accumulates the
            // following substitutions before we hit the Function type:
            //
            //   { 0 -> f', 1 -> g', 2 -> 'a }
            //
            Type::Forall(ref binder, inner) => {
                let mut binder = ForallBinder::clone(binder);

                let new_level = if !binder.implicit
                    && let Some(&binding_id) = surface_bindings.next()
                {
                    state.type_scope.bound.bind(debruijn::Variable::Forall(binding_id))
                } else {
                    state.type_scope.bound.bind(debruijn::Variable::Core)
                };

                let old_level = binder.level;
                binder.level = new_level;
                state.type_scope.kinds.insert(new_level, binder.kind);

                let variable = Type::Variable(Variable::Bound(new_level, binder.kind));
                let variable = state.storage.intern(variable);

                bindings.insert(old_level, variable);
                variables.push(binder);
                current_id = inner;
            }

            Type::Constrained(constraint, constrained) => {
                state.push_given(constraint);
                current_id = constrained;
            }

            _ => {
                let (arguments, result) = signature_components(state, context, current_id)?;
                return Ok(InspectSignature { variables, function: current_id, arguments, result });
            }
        }
    }
}

fn signature_components<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
) -> QueryResult<(Vec<TypeId>, TypeId)>
where
    Q: ExternalQueries,
{
    let mut arguments = vec![];
    let mut current_id = type_id;
    let mut bindings = substitute::LevelToType::default();

    safe_loop! {
        current_id = synonym::normalize_expand_type(state, context, current_id)?;

        if !matches!(state.storage[current_id], Type::Forall(..)) && !bindings.is_empty() {
            current_id = substitute::SubstituteBindings::on(state, &bindings, current_id);
            bindings.clear();
            continue;
        }

        match state.storage[current_id] {
            Type::Forall(ref binder, inner) => {
                let old_level = binder.level;
                let kind = binder.kind;

                let new_level = state.type_scope.bound.bind(debruijn::Variable::Core);
                state.type_scope.kinds.insert(new_level, kind);

                let variable = Type::Variable(Variable::Bound(new_level, kind));
                let variable = state.storage.intern(variable);

                bindings.insert(old_level, variable);
                current_id = inner;
            }

            Type::Function(argument_id, return_id) => {
                arguments.push(argument_id);
                current_id = return_id;
            }

            _ => {
                return Ok((arguments, current_id));
            }
        }
    }
}
