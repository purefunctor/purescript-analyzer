use std::sync::Arc;

use building_types::QueryResult;
use lowering::TypeVariableBindingId;

use crate::ExternalQueries;
use crate::algorithm::safety::safe_loop;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{normalise, substitute};
use crate::core::{ForallBinder, Type, TypeId, Variable};

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
    // Synonym expansion can reveal additional quantifiers:
    //
    //   transform :: forall f g. forall a. f a -> g a
    //
    // The following algorithm rebinds each quantifier's variable
    // name to a fresh name in the current scope.
    let mut surface_bindings = surface_bindings.iter();
    let mut variables = vec![];
    let mut current_id = type_id;

    let mut bindings = substitute::NameToType::default();

    safe_loop! {
        current_id = normalise::normalise_expand_type(state, context, current_id)?;

        // In the example, after the Forall case has peeled f, g, and the
        // synonym-expanded a, the accumulated bindings are the following:
        //
        //   { old_f -> f', old_g -> g', old_a -> a' }
        //
        // We're at a monomorphic type at this point, so we can now proceed
        // with applying the substitutions and continuing.
        if !matches!(state.storage[current_id], Type::Forall(..)) && !bindings.is_empty() {
            current_id = substitute::SubstituteBindings::on(state, &bindings, current_id);
            bindings.clear();
            continue;
        }

        match state.storage[current_id] {
            // Bind each ForallBinder relative to the current scope, recording
            // the name substitution for later.
            Type::Forall(ref binder, inner) => {
                let mut binder = ForallBinder::clone(binder);

                let old_name = binder.variable.clone();
                let new_name = state.fresh_name(&binder.text);

                if !binder.implicit && let Some(&binding_id) = surface_bindings.next() {
                    state.type_scope.bind_forall(binding_id, binder.kind, new_name.clone());
                } else {
                    state.type_scope.bind_core(binder.kind, new_name.clone());
                }

                binder.variable = new_name.clone();

                // Substitute the binder's kind through existing bindings so that
                // references to earlier forall variables use the fresh Names.
                if !bindings.is_empty() {
                    binder.kind = substitute::SubstituteBindings::on(state, &bindings, binder.kind);
                }

                let variable = Type::Variable(Variable::Bound(new_name, binder.kind));
                let variable = state.storage.intern(variable);

                bindings.insert(old_name, variable);
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
    let mut bindings = substitute::NameToType::default();

    safe_loop! {
        current_id = normalise::normalise_expand_type(state, context, current_id)?;

        if !matches!(state.storage[current_id], Type::Forall(..)) && !bindings.is_empty() {
            current_id = substitute::SubstituteBindings::on(state, &bindings, current_id);
            bindings.clear();
            continue;
        }

        match state.storage[current_id] {
            Type::Forall(ref binder, inner) => {
                let old_name = binder.variable.clone();
                let mut kind = binder.kind;

                let text = binder.text.clone();
                let name = state.fresh_name(&text);

                state.type_scope.bind_core(kind, name.clone());

                if !bindings.is_empty() {
                    kind = substitute::SubstituteBindings::on(state, &bindings, kind);
                }

                let variable = Type::Variable(Variable::Bound(name, kind));
                let variable = state.storage.intern(variable);

                bindings.insert(old_name, variable);
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
