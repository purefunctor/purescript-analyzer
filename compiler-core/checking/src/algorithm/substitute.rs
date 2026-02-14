use rustc_hash::FxHashMap;

use crate::algorithm::fold::{FoldAction, TypeFold, fold_type};
use crate::algorithm::state::CheckState;
use crate::core::{Name, Type, TypeId, Variable};

pub struct SubstituteBound {
    target: Name,
    replacement: TypeId,
}

impl SubstituteBound {
    /// Substitutes a bound variable with a specific name with a replacement type.
    ///
    /// Since names are globally unique, no scope tracking is needed.
    /// We simply match on the target name directly.
    pub fn on(
        state: &mut CheckState,
        target: Name,
        replacement: TypeId,
        in_type: TypeId,
    ) -> TypeId {
        fold_type(state, in_type, &mut SubstituteBound { target, replacement })
    }
}

impl TypeFold for SubstituteBound {
    fn transform(&mut self, _state: &mut CheckState, id: TypeId, t: &Type) -> FoldAction {
        match t {
            // The forall rebinds the target name, so substitution stops.
            Type::Forall(binder, _) if binder.variable == self.target => FoldAction::Replace(id),
            Type::Variable(Variable::Bound(name, _)) if *name == self.target => {
                FoldAction::Replace(self.replacement)
            }
            _ => FoldAction::Continue,
        }
    }
}

pub type UniToName = FxHashMap<u32, (Name, TypeId)>;

pub struct SubstituteUnification<'a> {
    substitutions: &'a UniToName,
}

impl SubstituteUnification<'_> {
    /// Name-based substitution over a [`Type`].
    ///
    /// Replaces unification variables with bound variables using a name-based
    /// mapping. Since names are globally unique, no scope tracking is needed.
    pub fn on(substitutions: &UniToName, state: &mut CheckState, id: TypeId) -> TypeId {
        fold_type(state, id, &mut SubstituteUnification { substitutions })
    }
}

impl TypeFold for SubstituteUnification<'_> {
    fn transform(&mut self, state: &mut CheckState, id: TypeId, t: &Type) -> FoldAction {
        if let Type::Unification(unification_id) = t {
            if let Some((name, kind)) = self.substitutions.get(unification_id) {
                let (name, kind) = (name.clone(), *kind);
                let kind = SubstituteUnification::on(self.substitutions, state, kind);
                return FoldAction::Replace(
                    state.storage.intern(Type::Variable(Variable::Bound(name, kind))),
                );
            }
            return FoldAction::Replace(id);
        }
        FoldAction::Continue
    }
}

pub type NameToType = FxHashMap<Name, TypeId>;

pub struct SubstituteBindings<'a> {
    bindings: &'a NameToType,
}

impl SubstituteBindings<'_> {
    /// Substitutes bound variables using a name-based mapping.
    ///
    /// This is used to specialise class superclasses with instance arguments.
    /// For example, when deriving `Traversable (Compose f g)`, the superclass
    /// `Functor t` becomes `Functor (Compose f g)` by binding `t`'s name to
    /// `Compose f g`.
    pub fn on(state: &mut CheckState, bindings: &NameToType, id: TypeId) -> TypeId {
        fold_type(state, id, &mut SubstituteBindings { bindings })
    }
}

impl TypeFold for SubstituteBindings<'_> {
    fn transform(&mut self, _state: &mut CheckState, id: TypeId, t: &Type) -> FoldAction {
        match t {
            Type::Variable(Variable::Bound(name, _)) => {
                let id = self.bindings.get(name).copied().unwrap_or(id);
                FoldAction::Replace(id)
            }
            _ => FoldAction::Continue,
        }
    }
}
