use rustc_hash::FxHashMap;

use crate::algorithm::fold::{FoldAction, TypeFold, fold_type};
use crate::algorithm::state::CheckState;
use crate::core::{ForallBinder, Type, TypeId, Variable, debruijn};

pub struct SubstituteBound {
    target_level: debruijn::Level,
    with_type: TypeId,
}

impl SubstituteBound {
    /// Substitutes a bound variable at a specific level with a replacement type.
    ///
    /// Since levels are absolute positions, no scope tracking is needed,
    /// we simply match on the target level directly.
    pub fn on(
        state: &mut CheckState,
        target_level: debruijn::Level,
        with_type: TypeId,
        in_type: TypeId,
    ) -> TypeId {
        fold_type(state, in_type, &mut SubstituteBound { target_level, with_type })
    }
}

impl TypeFold for SubstituteBound {
    fn transform(&mut self, _state: &mut CheckState, _id: TypeId, t: &Type) -> FoldAction {
        if let Type::Variable(Variable::Bound(level)) = t
            && *level == self.target_level
        {
            return FoldAction::Replace(self.with_type);
        }
        FoldAction::Continue
    }
}

pub struct ShiftBound {
    offset: u32,
}

impl ShiftBound {
    /// Shifts all bound variable levels in a type by a given offset.
    ///
    /// This is needed when adding new forall binders at the front of a type,
    /// as existing bound variables need their levels adjusted to account for
    /// the new binders.
    pub fn on(state: &mut CheckState, id: TypeId, offset: u32) -> TypeId {
        if offset == 0 {
            return id;
        }
        fold_type(state, id, &mut ShiftBound { offset })
    }
}

impl TypeFold for ShiftBound {
    fn transform(&mut self, state: &mut CheckState, _id: TypeId, t: &Type) -> FoldAction {
        if let Type::Variable(Variable::Bound(level)) = t {
            let level = debruijn::Level(level.0 + self.offset);
            FoldAction::Replace(state.storage.intern(Type::Variable(Variable::Bound(level))))
        } else {
            FoldAction::Continue
        }
    }

    fn transform_binder(&mut self, binder: &mut ForallBinder) {
        binder.level = debruijn::Level(binder.level.0 + self.offset);
    }
}

pub struct ShiftImplicit {
    offset: u32,
}

impl ShiftImplicit {
    /// Shifts all bound AND implicit variable levels in a type by a given offset.
    ///
    /// This is needed when adding new kind binders to instance heads, where
    /// implicit variables also need their levels adjusted.
    pub fn on(state: &mut CheckState, id: TypeId, offset: u32) -> TypeId {
        if offset == 0 {
            return id;
        }
        fold_type(state, id, &mut ShiftImplicit { offset })
    }
}

impl TypeFold for ShiftImplicit {
    fn transform(&mut self, state: &mut CheckState, _id: TypeId, t: &Type) -> FoldAction {
        match t {
            Type::Variable(Variable::Bound(level)) => {
                let level = debruijn::Level(level.0 + self.offset);
                FoldAction::Replace(state.storage.intern(Type::Variable(Variable::Bound(level))))
            }
            _ => FoldAction::Continue,
        }
    }

    fn transform_binder(&mut self, binder: &mut ForallBinder) {
        binder.level = debruijn::Level(binder.level.0 + self.offset);
    }
}

pub type UniToLevel = FxHashMap<u32, debruijn::Level>;

pub struct SubstituteUnification<'a> {
    substitutions: &'a UniToLevel,
}

impl SubstituteUnification<'_> {
    /// Level-based substitution over a [`Type`].
    ///
    /// Replaces unification variables with bound variables using a level-based
    /// mapping. Since levels are absolute positions, no scope tracking is needed.
    pub fn on(substitutions: &UniToLevel, state: &mut CheckState, id: TypeId) -> TypeId {
        fold_type(state, id, &mut SubstituteUnification { substitutions })
    }
}

impl TypeFold for SubstituteUnification<'_> {
    fn transform(&mut self, state: &mut CheckState, id: TypeId, t: &Type) -> FoldAction {
        if let Type::Unification(unification_id) = t {
            if let Some(&level) = self.substitutions.get(unification_id) {
                return FoldAction::Replace(
                    state.storage.intern(Type::Variable(Variable::Bound(level))),
                );
            }
            return FoldAction::Replace(id);
        }
        FoldAction::Continue
    }
}

pub type LevelToType = FxHashMap<debruijn::Level, TypeId>;

pub struct SubstituteBindings<'a> {
    bindings: &'a LevelToType,
}

impl SubstituteBindings<'_> {
    /// Substitutes bound and implicit variables using a level-based mapping.
    ///
    /// This is used to specialize class superclasses with instance arguments.
    /// For example, when deriving `Traversable (Compose f g)`, the superclass
    /// `Functor t` becomes `Functor (Compose f g)` by binding `t`'s level to
    /// `Compose f g`.
    pub fn on(state: &mut CheckState, bindings: &LevelToType, id: TypeId) -> TypeId {
        fold_type(state, id, &mut SubstituteBindings { bindings })
    }
}

impl TypeFold for SubstituteBindings<'_> {
    fn transform(&mut self, _state: &mut CheckState, id: TypeId, t: &Type) -> FoldAction {
        match t {
            Type::Variable(Variable::Bound(level)) => {
                let id = self.bindings.get(level).copied().unwrap_or(id);
                FoldAction::Replace(id)
            }
            _ => FoldAction::Continue,
        }
    }
}
