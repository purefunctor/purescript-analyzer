//! Implements De Bruijn index and level management
//!
//! This module implements structures for managing De Bruijn indices
//! and levels. The type checker uses a locally nameless representation
//! for type variables, with additional abstractions for implicitly
//! quantified type variables in permitted contexts like instance heads.
use std::ops;

use lowering::{GraphNodeId, ImplicitBindingId, TypeVariableBindingId};

/// A well-scoped type variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Variable {
    /// A type variable bound by an explicit `forall`.
    Forall(TypeVariableBindingId),
    /// A type variable implicitly bound in a permitted context.
    ///
    /// See also: [`lowering::ImplicitTypeVariable`]
    Implicit { node: GraphNodeId, id: ImplicitBindingId },
}

/// Manages De Bruijn indices and levels.
#[derive(Debug, Default)]
pub struct Bound {
    inner: Vec<Variable>,
}

/// A De Bruijn level.
///
/// De Bruijn levels are used to identify variables from the
/// outermost scope inwards:
///
/// ```purescript
/// ∀ a b. a -> b
///
/// ∀. ∀. &0 -> &1
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Level(u32);

/// A De Bruijn index.
///
/// De Bruijn indices are used to identify variables from the
/// innermost scope outwards:
///
/// ```purescript
/// ∀ a b. a -> b
///
/// ∀. ∀. *1 -> *0
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Index(u32);

impl Bound {
    /// Returns the current De Bruijn [`Level`].
    pub fn level(&self) -> Level {
        let level = self.inner.len();
        Level(level as u32)
    }

    /// Binds a well-scoped [`Variable`], returning its [`Level`].
    pub fn bind(&mut self, value: Variable) -> Level {
        let level = self.inner.len();
        self.inner.push(value);
        Level(level as u32)
    }

    /// Unbinds names after a De Bruijn [`Level`].
    pub fn unbind(&mut self, Level(index): Level) {
        let index = index as usize;
        self.inner.drain(index..);
    }

    /// Finds the De Bruijn [`Level`] of a [`Variable`].
    pub fn level_of(&self, value: Variable) -> Level {
        let length = self.inner.len() as u32;
        let Index(index) = self.index_of(value);
        let level = length - index - 1;
        Level(level)
    }

    /// Finds the De Bruijn [`Index`] of a [`Variable`].
    pub fn index_of(&self, value: Variable) -> Index {
        let index = self
            .inner
            .iter()
            .rev()
            .position(|&other| value == other)
            .expect("invariant violated: unbound variable");
        Index(index as u32)
    }

    /// Returns a [`Variable`] given a valid [`Index`].
    pub fn get_index(&self, Index(index): Index) -> Option<Variable> {
        let length = self.inner.len();
        let index = length - index as usize - 1;
        self.inner.get(index).copied()
    }

    /// Returns a [`Variable`] given a valid [`Level`].
    pub fn get_level(&self, Level(index): Level) -> Option<Variable> {
        self.inner.get(index as usize).copied()
    }
}

impl ops::Index<Level> for Bound {
    type Output = Variable;

    fn index(&self, Level(index): Level) -> &Self::Output {
        &self.inner[index as usize]
    }
}

impl ops::Index<Index> for Bound {
    type Output = Variable;

    fn index(&self, Index(index): Index) -> &Self::Output {
        let length = self.inner.len();
        let index = length - index as usize - 1;
        &self.inner[index]
    }
}

#[cfg(test)]
mod tests {
    use std::num::NonZeroU32;

    use lowering::TypeVariableBindingId;

    use super::{Bound, Index, Level, Variable};

    const ONE: NonZeroU32 = NonZeroU32::new(1).unwrap();
    const TWO: NonZeroU32 = NonZeroU32::new(2).unwrap();

    const VARIABLE_ZERO: Variable = Variable::Forall(TypeVariableBindingId::new(ONE));
    const VARIABLE_ONE: Variable = Variable::Forall(TypeVariableBindingId::new(TWO));

    #[test]
    fn test_index_level() {
        let mut bound = Bound::default();
        bound.bind(VARIABLE_ZERO);
        bound.bind(VARIABLE_ONE);

        assert_eq!(bound.level_of(VARIABLE_ZERO), Level(0));
        assert_eq!(bound.level_of(VARIABLE_ONE), Level(1));

        assert_eq!(bound.index_of(VARIABLE_ZERO), Index(1));
        assert_eq!(bound.index_of(VARIABLE_ONE), Index(0));
    }

    #[test]
    fn test_indexing() {
        let mut bound = Bound::default();
        bound.bind(VARIABLE_ZERO);
        bound.bind(VARIABLE_ONE);

        assert_eq!(bound[Level(0)], VARIABLE_ZERO);
        assert_eq!(bound[Level(1)], VARIABLE_ONE);

        assert_eq!(bound[Index(0)], VARIABLE_ONE);
        assert_eq!(bound[Index(1)], VARIABLE_ZERO);

        assert_eq!(bound.get_level(Level(0)), Some(VARIABLE_ZERO));
        assert_eq!(bound.get_level(Level(1)), Some(VARIABLE_ONE));

        assert_eq!(bound.get_index(Index(0)), Some(VARIABLE_ONE));
        assert_eq!(bound.get_index(Index(1)), Some(VARIABLE_ZERO));
    }

    #[test]
    fn test_shadowing() {
        let mut bound = Bound::default();
        bound.bind(VARIABLE_ZERO);
        bound.bind(VARIABLE_ONE);
        bound.bind(VARIABLE_ONE);

        assert_eq!(bound.level_of(VARIABLE_ZERO), Level(0));
        assert_eq!(bound.level_of(VARIABLE_ONE), Level(2));

        assert_eq!(bound.index_of(VARIABLE_ZERO), Index(2));
        assert_eq!(bound.index_of(VARIABLE_ONE), Index(0));

        assert_eq!(bound[Level(0)], VARIABLE_ZERO);
        assert_eq!(bound[Level(1)], VARIABLE_ONE);
        assert_eq!(bound[Level(2)], VARIABLE_ONE);

        assert_eq!(bound[Index(0)], VARIABLE_ONE);
        assert_eq!(bound[Index(1)], VARIABLE_ONE);
        assert_eq!(bound[Index(2)], VARIABLE_ZERO);
    }

    #[test]
    fn test_unbind() {
        let mut bound = Bound::default();

        let zero = bound.bind(VARIABLE_ZERO);
        let one = bound.bind(VARIABLE_ONE);

        bound.unbind(zero);

        assert_eq!(bound.get_level(zero), None);
        assert_eq!(bound.get_level(one), None);
    }
}
