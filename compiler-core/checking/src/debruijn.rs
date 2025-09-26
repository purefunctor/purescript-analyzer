use std::ops;

use lowering::{GraphNodeId, ImplicitBindingId, TypeVariableBindingId};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Binding {
    Forall(TypeVariableBindingId),
    Implicit(GraphNodeId, ImplicitBindingId),
}

/// Assigns De Bruijn levels and indices for type variables.
#[derive(Debug, Default)]
pub struct Bound {
    inner: Vec<Binding>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Level(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Index(u32);

impl Bound {
    /// Returns the current De Bruijn [`Level`].
    pub fn level(&self) -> Level {
        let level = self.inner.len();
        Level(level as u32)
    }

    /// Binds a well-scoped [`Binding`], returning its [`Level`].
    pub fn bind(&mut self, value: Binding) -> Level {
        let level = self.inner.len();
        self.inner.push(value);
        Level(level as u32)
    }

    /// Unbinds names after a De Bruijn [`Level`].
    pub fn unbind(&mut self, Level(index): Level) {
        let index = index as usize;
        self.inner.drain(index..);
    }

    /// Finds the De Bruijn [`Level`] of a [`Binding`].
    pub fn level_of(&self, value: Binding) -> Level {
        let length = self.inner.len() as u32;
        let Index(index) = self.index_of(value);
        let level = length - index - 1;
        Level(level)
    }

    /// Finds the De Bruijn [`Index`] of a [`Binding`].
    pub fn index_of(&self, value: Binding) -> Index {
        let index = self
            .inner
            .iter()
            .rev()
            .position(|&other| value == other)
            .expect("invariant violated: unbound variable");
        Index(index as u32)
    }

    /// Returns a [`Binding`] given a valid [`Index`].
    pub fn get_index(&self, Index(index): Index) -> Option<Binding> {
        let length = self.inner.len();
        let index = length - index as usize - 1;
        self.inner.get(index).copied()
    }

    /// Returns a [`Binding`] given a valid [`Level`].
    pub fn get_level(&self, Level(index): Level) -> Option<Binding> {
        self.inner.get(index as usize).copied()
    }
}

impl ops::Index<Level> for Bound {
    type Output = Binding;

    fn index(&self, Level(index): Level) -> &Self::Output {
        &self.inner[index as usize]
    }
}

impl ops::Index<Index> for Bound {
    type Output = Binding;

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

    use super::{Binding, Bound, Index, Level};

    const ONE: NonZeroU32 = NonZeroU32::new(1).unwrap();
    const TWO: NonZeroU32 = NonZeroU32::new(2).unwrap();

    const BINDING_ZERO: Binding = Binding::Forall(TypeVariableBindingId::new(ONE));
    const BINDING_ONE: Binding = Binding::Forall(TypeVariableBindingId::new(TWO));

    #[test]
    fn test_index_level() {
        let mut bound = Bound::default();
        bound.bind(BINDING_ZERO);
        bound.bind(BINDING_ONE);

        assert_eq!(bound.level_of(BINDING_ZERO), Level(0));
        assert_eq!(bound.level_of(BINDING_ONE), Level(1));

        assert_eq!(bound.index_of(BINDING_ZERO), Index(1));
        assert_eq!(bound.index_of(BINDING_ONE), Index(0));
    }

    #[test]
    fn test_indexing() {
        let mut bound = Bound::default();
        bound.bind(BINDING_ZERO);
        bound.bind(BINDING_ONE);

        assert_eq!(bound[Level(0)], BINDING_ZERO);
        assert_eq!(bound[Level(1)], BINDING_ONE);

        assert_eq!(bound[Index(0)], BINDING_ONE);
        assert_eq!(bound[Index(1)], BINDING_ZERO);

        assert_eq!(bound.get_level(Level(0)), Some(BINDING_ZERO));
        assert_eq!(bound.get_level(Level(1)), Some(BINDING_ONE));

        assert_eq!(bound.get_index(Index(0)), Some(BINDING_ONE));
        assert_eq!(bound.get_index(Index(1)), Some(BINDING_ZERO));
    }

    #[test]
    fn test_shadowing() {
        let mut bound = Bound::default();
        bound.bind(BINDING_ZERO);
        bound.bind(BINDING_ONE);
        bound.bind(BINDING_ONE);

        assert_eq!(bound.level_of(BINDING_ZERO), Level(0));
        assert_eq!(bound.level_of(BINDING_ONE), Level(2));

        assert_eq!(bound.index_of(BINDING_ZERO), Index(2));
        assert_eq!(bound.index_of(BINDING_ONE), Index(0));

        assert_eq!(bound[Level(0)], BINDING_ZERO);
        assert_eq!(bound[Level(1)], BINDING_ONE);
        assert_eq!(bound[Level(2)], BINDING_ONE);

        assert_eq!(bound[Index(0)], BINDING_ONE);
        assert_eq!(bound[Index(1)], BINDING_ONE);
        assert_eq!(bound[Index(2)], BINDING_ZERO);
    }

    #[test]
    fn test_unbind() {
        let mut bound = Bound::default();

        let zero = bound.bind(BINDING_ZERO);
        let one = bound.bind(BINDING_ONE);

        bound.unbind(zero);

        assert_eq!(bound.get_level(zero), None);
        assert_eq!(bound.get_level(one), None);
    }
}
