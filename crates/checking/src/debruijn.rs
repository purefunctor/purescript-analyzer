use std::ops;

use lowering::TypeVariableBindingId;

/// Allocates De Bruijn indices for bound type variables.
#[derive(Debug, Default)]
pub struct Bound {
    inner: Vec<TypeVariableBindingId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Level(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Index(u32);

impl Bound {
    /// Get the current De Bruijn [`Level`].
    pub fn level(&self) -> Level {
        let level = self.inner.len();
        Level(level as u32)
    }

    /// Bind a well-scoped [`TypeVariableBindingId`].
    pub fn bind(&mut self, value: TypeVariableBindingId) -> Level {
        let level = self.inner.len();
        self.inner.push(value);
        Level(level as u32)
    }

    /// Unbind names after a De Bruijn [`Level`].
    pub fn unbind(&mut self, Level(index): Level) {
        let index = index as usize;
        self.inner.drain(index..);
    }

    /// Find the De Bruijn [`Level`] of a bound type variable.
    pub fn level_of(&self, value: TypeVariableBindingId) -> Level {
        let length = self.inner.len() as u32;
        let Index(index) = self.index_of(value);
        let level = length - index - 1;
        Level(level as u32)
    }

    /// Find the De Bruijn [`Index`] of a bound type variable.
    pub fn index_of(&self, value: TypeVariableBindingId) -> Index {
        let index = self
            .inner
            .iter()
            .rev()
            .position(|&other| value == other)
            .expect("invariant violated: unbound variable");
        Index(index as u32)
    }
}

impl ops::Index<Level> for Bound {
    type Output = TypeVariableBindingId;

    fn index(&self, Level(index): Level) -> &Self::Output {
        &self.inner[index as usize]
    }
}

impl ops::Index<Index> for Bound {
    type Output = TypeVariableBindingId;

    fn index(&self, Index(index): Index) -> &Self::Output {
        let length = self.inner.len();
        let index = length - index as usize - 1;
        &self.inner[index as usize]
    }
}

#[cfg(test)]
mod tests {
    use la_arena::{Idx, RawIdx};
    use lowering::TypeVariableBindingId;

    use super::{Bound, Index, Level};

    const ZERO: TypeVariableBindingId = Idx::from_raw(RawIdx::from_u32(0));
    const ONE: TypeVariableBindingId = Idx::from_raw(RawIdx::from_u32(1));

    #[test]
    fn test_index_level() {
        let mut bound = Bound::default();
        bound.bind(ZERO);
        bound.bind(ONE);

        assert_eq!(bound.level_of(ZERO), Level(0));
        assert_eq!(bound.level_of(ONE), Level(1));

        assert_eq!(bound.index_of(ZERO), Index(1));
        assert_eq!(bound.index_of(ONE), Index(0));
    }

    #[test]
    fn test_indexing() {
        let mut bound = Bound::default();
        bound.bind(ZERO);
        bound.bind(ONE);

        assert_eq!(bound[Level(0)], ZERO);
        assert_eq!(bound[Level(1)], ONE);

        assert_eq!(bound[Index(0)], ONE);
        assert_eq!(bound[Index(1)], ZERO);
    }

    #[test]
    fn test_shadowing() {
        let mut bound = Bound::default();
        bound.bind(ZERO);
        bound.bind(ONE);
        bound.bind(ONE);

        assert_eq!(bound.level_of(ZERO), Level(0));
        assert_eq!(bound.level_of(ONE), Level(2));

        assert_eq!(bound.index_of(ZERO), Index(2));
        assert_eq!(bound.index_of(ONE), Index(0));

        assert_eq!(bound[Level(0)], ZERO);
        assert_eq!(bound[Level(1)], ONE);
        assert_eq!(bound[Level(2)], ONE);

        assert_eq!(bound[Index(0)], ONE);
        assert_eq!(bound[Index(1)], ONE);
        assert_eq!(bound[Index(2)], ZERO);
    }
}
