//! Implements De Bruijn index and level management
//!
//! This module implements structures for managing De Bruijn indices
//! and levels. The type checker uses a locally nameless representation
//! for type variables, with additional abstractions for implicitly
//! quantified type variables in permitted contexts like instance heads.
use std::{fmt, ops};

use lowering::{GraphNodeId, ImplicitBindingId, TypeVariableBindingId};
use rustc_hash::FxHashMap;

/// A well-scoped type variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Variable {
    /// A type variable bound from the core.
    Core,
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Level(pub u32);

impl Level {
    pub fn increment(self) -> Level {
        Level(self.0 + 1)
    }

    pub fn to_index(&self, size: Size) -> Option<Index> {
        if self.0 < size.0 { Some(Index(size.0 - self.0 - 1)) } else { None }
    }
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "&{}", self.0)
    }
}

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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Index(pub u32);

impl Index {
    pub fn in_scope(self, size: Size) -> bool {
        self.0 < size.0
    }

    pub fn increment(self) -> Index {
        Index(self.0 + 1)
    }

    pub fn to_level(&self, size: Size) -> Option<Level> {
        if self.in_scope(size) { Some(Level(size.0 - self.0 - 1)) } else { None }
    }
}

impl fmt::Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "*{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Size(pub u32);

impl Size {
    pub fn increment(self) -> Size {
        Size(self.0 + 1)
    }
}

impl fmt::Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ":{}", self.0)
    }
}

impl Bound {
    /// Returns the current De Bruijn [`Level`].
    pub fn size(&self) -> Size {
        let size = self.inner.len();
        Size(size as u32)
    }

    /// Binds a well-scoped [`Variable`], returning its [`Level`].
    pub fn bind(&mut self, value: Variable) -> Level {
        let level = self.inner.len();
        self.inner.push(value);
        Level(level as u32)
    }

    /// Unbinds names starting from a De Bruijn [`Level`].
    pub fn unbind(&mut self, Level(index): Level) {
        let index = index as usize;
        self.inner.truncate(index);
    }

    /// Finds the De Bruijn [`Level`] of a [`Variable`].
    pub fn level_of(&self, value: Variable) -> Option<Level> {
        let length = self.inner.len() as u32;
        let Index(index) = self.index_of(value)?;
        let level = length - index - 1;
        Some(Level(level))
    }

    /// Finds the De Bruijn [`Index`] of a [`Variable`].
    pub fn index_of(&self, value: Variable) -> Option<Index> {
        let index = self.inner.iter().rev().position(|&other| value == other)?;
        Some(Index(index as u32))
    }

    /// Returns a [`Variable`] given a valid [`Index`].
    pub fn get_index(&self, Index(index): Index) -> Option<Variable> {
        let length = self.inner.len() as u32;
        let index = length - index - 1;
        self.inner.get(index as usize).copied()
    }

    /// Returns a [`Variable`] given a valid [`Level`].
    pub fn get_level(&self, Level(index): Level) -> Option<Variable> {
        self.inner.get(index as usize).copied()
    }

    /// Returns `true` if the variable at `level` was implicitly bound.
    pub fn is_implicit(&self, level: Level) -> bool {
        matches!(self.get_level(level), Some(Variable::Implicit { .. }))
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = (Level, Variable)> {
        self.inner.iter().enumerate().map(|(index, variable)| (Level(index as u32), *variable))
    }

    /// Returns an iterator over variables starting from a given [`Level`].
    ///
    /// If the level is out of bounds, returns an empty iterator.
    pub fn iter_from(&self, level: Level) -> impl Iterator<Item = (Level, Variable)> + '_ {
        let start = level.0 as usize;
        let slice = if let Some(slice) = self.inner.get(start..) { slice } else { &[] };
        slice.iter().enumerate().map(move |(index, &variable)| {
            let level = level.0 + index as u32;
            (Level(level), variable)
        })
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
        let length = self.inner.len() as u32;
        let index = length - index - 1;
        &self.inner[index as usize]
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BoundMap<T> {
    inner: FxHashMap<Level, T>,
}

impl<T> Default for BoundMap<T> {
    fn default() -> Self {
        let inner = FxHashMap::default();
        BoundMap { inner }
    }
}

impl<T> BoundMap<T> {
    pub fn contains(&self, level: Level) -> bool {
        self.inner.contains_key(&level)
    }

    pub fn insert(&mut self, level: Level, value: T) {
        self.inner.insert(level, value);
    }

    pub fn get(&self, level: Level) -> Option<&T> {
        self.inner.get(&level)
    }

    pub fn unbind(&mut self, level: Level) {
        self.inner.retain(|&bound, _| bound < level);
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

        assert_eq!(bound.level_of(VARIABLE_ZERO), Some(Level(0)));
        assert_eq!(bound.level_of(VARIABLE_ONE), Some(Level(1)));

        assert_eq!(bound.index_of(VARIABLE_ZERO), Some(Index(1)));
        assert_eq!(bound.index_of(VARIABLE_ONE), Some(Index(0)));
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

        assert_eq!(bound.level_of(VARIABLE_ZERO), Some(Level(0)));
        assert_eq!(bound.level_of(VARIABLE_ONE), Some(Level(2)));

        assert_eq!(bound.index_of(VARIABLE_ZERO), Some(Index(2)));
        assert_eq!(bound.index_of(VARIABLE_ONE), Some(Index(0)));

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

    #[test]
    fn test_iter_from_middle() {
        let mut bound = Bound::default();
        bound.bind(Variable::Core);
        bound.bind(Variable::Core);
        bound.bind(Variable::Core);

        let items: Vec<_> = bound.iter_from(Level(1)).collect();
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].0, Level(1));
        assert_eq!(items[1].0, Level(2));
    }

    #[test]
    fn test_iter_from_start() {
        let mut bound = Bound::default();
        bound.bind(Variable::Core);
        bound.bind(Variable::Core);

        let items: Vec<_> = bound.iter_from(Level(0)).collect();
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn test_iter_from_end() {
        let mut bound = Bound::default();
        bound.bind(Variable::Core);
        bound.bind(Variable::Core);

        let items: Vec<_> = bound.iter_from(Level(2)).collect();
        assert!(items.is_empty());
    }

    #[test]
    fn test_iter_from_past_end() {
        let mut bound = Bound::default();
        bound.bind(Variable::Core);

        // Should not panic, just return empty iterator
        let items: Vec<_> = bound.iter_from(Level(100)).collect();
        assert!(items.is_empty());
    }
}
