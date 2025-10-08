//! Higher-order pattern unification.

use std::sync::Arc;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    check::{CheckContext, CheckState, substitute, unification::UnificationState},
    core::{Pruning, Spine, Type, TypeId, TypeStorage, Variable, debruijn},
};

/// Functions for unification variables.
impl<'s, S> CheckState<'s, S>
where
    S: TypeStorage,
{
    /// Creates a fresh polykinded pattern unification variable
    /// instantiated with the current context:
    ///
    /// Returns [`Pruning(?t, Γ)`](Type::Pruning)
    /// ```purescript
    /// ?k :: Γ -> Type
    /// ?t :: Γ -> ?k
    /// ```
    pub fn fresh_unification(&mut self, context: &CheckContext) -> TypeId {
        // Create a new unification variable for the kind `?k :: <bound> -> Type`
        let kind_pi = self.unification_function_kind(context, context.prim.t);
        let kind_id = self.unification.fresh(kind_pi);

        let kind_pr = self.bound.iter().map(|_| true).collect();
        let kind_ty = self.storage.intern(Type::Pruning(kind_id, kind_pr));

        // Create a new unification variable for the type: `?t :: <bound> -> ?k`
        let type_pi = self.unification_function_kind(context, kind_ty);
        let type_id = self.unification.fresh(type_pi);

        let type_pr = self.bound.iter().map(|_| true).collect();
        self.storage.intern(Type::Pruning(type_id, type_pr))
    }

    /// Create the [`Type::Function`]-based kind representation
    /// using the current environment and a given kind:
    ///
    /// ```text
    /// [ a :: Type, b :: Type ], Type
    ///
    /// (a :: Type) -> (b :: Type) -> Type
    /// ```
    fn unification_function_kind(&mut self, context: &CheckContext, kind: TypeId) -> TypeId {
        self.bound.iter().rev().fold(kind, |kind, (level, _)| {
            let variable_kind = self.kinds.get(level).copied();
            let variable_kind = variable_kind.unwrap_or(context.prim.unknown);
            self.storage.intern(Type::Function(variable_kind, kind))
        })
    }

    /// Finds the terminal solution of a unification variable.
    pub fn force_unification(&self, mut t: TypeId) -> TypeId {
        while let Type::Unification(unification, _) = self.storage.index(t)
            && let UnificationState::Solved(solution) = self.unification.get(*unification).state
        {
            t = solution
        }
        t
    }
}

/// Functions for normalisation.
impl<'s, S> CheckState<'s, S>
where
    S: TypeStorage,
{
    /// Evaluates [`Type::Application`] and [`Type::Pruning`].
    pub fn normalize(&mut self, t: TypeId) -> TypeId {
        let t = self.force_unification(t);
        match *self.storage.index(t) {
            Type::Application(function, argument) => {
                let function = self.normalize(function);
                if let Type::Lambda(lambda) = *self.storage.index(function) {
                    let result = substitute::substitute_bound(self, argument, lambda);
                    self.normalize(result)
                } else {
                    t
                }
            }

            Type::Pruning(unification, ref pruning) => {
                let pruning = Arc::clone(pruning);
                self.apply_pruning(unification, pruning)
            }

            _ => t,
        }
    }

    /// Converts [`Type::Pruning`] into [`Type::Unification`].
    fn apply_pruning(&mut self, unification: u32, pruning: Pruning) -> TypeId {
        let debruijn::Level(level) = self.bound.level();

        let spine = pruning.iter().enumerate().filter(|(_, keep)| **keep).map(|(index, _)| {
            let index = level - index as u32 - 1;
            let index = debruijn::Index(index);

            let variable = Variable::Bound(index);
            self.storage.intern(Type::Variable(variable))
        });

        let spine = spine.collect();
        let core = self.storage.intern(Type::Unification(unification, spine));

        self.normalize(core)
    }
}

/// Functions for inversion and pruning.
impl<'s, S> CheckState<'s, S>
where
    S: TypeStorage,
{
    /// Inverts a [`Spine`] into [`PartialRenaming`] and [`Pruning`].
    pub fn invert_spine(
        &self,
        codomain: debruijn::Level,
        spine: &[TypeId],
    ) -> Option<(PartialRenaming, Option<Pruning>)> {
        let mut domain = debruijn::Level(0);
        let mut renaming = FxHashMap::default();

        let mut nonlinear = FxHashSet::default();
        let mut spine_variables = vec![];

        for argument in spine.iter() {
            let argument = self.force_unification(*argument);
            if let Type::Variable(Variable::Bound(debruijn::Index(index))) =
                self.storage.index(argument)
            {
                let level = codomain.0 - index - 1;

                if renaming.contains_key(&level) || nonlinear.contains(&level) {
                    renaming.remove(&level);
                    nonlinear.insert(level);
                } else {
                    renaming.insert(level, domain);
                }

                domain = domain.increment();
                spine_variables.push(level);
            } else {
                return None;
            }
        }

        let positions = spine_variables.iter().map(|level| !nonlinear.contains(level)).collect();

        let partial_renaming = PartialRenaming { occurs: None, domain, codomain, renaming };
        let pruning = if nonlinear.is_empty() { None } else { Some(positions) };

        Some((partial_renaming, pruning))
    }

    /// Prunes arguments from a [`Type::Function`].
    ///
    /// For example:
    /// ```purescript
    /// -- Given the input type;
    /// (a : Type) -> (b : Type) -> Type
    ///
    /// -- and a Pruning mask;
    /// [true, false]
    ///
    /// -- this function returns:
    /// (a : Type) -> Type
    /// ```
    ///
    /// This function is typically used to prune the kind of a pattern
    /// unification variable along with its [`Spine`]. For example, when
    /// pruning `?0 a b` to `?0 a`, we want to make sure that its kind
    /// is also updated accordingly.
    pub fn prune_type(&mut self, pruning: &[bool], t: TypeId) -> Option<TypeId> {
        let mut partial_renaming = PartialRenaming {
            occurs: None,
            domain: debruijn::Level(0),
            codomain: debruijn::Level(0),
            renaming: FxHashMap::default(),
        };

        let mut arguments = vec![];
        let mut current = t;

        for keep in pruning.iter() {
            let Type::Function(argument, result) = *self.storage.index(current) else {
                return None;
            };

            partial_renaming = if *keep {
                let renamed_argument = self.rename(&partial_renaming, argument)?;
                arguments.push(renamed_argument);
                partial_renaming.lift()
            } else {
                partial_renaming.skip()
            };

            current = result;
        }

        let mut result = self.rename(&partial_renaming, current)?;
        for argument in arguments.into_iter().rev() {
            result = self.storage.intern(Type::Function(argument, result));
        }

        Some(result)
    }

    /// Prunes a pattern unification variable.
    ///
    /// Creates a new pattern unification variable with a pruned kind,
    /// then solves the original unification variable to the new one.
    ///
    /// For example:
    /// ```purescript
    /// -- Given the input variable;
    /// ?0 :: (a : Type) -> (b : Type) -> Type
    ///
    /// -- and a Pruning mask;
    /// [true, false]
    ///
    /// -- this function returns;
    /// ?1 :: (a : Type) -> Type
    ///
    /// -- and creates the solution:
    /// ?0 := Λa. Λb. ?1 a
    /// ```
    pub fn prune_unification(&mut self, pruning: &[bool], unification: u32) -> Option<u32> {
        let unification_kind = self.unification.get(unification).kind;
        let unification_kind = self.prune_type(pruning, unification_kind)?;

        let fresh = self.unification.fresh(unification_kind);
        let solution = self.build_pruned_lambda(pruning, fresh);

        self.unification.solve(unification, solution);
        Some(fresh)
    }

    /// Creates a pattern unification lambda for a pruned unification variable.
    ///
    /// For example:
    /// ```purescript
    /// -- Given the input variable;
    /// ?1
    ///
    /// -- and a Pruning mask;
    /// [true, false]
    ///
    /// -- this function returns:
    /// Λa. Λb. ?1 a
    /// ```
    fn build_pruned_lambda(&mut self, pruning: &[bool], fresh: u32) -> TypeId {
        let level = pruning.len();
        let kept = pruning.iter().enumerate().filter(|(_, keep)| **keep);

        let spine = kept.map(|(index, _)| {
            let index = level - index - 1;
            let index = debruijn::Index(index as u32);

            let variable = Variable::Bound(index);
            self.storage.intern(Type::Variable(variable))
        });

        let spine = spine.collect();
        let unification = self.storage.intern(Type::Unification(fresh, spine));

        pruning.iter().fold(unification, |body, _| self.storage.intern(Type::Lambda(body)))
    }

    pub fn rename(&mut self, partial_renaming: &PartialRenaming, t: TypeId) -> Option<TypeId> {
        let t = self.force_unification(t);
        match *self.storage.index(t) {
            Type::Application(function, argument) => {
                let function = self.rename(partial_renaming, function)?;
                let argument = self.rename(partial_renaming, argument)?;
                Some(self.storage.intern(Type::Application(function, argument)))
            }

            Type::Constructor(_, _) => Some(t),

            Type::Forall(ref binder, body) => {
                let mut binder = binder.clone();

                binder.kind = self.rename(partial_renaming, binder.kind)?;
                let body = self.rename(partial_renaming, body)?;

                Some(self.storage.intern(Type::Forall(binder, body)))
            }

            Type::Function(argument, result) => {
                let argument = self.rename(partial_renaming, argument)?;
                let result = self.rename(partial_renaming, result)?;
                Some(self.storage.intern(Type::Function(argument, result)))
            }

            Type::KindApplication(function, argument) => {
                let function = self.rename(partial_renaming, function)?;
                let argument = self.rename(partial_renaming, argument)?;
                Some(self.storage.intern(Type::KindApplication(function, argument)))
            }

            Type::Lambda(body) => {
                let body = self.rename(partial_renaming, body)?;
                Some(self.storage.intern(Type::Lambda(body)))
            }

            Type::Pruning(_, _) => Some(t),

            Type::Unification(unification, ref spine) => {
                if Some(unification) == partial_renaming.occurs {
                    None
                } else {
                    let spine: Option<Spine> = Arc::clone(spine)
                        .iter()
                        .map(|argument| self.rename(partial_renaming, *argument))
                        .collect();

                    let spine = spine?;
                    Some(self.storage.intern(Type::Unification(unification, spine)))
                }
            }

            Type::Variable(ref variable) => {
                let Variable::Bound(debruijn::Index(index)) = variable else {
                    return Some(t);
                };

                let level = partial_renaming.codomain.0 - index - 1;
                let renamed = partial_renaming.renaming.get(&level)?;

                let index = debruijn::Index(partial_renaming.domain.0 - renamed.0 - 1);
                let variable = Variable::Bound(index);

                Some(self.storage.intern(Type::Variable(variable)))
            }

            Type::Unknown => Some(t),
        }
    }
}

/// Functions for solving unification variables.
impl<'s, S> CheckState<'s, S>
where
    S: TypeStorage,
{
    /// Solves a unification variable to a [`TypeId`],
    /// performing renaming and pruning when appropriate.
    pub fn solve(
        &mut self,
        codomain: debruijn::Level,
        unification: u32,
        spine: &[TypeId],
        solution: TypeId,
    ) -> Option<u32> {
        let (mut partial_renaming, pruning) = self.invert_spine(codomain, spine)?;

        let unification = if let Some(pruning) = pruning {
            self.prune_unification(&pruning, unification)?
        } else {
            unification
        };

        partial_renaming.occurs = Some(unification);
        let renamed = self.rename(&partial_renaming, solution)?;

        let kind = self.unification.get(unification).kind;
        let lambda = self.build_solution_lambda(kind, renamed);

        let normalized = self.normalize(lambda);
        self.unification.solve(unification, normalized);

        Some(unification)
    }

    fn build_solution_lambda(&mut self, kind: TypeId, body: TypeId) -> TypeId {
        let mut bindings = 0;
        let mut current = kind;

        while let Type::Function(_, result) = *self.storage.index(current) {
            bindings += 1;
            current = result;
        }

        (0..bindings).fold(body, |body, _| self.storage.intern(Type::Lambda(body)))
    }
}

#[derive(Debug)]
pub struct PartialRenaming {
    /// Unification variable for the occurs check.
    pub occurs: Option<u32>,
    /// The size of the source context.
    pub domain: debruijn::Level,
    /// The size of the target context.
    pub codomain: debruijn::Level,
    /// The actual variable mapping.
    pub renaming: FxHashMap<u32, debruijn::Level>,
}

impl PartialRenaming {
    pub fn lift(mut self) -> PartialRenaming {
        self.renaming.insert(self.codomain.0, self.domain);
        self.domain = self.domain.increment();
        self.codomain = self.codomain.increment();
        self
    }

    pub fn skip(mut self) -> PartialRenaming {
        self.codomain = self.codomain.increment();
        self
    }
}
