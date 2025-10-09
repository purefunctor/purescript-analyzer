//! Higher-order pattern unification.

use rustc_hash::FxHashMap;

use crate::{
    check::{CheckContext, CheckState, substitute, unification::UnificationState},
    core::{Type, TypeId, TypeStorage, Variable, debruijn},
};

/// Functions for unification variables.
impl<'s, S> CheckState<'s, S>
where
    S: TypeStorage,
{
    /// Creates a fresh pattern unification variable with the provided
    /// kind and instantiated with the current context:
    ///
    /// Returns [`Unification(?t, Γ)`](Type::Unification)
    /// ```purescript
    /// ?t :: Γ -> <kind>
    /// ```
    pub fn fresh_unification_kinded(&mut self, context: &CheckContext, kind: TypeId) -> TypeId {
        let function = self.unification_function_kind(context, kind);
        let unification = self.unification.fresh(function);

        let spine = self.bound.iter().map(|(level, _)| level).collect();
        self.storage.intern(Type::Unification(unification, spine))
    }

    /// Creates a fresh polykinded pattern unification variable
    /// instantiated with the current context:
    ///
    /// Returns [Unification(?t, Γ)`](Type::Unification)
    /// ```purescript
    /// ?k :: Γ -> Type
    /// ?t :: Γ -> ?k
    /// ```
    pub fn fresh_unification(&mut self, context: &CheckContext) -> TypeId {
        let kind_ty = self.fresh_unification_type(context);
        self.fresh_unification_kinded(context, kind_ty)
    }

    /// Creates a fresh [`Type`]-kinded pattern unification variable
    /// instantiated with the current context:
    ///
    /// Returns [`Unification(?t, Γ)`](Type::Unification)
    /// ```purescript
    /// ?t :: Γ -> Type
    /// ```
    pub fn fresh_unification_type(&mut self, context: &CheckContext) -> TypeId {
        self.fresh_unification_kinded(context, context.prim.t)
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

            _ => t,
        }
    }
}

/// Functions for solving unification variables.
impl<'s, S> CheckState<'s, S>
where
    S: TypeStorage,
{
    /// Solves a unification variable to a [`TypeId`].
    pub fn solve(
        &mut self,
        codomain: debruijn::Level,
        unification: u32,
        spine: &[debruijn::Level],
        solution: TypeId,
    ) -> Option<u32> {
        let occurs = Some(unification);
        let domain = debruijn::Level(spine.len() as u32);

        if !self.is_well_scoped(occurs, codomain, domain, solution) {
            return None;
        }

        let kind = self.unification.get(unification).kind;
        let lambda = self.build_solution_lambda(kind, solution);

        let normalized = self.normalize(lambda);
        self.unification.solve(unification, normalized);

        Some(unification)
    }

    fn is_well_scoped(
        &self,
        occurs: Option<u32>,
        codomain: debruijn::Level,
        domain: debruijn::Level,
        solution: TypeId,
    ) -> bool {
        match *self.storage.index(solution) {
            Type::Application(function, argument) => {
                self.is_well_scoped(occurs, codomain, domain, function)
                    && self.is_well_scoped(occurs, codomain, domain, argument)
            }

            Type::Constructor(_, _) => true,

            Type::Forall(ref binder, inner) => {
                let inner_codomain = domain.increment();
                self.is_well_scoped(occurs, codomain, domain, binder.kind)
                    && self.is_well_scoped(occurs, codomain, inner_codomain, inner)
            }

            Type::Function(argument, result) => {
                self.is_well_scoped(occurs, codomain, domain, argument)
                    && self.is_well_scoped(occurs, codomain, domain, result)
            }

            Type::KindApplication(function, argument) => {
                self.is_well_scoped(occurs, codomain, domain, function)
                    && self.is_well_scoped(occurs, codomain, domain, argument)
            }

            Type::Lambda(body) => {
                let body_codomain = domain.increment();
                self.is_well_scoped(occurs, codomain, body_codomain, body)
            }

            Type::Unification(unification, _) => Some(unification) != occurs,

            Type::Variable(ref variable) => match variable {
                Variable::Implicit(_) => true,
                Variable::Skolem(_) => true,
                Variable::Bound(index) => {
                    let level = codomain.0 - index.0 - 1;
                    level < domain.0
                }
                Variable::Free(_) => true,
            },

            Type::Unknown => true,
        }
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
