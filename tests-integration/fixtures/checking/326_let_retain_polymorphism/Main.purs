module Main where

class IxFunctor :: forall ix. (ix -> ix -> Type -> Type) -> Constraint
class IxFunctor f where
  imap :: forall a b x y. (a -> b) -> f x y a -> f x y b

class IxApply :: forall ix. (ix -> ix -> Type -> Type) -> Constraint
class IxFunctor m <= IxApply m where
  iapply :: forall a b x y z. m x y (a -> b) -> m y z a -> m x z b

class IxApplicative :: forall ix. (ix -> ix -> Type -> Type) -> Constraint
class IxApply m <= IxApplicative m where
  ipure :: forall a x. a -> m x x a

class IxBind :: forall ix. (ix -> ix -> Type -> Type) -> Constraint
class IxApply m <= IxBind m where
  ibind :: forall a b x y z. m x y a -> (a -> m y z b) -> m x z b

class IxMonad :: forall ix. (ix -> ix -> Type -> Type) -> Constraint
class (IxApplicative m, IxBind m) <= IxMonad m

iap :: forall m a b x y z. IxMonad m => m x y (a -> b) -> m y z a -> m x z b
iap f a = do
  f' <- f
  a' <- a
  ipure (f' a')
  where
    bind = ibind
