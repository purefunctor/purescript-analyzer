module ValueAnnotation where
  
identity :: forall a. a -> a
const :: forall a b. a -> b -> a

identity :: forall (a :: Type). a -> a
const :: forall (a :: Type) (b :: Type). a -> b -> a

identity :: forall @a. a -> a
const :: forall @a @b. a -> b -> a

identity :: forall (@a :: Type). a -> a
const :: forall (@a :: Type) (@b :: Type). a -> b -> a
