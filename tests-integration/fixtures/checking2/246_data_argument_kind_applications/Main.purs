module Main where

foreign import data Lazy :: Type -> Type
foreign import force :: forall a. Lazy a -> a

data Tuple a b = Tuple a b

data Supply s a = Node (Lazy a) (Lazy (Supply s a)) (Lazy (Supply s a))
