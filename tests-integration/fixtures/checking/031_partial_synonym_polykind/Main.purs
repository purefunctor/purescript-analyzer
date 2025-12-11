module Main where

-- Polykinded type synonym applications

type PolyApply :: forall k. (k -> Type) -> k -> Type
type PolyApply f a = f a

-- Row-kinded synonyms (like Halogen patterns)
type RowApply :: forall k. (Row k -> Row k) -> Row k -> Row k
type RowApply f r = f r

type AddField :: Row Type -> Row Type
type AddField r = ( field :: String | r )

type AddAnother :: Row Type -> Row Type
type AddAnother r = ( another :: Int | r )

-- Using RowApply with row synonyms
type Applied1 = RowApply AddField ()
type Applied2 = RowApply AddAnother ()

-- Composing row transformers
type ComposeRow :: (Row Type -> Row Type) -> (Row Type -> Row Type) -> Row Type -> Row Type
type ComposeRow f g r = f (g r)

type Combined = ComposeRow AddField AddAnother ()

-- Natural transformation pattern
type NaturalTransformation :: forall k. (k -> Type) -> (k -> Type) -> Type
type NaturalTransformation f g = forall a. f a -> g a

-- Endo pattern from purescript-prelude
type Endo :: forall k. (k -> k -> Type) -> k -> Type
type Endo c a = c a a

-- Function endomorphism
type EndoFn a = Endo Function a

data Function :: Type -> Type -> Type
data Function a b
