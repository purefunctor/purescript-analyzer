module Main where

foreign import data Unit :: Type
foreign import data Identity :: Type -> Type

foreign import data Run :: Row Type -> Type -> Type

type NaturalTransformation f g = forall a. f a -> g a
infixr 4 type NaturalTransformation as ~>

newtype Interpreter r1 r2 = Interpreter (Run r1 ~> Run r2)

unInterpreter :: forall r1 r2. Interpreter r1 r2 -> (Run r1 ~> Run r2)
unInterpreter (Interpreter i) = i

composeInterpreter :: forall r1 r2 r3. Interpreter r1 r2 -> Interpreter r2 r3 -> Interpreter r1 r3
composeInterpreter _ _ = interpreter

infixr 9 composeInterpreter as >>>

applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped a f = f a

infixl 1 applyFlipped as #

identity :: forall a. a -> a
identity a = a

data ComputationType = TestWithName

foreign import data SpecT :: (Type -> Type) -> Type -> (Type -> Type) -> Type -> Type

foreign import hoistSpec :: forall m' m i a b. (m ~> m') -> (ComputationType -> a ~> b) -> SpecT a i m ~> SpecT b i m'
foreign import interpreter :: forall r1 r2. Interpreter r1 r2

foreign import data A :: Type
foreign import data B :: Type
foreign import data C :: Type

type R1 r = (a :: A, b :: B, c :: C | r)
type R2 r = (b :: B, c :: C | r)
type R3 r = (c :: C | r)
type R4 r = r

newtype Eff = Eff (forall r. Interpreter (R1 r) (R2 r))

unEff :: Eff -> (forall r. Interpreter (R1 r) (R2 r))
unEff (Eff i) = i

foreign import eff :: Eff

newtype Env = Env (forall r. Interpreter (R2 r) (R3 r))

unEnv :: Env -> (forall r. Interpreter (R2 r) (R3 r))
unEnv (Env i) = i

foreign import env :: Env

newtype Aff = Aff (forall r. Interpreter (R3 r) (R4 r))

unAff :: Aff -> (forall r. Interpreter (R3 r) (R4 r))
unAff (Aff i) = i

foreign import aff :: Aff

foreign import base :: SpecT (Run (R1 ())) Unit Identity Unit

test :: SpecT (Run (R4 ())) Unit Identity Unit
test =
  hoistSpec identity
    (\_ a ->
      a
        # unInterpreter (unEff eff >>> unEnv env)
        # unInterpreter (unAff aff)
    )
    base
