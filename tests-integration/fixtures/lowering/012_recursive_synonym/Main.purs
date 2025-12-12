module Main where

-- Self-recursive synonym
type SelfRecursive = SelfRecursive

-- Mutually recursive synonyms
type MutualA = MutualB
type MutualB = MutualA

-- Triple mutual recursion
type TripleA = TripleB
type TripleB = TripleC
type TripleC = TripleA

-- Valid: synonym referencing data (not recursive synonym expansion)
type ValidSynonym = ValidData Int
data ValidData a = ValidData (ValidSynonym)

-- Valid: data types in mutual recursion with synonym
type MixedSyn a = MixedData a
data MixedData a = MixedData (MixedSyn a)

-- Edge case: synonym with kind signature dependency (from user's example)
foreign import data Carry :: forall k. k -> Type

data Proxy a = Proxy (Carry (KindSigA a))

type KindSigA :: Proxy a -> Proxy a
type KindSigA a = KindSigB a

type KindSigB :: Proxy a -> Proxy a
type KindSigB a = KindSigA a
