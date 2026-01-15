module Main where

import Safe.Coerce (coerce)

newtype Name = Name String
newtype Age = Age Int

newtype Person = Person { name :: Name, age :: Age }
newtype Company = Company { ceo :: Person, name :: Name }

type RawPerson = { name :: String, age :: Int }
type RawCompany = { ceo :: RawPerson, name :: String }

unwrapCompany :: Company -> { ceo :: Person, name :: Name }
unwrapCompany = coerce

fullyUnwrap :: Company -> RawCompany
fullyUnwrap = coerce

fullyWrap :: RawCompany -> Company
fullyWrap = coerce

unwrapPerson :: Person -> RawPerson
unwrapPerson = coerce

nestedFieldCoerce :: { person :: Person } -> { person :: RawPerson }
nestedFieldCoerce = coerce

arrayOfRecords :: Array { name :: Name } -> Array { name :: String }
arrayOfRecords = coerce
