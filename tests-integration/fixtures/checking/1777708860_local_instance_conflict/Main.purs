module Main where

class Class :: Type -> Constraint
class Class a

instance classInt :: Class
foreign import classInt :: Int

test = [classInt]
