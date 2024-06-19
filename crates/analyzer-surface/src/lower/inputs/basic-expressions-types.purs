module Main where

application = function argument
constructor = Constructor
prefixedConstructor = A.Constructor
lambda = \a b -> a
letIn = let const a b = a in const
literalInt = 0
literalNumber = 0.0
literalChar = 'a'
literalString = "hello"
literalArray = [ 0, 0 ]
literalRecord = { a: 0, b }
variable = a
prefixedVariable = A.a

application :: Function Argument
arrow :: Argument -> Result
constrained :: Constraint a => a
constructor :: Constructor
qualified :: forall a (b :: Type). a -> b -> a
parenthesized :: (Constructor Argument)
variable :: a
