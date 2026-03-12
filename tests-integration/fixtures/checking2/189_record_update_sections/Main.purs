module Main where

singleFieldUpdate = _ { a = 1 }

multipleFieldUpdate = _ { a = 2, b = true, c = "three" }

nestedRecordUpdate = _ { a { b { c = 42 } } }

updateWithValueSection = _ { a = (_ + 1) }

updateWithSectionInteraction = _ { a = _ }

polymorphicRecordUpdate = _ { foo = 0 }

higherOrderContext = map (_ { x = 10 })

multipleSectionsInteraction = _ { x = _, y = _ }

nestedSectionInteraction = _ { a { b = _ } }

mixedSections = _ { a = _ + 1, b = 2 }

recordAccessSectionUpdate = _ { a = _.b }

concreteRecordUpdateSection = ({ a: 1 } { a = _ })

map :: forall a b. (a -> b) -> Array a -> Array b
map f x = []

infixl 6 add as +

add :: Int -> Int -> Int
add x y = x
