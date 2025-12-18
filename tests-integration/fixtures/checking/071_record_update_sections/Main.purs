module Main where

-- { a :: t1 | r } -> { a :: Int | r }
singleFieldUpdate = _ { a = 1 }

-- { a :: t1, b :: t2, c :: t3 | r } -> { a :: Int, b :: Boolean, c :: String | r }
multipleFieldUpdate = _ { a = 2, b = true, c = "three" }

-- { a :: { b :: { c :: t1 | r2 } | r1 } | r0 } -> { a :: { b :: { c :: Int | r2 } | r1 } | r0 }
nestedRecordUpdate = _ { a { b { c = 42 } } }

-- { a :: t1 | r } -> { a :: Int -> Int | r }
updateWithValueSection = _ { a = (_ + 1) }

-- { a :: t1 | r } -> t2 -> { a :: t2 | r }
updateWithSectionInteraction = _ { a = _ }

-- { foo :: t1 | r } -> { foo :: Int | r }
polymorphicRecordUpdate = _ { foo = 0 }

-- Array { x :: t1 | r } -> Array { x :: Int | r }
higherOrderContext = map (_ { x = 10 })

-- { x :: t1, y :: t2 | r } -> t3 -> t4 -> { x :: t3, y :: t4 | r }
multipleSectionsInteraction = _ { x = _, y = _ }

-- { a :: { b :: t1 | r1 } | r0 } -> t2 -> { a :: { b :: t2 | r1 } | r0 }
nestedSectionInteraction = _ { a { b = _ } }

-- { a :: t1, b :: t2 | r } -> { a :: Int -> Int, b :: Int | r }
mixedSections = _ { a = _ + 1, b = 2 }

-- { a :: t1 | r } -> { a :: { b :: t2 | r1 } -> t2 | r }
recordAccessSectionUpdate = _ { a = _.b }

-- t1 -> { a :: t1 }
concreteRecordUpdateSection = ({ a: 1 } { a = _ })

map :: forall a b. (a -> b) -> Array a -> Array b
map _ _ = []

infixl 6 add as +
add :: Int -> Int -> Int
add x y = x
