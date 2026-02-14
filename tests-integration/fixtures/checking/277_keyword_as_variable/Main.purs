module Main where

foreign import data Test1 :: forall as. as -> as
foreign import data Test2 :: forall phantom. phantom -> phantom
foreign import data Test3 :: forall nominal. nominal -> nominal
foreign import data Test4 :: forall representational. representational -> representational
foreign import data Test5 :: forall hiding. hiding -> hiding
foreign import data Test6 :: forall role. role -> role

test1 = \as -> as
test2 = \phantom -> phantom
test3 = \nominal -> nominal
test4 = \representational -> representational
test5 = \hiding -> hiding
test6 = \role -> role
