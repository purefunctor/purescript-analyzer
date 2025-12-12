module Main where

type F = G
type G = F

type H = H

testF :: F -> F
testF x = x

testG :: G -> G
testG x = x

testH :: H -> H
testH x = x

type Valid = Int

testValid :: Valid -> Valid
testValid x = x
