module Main where

data Unit = Unit

unit :: Unit -> Int
unit Unit = 1
unit _ = 2
unit Unit = 3

data YesNo = Yes | No

yes :: YesNo -> Int
yes Yes = 1
yes _ = 2
yes Yes = 3

no :: YesNo -> Int
no Yes = 1
no _ = 2
no No = 3

yesNo :: YesNo -> Int
yesNo Yes = 1
yesNo No = 2
yesNo _ = 3
