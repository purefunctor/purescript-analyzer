module Main where

import Prim.TypeError (class Warn, Text, Quote, QuoteLabel, Beside, Above)

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

warnBasic :: forall a. Warn (Text "This function is deprecated") => a -> a
warnBasic x = x

useWarnBasic :: Int
useWarnBasic = warnBasic 42

warnBeside :: forall a. Warn (Beside (Text "Left ") (Text "Right")) => a -> a
warnBeside x = x

useWarnBeside :: Int
useWarnBeside = warnBeside 42

warnAbove :: forall a. Warn (Above (Text "Line 1") (Text "Line 2")) => a -> a
warnAbove x = x

useWarnAbove :: Int
useWarnAbove = warnAbove 42

warnQuote :: forall a. Warn (Beside (Text "Got type: ") (Quote a)) => Proxy a -> Proxy a
warnQuote p = p

useWarnQuote :: Proxy Int
useWarnQuote = warnQuote Proxy

warnQuoteLabel :: forall a. Warn (Beside (Text "Label: ") (QuoteLabel "myField")) => a -> a
warnQuoteLabel x = x

useWarnQuoteLabel :: Int
useWarnQuoteLabel = warnQuoteLabel 42

warnQuoteLabelSpaces :: forall a. Warn (Beside (Text "Label: ") (QuoteLabel "h e l l o")) => a -> a
warnQuoteLabelSpaces x = x

useWarnQuoteLabelSpaces :: Int
useWarnQuoteLabelSpaces = warnQuoteLabelSpaces 42

warnQuoteLabelQuote :: forall a. Warn (Beside (Text "Label: ") (QuoteLabel "hel\"lo")) => a -> a
warnQuoteLabelQuote x = x

useWarnQuoteLabelQuote :: Int
useWarnQuoteLabelQuote = warnQuoteLabelQuote 42

warnQuoteLabelRaw :: forall a. Warn (Beside (Text "Label: ") (QuoteLabel """raw\nstring""")) => a -> a
warnQuoteLabelRaw x = x

useWarnQuoteLabelRaw :: Int
useWarnQuoteLabelRaw = warnQuoteLabelRaw 42
