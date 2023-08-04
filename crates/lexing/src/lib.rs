//! A lexer for PureScript source code.

mod layout;
mod lexed;
mod lexer;

pub use lexed::Lexed;

use layout::Machine;
use lexer::Lexer;
use syntax::SyntaxKind;

/// Tokenizes a source string.
pub fn lex(source: &str) -> Lexed {
    let mut lexer = Lexer::new(source);
    loop {
        if lexer.is_eof() {
            break lexer.finalize();
        }
        lexer.take_token();
    }
}

/// Applies the layout algorithm.
pub fn layout(lexed: &Lexed) -> Vec<SyntaxKind> {
    let mut machine = Machine::new(lexed);
    loop {
        if machine.is_eof() {
            break machine.finalize();
        }
        machine.take_token();
    }
}

#[cfg(test)]
mod tests {
    use syntax::SyntaxKind;

    use crate::{layout, lex};

    const FULL_SOURCE: &str = r"module Test where

test = ado
    baz
    let foo = bar
    in bar

test = ado in foo

test = ado
    foo <- bar $ let a = 42 in a
    baz <- b
    in bar

test = ado
    foo
    let bar = let a = 42 in a
    let baz = 42
    in bar
    
example1 = do
  foo bar
  <|> baz

example2 = do
  foo bar
  `wat` baz

example3 =
  case _ of
    Foo a -> 1
    Bar b -> 2
    `append` 3

example4 =
  case _ of
    Foo a -> 1
    Bar b -> 2
    + 3

-- Including data because of `|` masking
data Foo
 = Foo
 | Bar
 | Baz

test =
  case foo of
    a | b, c ->
      d
    a | b, c -> d

test = case a, b of
  c, d
   | e ->
     case e of
       f | true -> bar
         | false -> baz
   | f -> g

test a
  | false =
      case false of
        true | a > 12 -> true
  | otherwise = true

test = case a of foo | foo \a -> a -> true

test = a `case _ of x | unit # \_ -> true, true -> const` b

test = case a of
  12 | do that
          that   -> this
     | otherwise -> this

test a b = [ case _ of
  12 | case a, b of
         _, 42 -> b
         _, 12 -> false, b -> true
     | case a, b of
         _, 42 -> b
         _, 12 -> false, b -> true, false ]

test a
  | case a, b of
         _, 42 -> b
         _, 12 -> false, b = true
  | case a, b of
         _, 42 -> b
         _, 12 -> false, b = true

test = case foo of
  Nothing -> a
    where a = 12
  Just a -> do
    what
  where
    foo = bar

test = case f of Foo -> do that
                        where foo = 12

import Foo (class Foo)

class Foo a b c d | a -> b, c -> d where
  foo :: Foo

class Foo a b c d | a -> b, c -> d

instance foo :: Foo

test =
  [ case do foo, bar of
      a | b, c -> d, bar
    ]

test =
  [ case do foo, bar of a | b, c -> d, bar ]

test =
  [ do do do foo, bar ]

test =
  [ \foo -> foo, bar ]

test = foo where
  bar =
    case a, b of
      c, d | d == [case true, w of 1, a -> true, false ] -> d
      e, d | do what, do that -> d

test1 = a
test2 = {
  b
}
test3 = do
  foo
  bar (
    baz
  ) == 12
  baz
test4 = c

test = do
  let foo = bar
  foo

test = do
  let foo = bar
  in baz
  foo

test = do
  let foo = bar
    in baz
  foo

test = do
  foo
  foo do
    bar
  <|> bar

test =
  do
    do do
        foo where bar = baz

foo = do
  if true then
    false
  else if false then do
    that
    else do
     what
  that

instance foo :: Foo Int else bar :: Foo String
else baz :: Foo Boolean

type IntType = (-1)

type IntType' = (-
  -- here's a comment
  1)

test =
  let
    foo
      | bar
      , baz =
        42
      | otherwise = 100
  in
    foo

test = do
  let
    foo
      | bar
      , baz =
        42
      | otherwise = 100
  foo

test = ado
  let
    foo
      | bar
      , baz =
        42
      | otherwise = 100
  foo";

    #[test]
    fn tokens_are_identical() {
        let lexed = lex(FULL_SOURCE);

        let mut left = vec![];
        for index in 0..lexed.len() {
            let kind = lexed.kind(index);
            if kind.is_whitespace_or_comment() {
                continue;
            }
            left.push(kind);
        }

        let right: Vec<_> = layout(&lexed)
            .into_iter()
            .filter(|kind| {
                !matches!(
                    kind,
                    SyntaxKind::LayoutStart | SyntaxKind::LayoutSep | SyntaxKind::LayoutEnd
                )
            })
            .collect();

        assert_eq!(left, right);
    }
}
