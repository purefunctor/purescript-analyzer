# purescript-analyzer

## Goals

* Provide an independent frontend for providing information for IDE tooling, separate from the compiler.

* Implement a rich editing experience for PureScript through incrementality and resiliency.

* Efficient memory usage (at least lower than the current IDE tooling).

## Features

* Refactorings

* Auto-completion of syntactic structures

* Information for local bindings

* Semantic highlighting (?)


## Architecture

* Christoph: level of incrementality - at the module level

  how much information to recompute on every keystroke e.g.

  type at point, documentation strings, sum type variants
  imported names, new declarations (global and local)

* Resilience: 

  Information is added even on erroneous syntax trees
  or stuff that just partially compiles.


## Code Related

* CST - implemented through `rowan`, which is then lowered to a representation
  that can be "analyzed"

* Streams of tokens are parsed into a structured tree w/ respect to the indentation rules

* Lexer - port over stack machine lexer from the compiler to simplify parsing for indentation rules

* Name resolution step (should happen on the AST before type checking)

```hs
import X (x)

v = x

z = let y = 0 in y
```

```hs
v = X.x

z = let y = 0 in y@ss
```

* Type check the "surface" language rather than desugar the surface to a "core" language and then type check. The problem w/ desugaring is that position information _can_ be discarded one way or another.

```hs
main = do
  foo <- may error here
  bar <- despite error being here
  baz
```

or some other example.

* Type system (constraint-based/bidirectional)--bidirectional type checking can be a bit more local(?)

* Figure out how to store/load information after type checking based on the current context

```hs
main =
  let
    x = 0
    y = 1
  in
    x  -- cursor here
```

Can be type-directed search then return the current environment.

* Boundaries between declarations

```hs
x =
y = 0

x = {};
y = {0};
```

* Parser resilience (stuff like mismatched parentheses, indentation in do blocks)
