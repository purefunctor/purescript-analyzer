use crate::position::Position;

#[derive(Debug)]
pub struct Layout {
    pub kind: LayoutKind,
    pub position: Position,
}

#[derive(Debug)]
pub enum LayoutKind {
    Root,
    Module,
    Instance,
    Parenthesis,
}

impl Layout {
    pub fn new(kind: LayoutKind, position: Position) -> Layout {
        Layout { kind, position }
    }
}

/*

Layout contexts dictate the separation of tokens depending
on their indentation. An example of a layout context is an
indentation context, where groups of tokens are separated
once a future token aligns with a selected column. For example:

```hs
instance Life a where
  life :: a -> Int
  life _ = 42

instance ..
```

In order to disambiguate between the tokens for the annotation
declaration and the value declaration, we dictate that tokens
aligned with column 3 denote a start of a different group.
We can imagine the parser inserting invisible markers like this:

```hs
instance Life a where{
  life :: a -> Int;
  life _ = 42}

instance ..
```

Notice how after the value declaration `}` is introduced instead.
Since the next token is unindented, we "break" out of the indentation
context introduced by the instance declaration, allowing us to move
on to the next one.


Another kind of layout context is the parenthesis context, which is
odd in that it completely removes the layout rules that we've just
established, meaning that the following example parses fine:

```hs
f = (
0
+
1
)
```

The `(` token still cannot start at column 1 however, but any token
after it completely sidesteps the layout algorithm and just... ignores
it.


And so, how are these two different layout contexts different? The idea
is that the parser provides a function to check if the following token
finishes a group of tokens. This is particularly useful for repetitions,
for example, while parsing declarations in an instance declaration, you
can check if the current token breaks the repetition. Or while parsing
expressions, you can keep consuming tokens until you hit a separator
token.

*/
