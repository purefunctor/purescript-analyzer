# monarch-parsing
This crate provides the lexer and parser for the CST defined by `syntax`.

## Notes

### Lowering

In order to perform operations such as type checking, the CST is "lowered" into another typed representation, usually the AST. Some refactorings can be performed on the CST, like operator rebracketing, while some refactorings can only be performed on the AST, during or after lowering. Name resolution, for example, can occur during lowering; there's usually enough information in the CST to resolve names e.g. turning the `Nil` constructor into `Data.List.Nil`.

### Reparsing

Given the architecture of the parser and the CST, reparsing can be employed to modify certain tree nodes. Take for example the `ExpressionOperatorChain` node:

```hs
ExpressionOperatorChain
  Expression  "1"
  Operator    "+"
  Expression  "2"
  Operator    "*"
  Expression  "3"
```

It simply describes a chain of operator applications, since at this point in parsing, we have no idea what the associativity is for each `Operator`. Once the `FixityDeclaration` nodes have been parsed, and after some degree of name resolution, we're able to transform the node into the following using a basic Pratt-style parser!

```hs
ExpressionBinaryOperation
  Expression    "1"
  Operator      "+"
  ExpressionBinaryOperation
    Expression  "2"
    Operator    "*"
    Expression  "3"
```

### Parser

There's a couple of considerations that have to be made for the parser architecture, in particular with its input type. For the layout algorithm to work, the parser has to be aware of the indentation of a token, and to simplify peeking into a single-token lookup, whitespace and comment tokens are not included in the input type.

That being said, rather than simply having `Vec<SyntaxKind>` as the input, it'd be more like `Vec<SyntaxKind>, Vec<u32>` where the latter is the column offset for that specific token.

Conceptually, this means that the parser cannot be "generic" over its input type without offset information. For inputs coming from lexed tokens, the offset information is easy to compute. However for arbitrary token sources e.g. macro expansion, programmatic sources, even a node in the CST, the offset is a bit more difficult to compute.

## Old Notes

### Layout Algorithm

A distinct feature for the lexer is the layout algorithm, which is a stack machine. Each stack entry is a pair of two values, a column offset and a tag. When a token that enforces a layout is encountered, an entry is pushed into the stack with that token's tag. For example, let's say we're parsing a do-block.

```hs
main = do
  hello
  world

hello = pure ()
```

When `do` is encountered, the lexer peeks into the next significant[^1] token to determine its starting position, after which `(2, Do)` is pushed onto the stack. After consuming the `do` token, a `LayoutStart` token is inserted. We proceed to lex `hello`, and we peek into the next significant token: `world`. Since the offset for `world` is the same as the topmost column offset in the stack, we insert a `LayoutSep` token. After consuming `world`, we peek and see that `hello` occurs at the offset `0`, which is less than the topmost[^2] column offset in the stack. We proceed by inserting a `LayoutEnd` token and finally, we pop the stack to remove `(2, Do)`. A visualization of our source file with the ephemeral tokens may look like:

```hs
main = do{
  hello;
  world}

hello = pure ()
```

[^1]: Neither whitespace nor comment.
[^2]: Certain tags serve as "masks" for "indentation" tags, so rather than just a `peek` at the top, we perform a reverse lookup. Likewise, rather than a singular `pop`, we remove all elements after the indentation tag.

#### Implementation 1

`Lexer` and `Layout` are implemented separately, where `Lexer` provides an interface for parsing tokens and peeking into future tokens, while `Layout` provides the stack machine, which consumes the `Lexer` and emits tokens including ones pertinent to layout. This separation allows the `Lexer` to operate at the character level, while the `Layout` operates at the token level. The `Lexer` would have the following API surface:

```rs
// Internal
trait Lexer {
  fn consumed(&self) -> usize;

  fn peek(&self) -> char;

  fn take(&mut self) -> char;

  fn take_until(&mut self, f: impl Fn(char) -> bool);
}

// External
trait Lexer {
  fn take_token(&mut self) -> (usize, usize, SyntaxKind);

  fn take_trivial(&mut self) -> Option<(usize, usize, SyntaxKind)>;

  fn peek_token(&mut self) -> (usize, usize, SyntaxKind);
}
```

The `Layout` token then uses the builder pattern to build the stream of tokens, where it has the following API surface:

```rs
trait Layout {
  fn token(&mut self, now: (usize, usize, SyntaxKind), next: (usize, usize, SyntaxKind));

  fn trivial(&mut self, trivial: (usize, usize, SyntaxKind));

  fn finish(self) -> Vec<(usize, usize, SyntaxKind)>;
}
```

The `lex` function can tie them together using the following:

```rs
fn lex(mut lexer: impl Lexer, mut layout: impl Layout) -> Vec<(usize, usize, SyntaxKind)> {
  loop {
    if let now@(_, _, EndOfFile) = lexer.peek_token() {
      layout.token(now, now);
      break layout.finish();
    }

    let now = lexer.take_token();
    let next = lexer.peek_token();
    layout.token(now, next);

    while let Some(trivial) = lexer.take_trivial() {
      layout.trivial(trivial);
    }
  }
}
```

This vector is then consumed by the `parser` to build a syntax tree.

#### Implementation 2

Another approach for this algorithm is to implement layout rules within the handwritten parser. In particular, rather than emitting tokens, the state machine modifies how the parser reacts directly instead. For instance, let's say we want to parse the following code:

```hs
main = do
  hello
  world

hello = pure unit
```

Once the parser reaches `do`, it peeks into the next token to figure out the start of the indentation context, where it then inserts `(2, Do)`. If a token like the `hello` declaration breaks this indentation context `(2, Do)` is popped. In particular when constructing the CST with `rowan`, a start of an indentation context means the beginning of the node. We can imagine the code above parsing into the following:

```
ValueDeclaration
  Name
    Lower
  Equal
  Do
    DoKw
    DoStatement
      Name
        Lower
    DoStatement
      Name
        Lower

ValueDeclaration
  Name
    Lower
  Equal
  Application
    NameRef
      Lower
    NameRef
      Lower
```

`Lexer` operates on the character level in order to produce `Lexed`, which abstracts over `Vec<SyntaxKind>`. `Parser` operates on an `Input` stream, which is similar to `Lexed` except with no whitespace or comment tokens; an `Output` is produced, which is a stream of "events" that describes a depth-first search traversal of the tree. Finally, to build the concrete syntax tree, `Lexed` is interspered with `Output` to reinsert the whitespace tokens.

`Parser` benefits from consuming the `Input` rather than the `Lexed` as it simplifies peeking for the layout algorithm, as it only concerns itself with the tokens that make up the program. The event-driven `Output` makes it so that the parser is separate from the syntax tree actually being built.

### dot-dot (`..`)

The dot-dot (`..`) token is special in that it appears in two different contexts, namely with import/export lists and in expressions. The question is, should the lexer perform "gluing" and emit a `DotDot` token, or should it be a two separate `Period` tokens? In Rust, this is reminiscent of the `>>` operator clashing with its use in generic syntax:

```rs
let _ = Vec::<Vec<usize>>::new();

let _ = a >> b;
```

In this example, we simply cannot treat `>>` as a singular token as for the case of generic syntax, it's actually two distinct `>` at two different levels of nesting.

```rs
GENERIC_ARG_LIST@200..214
  COLON2@200..202 "::"
  L_ANGLE@202..203 "<"
  TYPE_ARG@203..213
    PATH_TYPE@203..213
      PATH@203..213
        PATH_SEGMENT@203..213
          NAME_REF@203..206
            IDENT@203..206 "Vec"
          GENERIC_ARG_LIST@206..213
            L_ANGLE@206..207 "<"
            TYPE_ARG@207..212
              PATH_TYPE@207..212
                PATH@207..212
                  PATH_SEGMENT@207..212
                    NAME_REF@207..212
                      IDENT@207..212 "usize"
            R_ANGLE@212..213 ">"  // HERE!
  R_ANGLE@213..214 ">"  // HERE!

BIN_EXPR@240..246
  PATH_EXPR@240..241
    PATH@240..241
      PATH_SEGMENT@240..241
        NAME_REF@240..241
          IDENT@240..241 "a"
  WHITESPACE@241..242 " "
  SHR@242..244 ">>"  // HERE!
  WHITESPACE@244..245 " "
  PATH_EXPR@245..246
    PATH@245..246
      PATH_SEGMENT@245..246
        NAME_REF@245..246
          IDENT@245..246 "b"
```

If it's the case that we perform gluing within the lexer, we have to add a special case in the expression parser that `DotDot` is a special case for an `Operator` token.

### Reparsing

In PureScript, precedence levels for operators are determined by fixity declarations, which means that an "association" step has to happen some time after obtaining fixity information for operators from name resolution. For instance, the following expression `1 + 2 * 3 / 4` is actually parsed into the CST as `1, [+ 2, * 3, / 4]`, where after name resolution, it can look something like `1, [Prelude.(+) 2, Prelude.(*) 3, Prelude.(/ 4)]`. The idea with reparsing is that initially, we can parse this into:

```rs
OperatorChain
  Literal
    Integer
  OperatorChainTail
    NameRef
      Operator
    NameRef
      Operator
```

Name resolution inserts qualification information:

```rs
OperatorChain
  Expression
  OperatorChainPair
    Qualified
      ModuleName
      NameRef
        Operator
    Expression
  OperatorChainPair
    Qualified
      ModuleName
      NameRef
        Operator
    Expression
```

Finally, this operator chain is flattened into:

```rs
Expression, Operator, Expression, Operator, Expression
```

Once fixity information is known, the parser can perform Pratt parsing to associate expressions.
