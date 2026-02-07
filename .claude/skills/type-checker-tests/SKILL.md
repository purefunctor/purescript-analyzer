---
name: type-checker-tests
description: Add integration tests for type checker inference and checking functions
allowed-tools: Bash(mkdir:*)
---

# Type Checker Integration Tests

Use the command reference at `reference/compiler-scripts.md` for test runner syntax, snapshot workflows, filters, and trace debugging. The category is `checking`.

**Language:** Fixtures use PureScript syntax, not Haskell.

## Creating a Test

### 1. Create fixture directory

```bash
just t checking --create "descriptive name"
```

The CLI picks the next fixture number and creates the folder.

Tests are auto-discovered by `build.rs`.

### 2. Write Main.purs

**Standard pattern** - pair typed (checking) and untyped (inference) variants:

```purescript
module Main where

-- Checking mode: explicit signature constrains type checker
test :: Array Int -> Int
test [x] = x

-- Inference mode: type checker infers unconstrained
test' [x] = x
```

**Guidelines:**
- Test ONE specific behavior per fixture
- Name tests: `test`, `test'`, `test2`, `test2'`, etc.
- Include edge cases relevant to the behavior

### 3. Run and review

```bash
just t checking NNN MMM
```

## Multi-File Tests

For imports, re-exports, or cross-module behavior:

```
tests-integration/fixtures/checking/NNN_import_test/
├── Main.purs    # Test file (snapshot generated)
├── Lib.purs     # Supporting module
└── Main.snap    # Generated snapshot
```

**Lib.purs:**
```purescript
module Lib where

life :: Int
life = 42

data Maybe a = Just a | Nothing
```

**Main.purs:**
```purescript
module Main where

import Lib (life, Maybe(..))

test :: Maybe Int
test = Just life
```

- Module name must match filename
- Only `Main.purs` generates a snapshot

## Snapshot Structure

```
Terms
functionName :: InferredOrCheckedType
...

Types
TypeName :: Kind
...

Errors
ErrorKind { details } at [location]
```

## Acceptance Criteria

Before accepting, verify:

1. **Types are correct**
   - `test :: Array Int -> Int` - signature preserved
   - `test' :: forall t. Array t -> t` - polymorphism inferred

2. **No unexpected `???`**
   - `test :: ???` - STOP: inference failure
   - `CannotUnify { ??? -> ???, Int }` - OK in error tests

3. **Errors appear where expected**
   - Confirm error kind matches (`NoInstanceFound`, `CannotUnify`)
   - Verify location points to correct declaration

4. **Polymorphism is appropriate**
   - Type variables scoped correctly
   - Constraints propagate as expected

## Common Issues

| Symptom | Likely Cause |
|---------|--------------|
| `test :: ???` | Syntax error or undefined names |
| Unexpected monomorphism | Missing polymorphic context |
| Wrong error location | Check binder/expression placement |
| Missing types in snapshot | Module header or imports incorrect |
