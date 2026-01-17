---
name: type-checker-tests
description: Add integration tests for type checker inference and checking functions
allowed-tools: Bash(mkdir:*)
---

# Type Checker Integration Tests

Use this skill when adding new type checker functions or expanding behavior.

**Language:** Test fixtures use PureScript syntax, not Haskell.

## Quick Reference

| Action | Command |
|--------|---------|
| Find next test number | `ls tests-integration/fixtures/checking/ \| tail -5` |
| Run a test or multiple tests | `just tc NNN` or `just tc 101 102` |
| Run with tracing enabled | `just tc --debug NNN` |
| Run all checking tests | `just tc` |
| Accept all pending snapshots | `cargo insta accept` |

Use `just tc --help` for all options.

## Creating a Test

### 1. Create fixture directory

```bash
mkdir tests-integration/fixtures/checking/{NNN_descriptive_name}
```

Tests are auto-discovered by `build.rs` - no manual registration needed.

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
- Name tests descriptively: `test`, `test'`, `test2`, `test2'`, etc.
- Include edge cases relevant to the behavior being tested

### 3. Generate and review snapshot

```bash
just tc NNN
```

This outputs:
- `CREATED path` (green) with numbered lines showing full content
- `UPDATED path` (yellow) with chunked diff (2 lines context, line numbers)

## Multi-File Tests

For testing imports, re-exports, or cross-module behavior, add multiple `.purs` files
to the same fixture directory. The type checker loads all `.purs` files in the folder.

**Example structure:**
```
tests-integration/fixtures/checking/NNN_import_test/
├── Main.purs    # The test file (snapshot generated for Main)
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

**Key points:**
- Module name must match filename (`Lib.purs` -> `module Lib where`)
- Only `Main.purs` generates a snapshot (the test runs against `Main`)
- Use standard PureScript import syntax

## Reviewing Snapshots

Snapshots have this structure:

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

### Acceptance Criteria

**Before accepting, verify:**

1. **Types are correct** - Check that inferred types match expectations
   - `test :: Array Int -> Int` - explicit signature preserved
   - `test' :: forall t. Array t -> t` - polymorphism inferred correctly

2. **No unexpected `???`** - This indicates inference failure
   - `test :: ???` - STOP: the term failed to type check
   - `CannotUnify { ??? -> ???, Int }` - OK in error tests, shows unresolved unification variables

3. **Errors appear where expected** - For tests validating error behavior
   - Confirm error kind matches expectations (e.g., `NoInstanceFound`, `CannotUnify`)
   - Verify error location points to the correct declaration

4. **Polymorphism is appropriate**
   - Check type variable names (`t6`, `a`, etc.) are scoped correctly
   - Verify constraints propagate as expected

### Common Issues

| Symptom | Likely Cause |
|---------|--------------|
| `test :: ???` | Test code has syntax error or uses undefined names |
| Unexpected monomorphism | Missing polymorphic context or over-constrained signature |
| Wrong error location | Check binder/expression placement in source |
| Missing types in snapshot | Module header or imports incorrect |

## Accept and Verify

```bash
# Accept only after thorough review
cargo insta accept

# Verify all checking tests pass
just tc
```

## Debugging

When investigating a potential compiler bug:

```bash
# Focus on single test to reduce noise
just tc NNN

# Enable tracing to see type checker behaviour
just tc --debug NNN
```

### Trace Files

The `--debug` flag emits detailed type checker traces to `target/compiler-tracing/`.

**Trace file naming:** `{test_id}_{module_name}.jsonl`
- Example: `200_int_compare_transitive_Main.jsonl`

**Output format:** JSON Lines (one JSON object per line), containing:
- `timestamp` - when the event occurred
- `level` - DEBUG, INFO, or TRACE
- `fields` - trace data (e.g., types being unified)
- `target` - the module emitting the trace (e.g., `checking::algorithm::unification`)
- `span`/`spans` - current span and span stack

**Example trace line:**
```json
{"timestamp":"...","level":"DEBUG","fields":{"t1":"?0","t2":"Int"},"target":"checking::algorithm::unification","span":{"name":"unify"}}
```

When `--debug` is used, the trace file path is shown alongside pending snapshots:
```
UPDATED tests-integration/fixtures/checking/200_int_compare_transitive/Main.snap
  TRACE target/compiler-tracing/200_int_compare_transitive_Main.jsonl
```

### Analysing Traces

Trace files can be large for complex tests. Use sampling and filtering:

```bash
# Check file size and line count
wc -l target/compiler-tracing/NNN_*.jsonl

# Sample random lines to get an overview
shuf -n 20 target/compiler-tracing/NNN_*.jsonl | jq .

# Filter by level
jq 'select(.level == "DEBUG")' target/compiler-tracing/NNN_*.jsonl

# Filter by target module
jq 'select(.target | contains("unification"))' target/compiler-tracing/NNN_*.jsonl

# Extract specific fields
jq '{level, target, fields}' target/compiler-tracing/NNN_*.jsonl
```

You should run `just tc` to check for regressions.
