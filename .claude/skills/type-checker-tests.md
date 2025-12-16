---
name: type-checker-tests
description: Add integration tests for type checker inference and checking functions
---

# Integration Test Writing Skill

Use this skill when adding new type checker functions or expanding behavior.

**Important:** Test fixtures are written for PureScript, not Haskell.

## Setup

1. Find next available test number: Check `tests-integration/fixtures/checking/` for highest NNN value
2. Create fixture directory: `tests-integration/fixtures/checking/{NNN_name}/`
3. Read existing fixture (e.g., `fixtures/checking/060_array_binder/Main.purs`) as reference for structure

## Write Main.purs Fixture

**Required pattern:**
- Write one `test :: Type -> Type` (checking mode with explicit signature)
- Write one `test'` (inference mode, no signature)
- Pair additional tests with their untyped variants (test2/test2', test3/test3')

**Focus:**
- Test one specific inference/checking behavior per fixture
- Use simple examples: pattern matching, binders, operators, recursion
- Include edge cases: empty arrays, nullary constructors, nested patterns

## Run and Accept Snapshot

1. Generate snapshots: `just pending-snapshots --test checking`
   - Runs tests with `INSTA_FORCE_PASS=1` to generate `.snap.new` files
   - Displays each pending snapshot for review

2. Review output carefully:
   - Verify types match expectations
   - Check error messages if applicable
   - Output shows file path and source location for reference
   - **⚠️ Critical: Check for `???` in snapshot output**
     - `???` indicates type inference failure (e.g., `test :: ???`)
     - This means either:
       - Bug in test code (invalid syntax, undefined values)
       - Compiler bug (incomplete inference implementation)
     - **Do NOT accept snapshots containing `???` without investigation**
   - **⚠️ Snapshot test success does not imply task completion**
   - **⚠️ Review snapshot contents against your success criteria**

3. Accept only if snapshots are valid: `cargo insta accept`
   - Commits all pending snapshots to `.snap` files

## Verify

- Run full suite: `just pending-snapshots --test checking`
- Confirm all tests pass (no `???` in output)
- Verify test appears in snapshots with correct output

## Debugging

When debugging a specific test or a potential compiler bug, use
`just pending-snapshots --test checking NNN` to focus on a single
test case. This is context efficient and allows you to focus on
a single problem. Once the user has confirmed that the bug has
been resolved, run the full.
