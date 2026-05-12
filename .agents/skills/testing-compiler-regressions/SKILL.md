---
name: testing-compiler-regressions
description: "Splits a known compiler bug fix into auditable jj history. Use when the current or parent commit already contains a likely type checker/compiler fix and needs a preceding failing fixture commit plus a fix commit that updates the same snapshot."
---

# Testing Compiler Regressions

Use this when the fix is already known and the working copy, current commit, or parent commit contains the bug fix.

The goal is an auditable two-commit history:

1. **Failing fixture commit**: add the fixture and accept a snapshot that captures the current undesirable behavior.
2. **Fix commit**: keep the compiler fix and update the same snapshot so the undesirable behavior disappears or changes to the desired output.

This makes the regression visible in version control before the fix removes it.

## Historical pattern

Historical examples in this repo follow this shape:

- `Add failing test case for bare row tail syntax` → `Fix inference for bare row tails in syntax`
- `Add failing test case for constrained pattern scrutinee` → `Fix constrained pattern scrutinee checking`
- `Add failing test case for open row matching` → `Fix open row tail instance matching`

The fix commit usually touches compiler code plus the same `.snap`; the snapshot diff often removes a `Diagnostics` block or replaces an incorrect inferred result.

## Workflow

### 1. Preserve the existing fix commit

If the current commit contains the intended fix, give it a clear description first:

```bash
jj describe -m "Fix <bug>"
```

Then insert a parent commit before it for the failing fixture:

```bash
jj new -B @ -m "Add failing test case for <bug>"
```

Use the existing commit message style when a more specific noun reads better, such as `Add <bug> regression fixture`.

### 2. Add the regression fixture

Create a checking fixture with a descriptive name:

```bash
just t checking --create "<descriptive name>"
```

Write a focused PureScript `Main.purs` that reproduces one behavior.

Accept the snapshot in the failing fixture commit:

```bash
just t checking NNN --accept
```

The snapshot should intentionally encode the bug: the wrong error, missing type, bad constraint, unexpected `???`, or other undesirable current behavior. Do not try to make this commit green by changing compiler behavior; its purpose is to record the failure before the fix.

### 3. Return to the fix commit and update the snapshot

Move back to the child commit containing the fix:

```bash
jj edit <fix-change-id>
```

Run the same fixture and inspect the snapshot change:

```bash
just t checking NNN --diff
```

Accept the updated snapshot:

```bash
just t checking NNN --accept
```

The diff should show the undesirable behavior being removed or replaced by the correct behavior. This is commonly a deleted `Diagnostics` section or a targeted type/constraint change in the same snapshot added by the previous commit.

## Snapshot review checklist

Before finishing, verify:

- The failing fixture commit snapshot captures the bug clearly.
- The fix commit snapshot changes only what the fix should change.
- Unexpected `???` does not appear unless it is the regression being documented.
- Error kinds, locations, inferred types, and constraints are intentional.
- The fixture is narrow enough for the snapshot diff to be easy to audit.

## Useful commands

```bash
just t checking NNN           # Run fixture
just t checking NNN --diff    # Inspect snapshot diff
just t checking NNN --accept  # Accept fixture snapshot
just t checking NNN --reject  # Reject fixture snapshot
```
