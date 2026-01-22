---
name: incremental-testing
description: SINGLE SOURCE OF TRUTH for running compiler integration tests + snapshots. Use for checking/lowering/resolving/lsp. Follow exactly; do not discover tests via filesystem.
---

# Compiler Integration Testing (AUTHORITATIVE)

Make sure to load the compiler-scripts skill for command syntax and flags.

## NON-NEGOTIABLE RULES (must follow)

1. You MUST run compiler integration tests ONLY via:
   `just t <category> [filters...]`

2. You MUST NOT perform test discovery by inspecting the filesystem.
   Specifically, you MUST NOT `glob`, `find`, or `rg` under:
   `tests-integration/fixtures/**`

3. Filters MUST come ONLY from:
   - The user's explicit test id/name/pattern, OR
   - The CLI output from a previous `just t ...` run.
   Filters MUST NOT come from fixture directory exploration.

4. STOP CONDITION: If you already know `<category>` and `[filters...]`,
   run the `just t ...` command now. Do not gather more context.

## Decision procedure (do exactly this)

1. Identify category: one of `c` (checking), `l` (lowering), `r` (resolving), `lsp`.
2. If the user provided any ids/patterns → pass them as space-delimited filters.
3. Else → run the category with no filters.
4. If results are too broad → rerun using ids/patterns copied from CLI output.

## Quick Examples

```bash
just t c                  # Run all checking tests
just t c 101              # Run test 101 in checking
just t c 101 102 pattern  # Multiple filters
just t c accept --all     # Accept all pending checking snapshots
```

## Forbidden (never do this)

- `rg tests-integration/fixtures`
- `find tests-integration/fixtures`
- `glob tests-integration/fixtures/**`
- "Open fixture files to see what tests exist"
- "List fixture directories to decide which test to run"
- Any form of filesystem exploration to discover tests
