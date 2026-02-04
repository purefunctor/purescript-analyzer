# Compiler Scripts Command Reference

CLI tools in `compiler-scripts/` for running integration tests.

## Test Runner Commands

### Run tests

```bash
just t <category> [filters...]         # Run tests (summary output)
just t <category> --diff [filters...]  # Run with full inline diffs
just t <category> --count 10 [filters...] # Show more snapshots (default: 3)
just t <category> --debug [filters...] # Enable tracing
just t <category> --verbose [filters...] # Show test progress
just t <category> --create "name"  # Scaffold a new fixture
just t <category> --delete "name"  # Dry-run fixture deletion (use --confirm)
```

### Categories

| Category | Alias | Description |
|----------|-------|-------------|
| checking | c | Type checker tests |
| lowering | l | Lowering tests |
| resolving | r | Resolver tests |
| lsp | - | LSP tests |

### Snapshot commands

```bash
just t <category> accept [--all] [filters...]  # Accept pending snapshots
just t <category> reject [--all] [filters...]  # Reject pending snapshots
```

Requires `--all` flag when no filters provided (safety guardrail).

### Exclusion filters

Hide snapshots ephemerally during a session:

```bash
just t <category> --exclude "pattern"              # Single exclusion
just t <category> --exclude "foo" --exclude "bar"  # Multiple exclusions
EXCLUDE_SNAPSHOTS="foo,bar" just t <category>      # Via environment variable
```

### Filters

Space-delimited, passed through to nextest. Mix numbers and patterns:

```bash
just t c 101 102           # Run tests 101 and 102
just t c pattern           # Filter by name pattern
just t c 101 102 pattern   # Numbers + pattern together
```

## Debugging Traces

When `--debug` is used, traces are written to `target/compiler-tracing/{test_id}_{module}.jsonl`.

```bash
wc -l target/compiler-tracing/*.jsonl              # Check sizes
shuf -n 20 target/compiler-tracing/NNN_*.jsonl | jq .  # Sample
jq 'select(.level == "DEBUG")' file.jsonl          # Filter by level
jq 'select(.target | contains("unification"))' file.jsonl
```
