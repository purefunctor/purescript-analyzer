## Commands

```bash
cargo check -p <crate-name> --tests  # Type check a crate (always specify -p)
just t checking [filters...]         # Type checker integration tests
just t lowering [filters...]         # Lowering integration tests
just t resolving [filters...]        # Resolver integration tests
just t lsp [filters...]              # LSP integration tests
just fix                             # Apply clippy fixes and format
```

For unit tests in compiler-core (not tests-integration which requires the test runner shim):
```bash
cargo nextest run -p <crate-name>              # Run all tests in a crate
cargo nextest run -p <crate-name> <test_name>  # Run single test
```

## Architecture

PureScript compiler frontend in Rust using rowan (lossless syntax trees) and query-based incremental builds.

**compiler-core/**: checking (types), indexing, lexing, lowering, parsing, resolving, stabilizing, sugar, syntax, building, files, interner
**compiler-bin/**: CLI executable | **compiler-lsp/**: LSP server

## Code Style

- Use `cargo fmt` (rustfmt with `use_small_heuristics = "Max"`)
- Use `just format-imports` for module-granularity imports (requires nightly)
- Leverage interning/arena allocation for caching; avoid unnecessary allocations

## Skills

Load `.claude/skills/type-checker-tests` when implementing type checker tests.
