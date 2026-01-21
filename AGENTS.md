## Commands

```bash
cargo check -p <crate-name> --tests     # Type check a crate (always specify -p)
cargo nextest run -p <crate-name>       # Run all tests in a crate
cargo nextest run -p <crate-name> <test_name>  # Run single test
just tc                                  # Type checker integration tests
just tc 101                              # Run specific test (by filter)
just fix                                 # Apply clippy fixes and format
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
