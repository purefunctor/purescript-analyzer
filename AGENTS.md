## Project Overview

purescript-analyzer is a compiler frontend for the PureScript programming language written
in Rust. It provides additional LSP server functionality for the language. The compiler is
organised as a Cargo workspace with functionality located in separate folders.

### Compiler Core

The compiler core is split into different core components in `./compiler-core`, with each
components having well-defined responsibilities. These components are designed to produce
information as transparent as possible, such that editor integrations can be built around
introspection. Likewise, these components are also designed to be compatible with a
query-based and incremental build system.

- checking: type checking and elaboration
- indexing: high-level relationships between module items
- lexing: tokenization and the layout algorithm
- lowering: core semantic representation, name resolution
- parsing: parsing into a rowan-based CST
- resolving: name-indexed interface for module items
- stabilizing: assigns stable IDs to source ranges
- sugar: syntax desugaring such as operator bracketing
- syntax: types for the rowan-based CST

Additionally, the following crates are related to the build system implementation.

- building: query-based parallel build system
- building-types: shared type definitions
- files: virtual file system
- interner: generic interner implementation

### LSP and Binary

- `./compiler-bin`: implements the `purescript-analyzer` executable
- `./compiler-lsp`: LSP server functionality used in `./compiler-bin`

## Key Concepts

Additional concepts that you should be mindful of, the compiler:
- uses rust-analyzer/rowan, a lossless syntax tree library inspired by Swift's libsyntax
- uses a query-based incremental build system rather than a traditional phase-based setup
- uses techniques like interning and arena allocation to enable better caching patterns
  - for instance, whitespace does not immediately invalidate type checking results

## Niche Guides

Load these guides from `agents/guides/` when working on related tasks:
- `build-system.md` - working with the build system
- `integration-tests.md` - working with fixtures and snapshots

## Communication Guidelines

When working on this project, prefer succinct and direct responses. Keep output tokens
minimal to reduce costs while using the default thinking budget to maintain reasoning.

**Key principles:**
- Provide brief, direct answers to queries
- Only elaborate when explicitly asked
- Keep output focused on what's necessary
- Avoid verbose explanations unless requested
- Do not write or edit code until permitted to do so

## Development Commands

`cargo check` to verify what you've written is correct

```bash
# Check all crates
cargo check

# Check a crate
cargo check -p indexing
```

`cargo nextest` to run tests blazingly fast

```bash
# Run unit tests for a specific crate
cargo nextest run -p indexing

# Run integration tests
just integration

# Run integration tests with filters
just integration --test resolving 001
```

`cargo clippy` and `cargo fmt` for code quality

```bash
# Get diagnostics from clippy
cargo clippy --workspace

# Format with stable and nightly
cargo fmt && just format-imports
```

### Testing Loop

This project uses `cargo-insta` for snapshot tests. Fresh snapshots are prefixed with `.snap.new`,
make sure to review these if a test run produces them. Use `cargo insta accept` once you're satisfied
with the output.
