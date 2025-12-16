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

## Skills

Agent skills are specialized instruction sets for common tasks. They're stored in `.claude/skills/`.

## Commands by Occasion

Occasionally run `cargo check -p <crate-name>` to verify that code compiles.
