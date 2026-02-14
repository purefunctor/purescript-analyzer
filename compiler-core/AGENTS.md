## Architecture

The compiler core is split into components with well-defined responsibilities, designed for
transparency (editor introspection) and compatibility with query-based incremental builds.

### Pipeline Components

The component names listed below are crate names in this workspace.

- **lexing**: tokenization and the layout algorithm
- **parsing**: parsing into a rowan-based CST
- **syntax**: types for the rowan-based CST
- **sugar**: syntax desugaring (e.g., operator bracketing)
- **lowering**: core semantic representation, name resolution
- **indexing**: high-level relationships between module items
- **resolving**: name-indexed interface for module items
- **stabilizing**: assigns stable IDs to source ranges
- **checking**: type checking and elaboration
- **diagnostics**: error collection and rendering for LSP and tests

### Infrastructure

- **building**: query-based parallel build system
- **building-types**: shared type definitions
- **files**: virtual file system
- **interner**: generic interner implementation
- **prim-constants**: primitive type constants

## Key Concepts

- Uses rust-analyzer/rowan, a lossless syntax tree library inspired by Swift's libsyntax
- Query-based incremental builds (not traditional phase-based)
- Interning and arena allocation enable better caching (e.g., whitespace changes don't invalidate type checking)
