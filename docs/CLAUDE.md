# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is the interactive documentation site for purescript-analyzer. It provides a browser-based playground for exploring PureScript type checking, CST parsing, and package loading via a WASM-compiled version of the compiler core.

## Commands

```bash
# Install dependencies
pnpm install

# Development (builds WASM + starts Vite dev server with HMR)
pnpm dev

# Production build (optimized WASM + minified JS)
pnpm build

# Type checking
pnpm typecheck

# Format code
pnpm format

# Preview production build
pnpm preview
```

## Architecture

```
User Input (MonacoEditor)
    ↓
App.tsx (state management)
    ↓
useDocsLib hook (Comlink worker proxy)
    ↓
worker/docs-lib.ts (Web Worker, isolated thread)
    ↓
WASM engine (src/wasm/src/lib.rs)
    ↓
Compiler Core crates (../compiler-core/*)
    ↓
Results → Panel components
```

**Key architectural decisions:**

- **Thread isolation**: All compiler operations run in a Web Worker via Comlink, preventing UI blocking
- **WASM integration**: The `src/wasm/` crate compiles to WebAssembly and exposes `parse()`, `check()`, `register_module()`, and `clear_packages()` functions
- **Package system**: Fetches from `packages.registry.purescript.org`, decompresses tar.gz with pako, caches in localStorage, resolves transitive dependencies topologically

## Key Directories

- `src/components/` - React UI components including Monaco editor integration
- `src/components/Editor/purescript.ts` - PureScript language registration for Monaco
- `src/hooks/` - Custom hooks (`useDocsLib` for WASM worker, `useDebounce`)
- `src/lib/packages/` - Package fetching, caching, and dependency resolution
- `src/worker/` - Comlink-exposed Web Worker that loads WASM
- `src/wasm/` - Rust crate that compiles to WASM, links all compiler-core crates

## Stack

- React 18 + TypeScript + Vite
- Tailwind CSS 4 with Catppuccin theme (Macchiato dark, Latte light)
- Monaco Editor for code editing
- Comlink for type-safe worker communication
- wasm-pack for WASM compilation

## WASM Crate

The `src/wasm/` directory contains a Rust crate that:
- Links to 13 compiler-core crates via relative paths
- Exposes functions via `wasm-bindgen`
- Uses `serde-wasm-bindgen` for JS interop
- Tracks performance timing via `web-sys::Performance`

Rebuild WASM manually: `cd src/wasm && wasm-pack build --target web`
