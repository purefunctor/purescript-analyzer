# purescript-analyzer

purescript-analyzer is a compiler frontend for the PureScript programming language.

## Contributing

The project uses the following development dependencies, please install them through `cargo install` or your preferred package manager. The project assumes some level of familiarity with these tools, but we're always happy to help with any contribution related questions.
* [just](https://crates.io/crates/just)
* [cargo-llvm-cov](https://crates.io/crates/cargo-llvm-cov)
* [cargo-nextest](https://crates.io/crates/cargo-nextest)
* [cargo-insta](https://crates.io/crates/cargo-insta)
* [node](https://github.com/nodejs/node) and [pnpm](https://github.com/pnpm/pnpm) or [corepack](https://github.com/nodejs/corepack)

### Development

To get started, build the crates in `compiler-core` and `compiler-lsp`:
```bash
$ cargo build
```

When working on individual crates, we recommend running tests with the `-p` flag:
```bash
$ cargo nextest -p indexing
```

Likewise, we also have an integration test suite, `tests-integration`, to track changes in compiler behaviour using snapshots:
```bash
$ just integration
# It also accepts command line arguments for `cargo nextest run`
$ just integration --test resolving 001
```

We also have compatibility tests against the package set, `tests-package-set`, and it requires a little bit more setup:
```bash
$ cd tests-package-set && pnpm install && node download-registry.js && cd ..
$ cargo nextest run -p tests-package-set
```

There are `just` rules for generating and inspecting coverage information:
```bash
$ just coverage
$ just coverage-html
```
