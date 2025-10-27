_default:
    just --list

set positional-arguments

[doc("Generate coverage for local tests")]
coverage:
  cargo llvm-cov clean --workspace
  cargo llvm-cov nextest --no-report
  cargo llvm-cov nextest --no-report -p tests-integration

[doc("Generate coverage with the package set")]
coverage-full: coverage
  cargo llvm-cov nextest --no-report -p tests-package-set

[doc("Generate coverage report for Codecov")]
coverage-codecov:
  cargo llvm-cov report --codecov --output-path codecov.json

[doc("Generate coverage report as HTML")]
coverage-html:
  cargo llvm-cov report --html

@integration *args="":
  cargo nextest run -p tests-integration "$@"

[doc("Apply clippy fixes and format")]
fix:
  cargo clippy --workspace --fix && cargo fmt

[doc("Update THIRDPARTY.toml")]
[working-directory: 'compiler-bin']
licenses:
  cargo bundle-licenses --prefer MIT -o ../THIRDPARTY.toml

[doc("Format imports with module granularity")]
format-imports:
  cargo +nightly fmt -- --config imports_granularity=Module
