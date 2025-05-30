_default:
    just --list

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
