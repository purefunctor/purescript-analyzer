_default:
    just --list

[doc('Run tests')]
test:
    cargo nextest run

[doc('Run tests and review')]
test-s:
    cargo insta test --test-runner nextest --review

[doc('Run tests with coverage')]
test-c:
    cargo llvm-cov nextest --html

[doc('Run tests against package set')]
test-p:
    cargo nextest run -p tests-package-set --release

[doc("Generate documentation")]
@doc flag="":
    cargo doc --document-private-items {{flag}}
