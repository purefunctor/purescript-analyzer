# purescript-analyzer

purescript-analyzer is a compiler frontend for the PureScript programming language, which aims to extract information pertinent to IDE tooling.

## Features

The project is still in a pre-alpha state, and IDE integration has not been implemented yet.

## Design

The following section details the internal design of the purescript-analyzer.

### Incremental Computation

purescript-analyzer's internal mechanics is inspired by rust-analyzer's design of using query-based incremental compilation using the [`salsa`](https://github.com/salsa-rs/salsa) framework. The analyzer is designed to accomodate for the volatile nature of information in editors. For instance, changes in source files are significantly more frequent, as in text is being added and deleted all of the time as a program is being built. In IDEs, it's expected for the availability of information to match this frequency. For instance, the IDE should be able to infer the type of a local binding as it's being typed, or it should be able to update completion candidates as names are added.

The rate of change in editors may be too fast for compilers that make use of multiple passes, in that for each change in the source file, the compiler would have to throw away all information that it has built for a previous version of the source file being edited. A query-based framework for a compiler allows information to be cached at multiple levels, like parsing, desugaring, or type checking. If an edit is not significant enough to change the semantic information contained within a source file, then the compiler would not have to recompute said information. For example, adding a documentation string may cause the compiler to recompute documentation information for a module, but it would not cause the entire module to be type checked again.

### Error Resilience

The analyzer is designed to be resilient to errors such as those introduced by invalid syntax, type errors, etc. to extract as much semantic information from the source file that's currently being edited. This allows the analyzer to gather and provide information like for instance, completion candidates or type-on-hover.

## Contributing

If you'd like to contribute to the project, the [issues](https://github.com/purefunctor/purescript-analyzer/issues) tab can help guide you towards stuff that needs to be implemented or fixed.
