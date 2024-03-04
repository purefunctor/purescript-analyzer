//! Implements a recursive descent parser for PureScript.

mod combinators;
mod rules;

#[cfg(test)]
mod tests;

pub(crate) use rules::module;
