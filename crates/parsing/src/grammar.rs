//! Implements a recursive descent parser for PureScript.

mod combinators;
mod rules;

#[cfg(test)]
mod tests;

use crate::parser::Parser;

use self::rules::{expr_0, pat_0, type_0};

pub(crate) fn expression(parser: &mut Parser) {
    expr_0(parser);
}

pub(crate) fn ty(parser: &mut Parser) {
    type_0(parser);
}

pub(crate) fn pattern(parser: &mut Parser) {
    pat_0(parser);
}
