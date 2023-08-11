mod combinators;
mod rules;

use crate::parser::Parser;

use self::rules::{expr_0, pat_0, type_0};

pub fn expression(parser: &mut Parser) {
    expr_0(parser);
}

pub fn ty(parser: &mut Parser) {
    type_0(parser);
}

pub fn pattern(parser: &mut Parser) {
    pat_0(parser);
}
