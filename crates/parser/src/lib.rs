use std::ops::Range;

use lexer::Positions;
use rowan::{GreenNode, GreenNodeBuilder, GreenToken, NodeCache};
use winnow::{error::ParserError, token::take_while, Located, PResult, Parser, Stateful};

use syntax::SyntaxKind::{self, *};

#[derive(Debug)]
struct State<'s> {
    cache: NodeCache,
    positions: Positions<'s>,
}

type Input<'t, 's> = Stateful<Located<&'t [SyntaxKind]>, State<'s>>;

fn whitespace(input: &mut Input) -> PResult<()> {
    let _ = take_while(0.., WHITESPACE).parse_next(input)?;
    Ok(())
}

struct Text {
    kind: SyntaxKind,
}

impl<'t, 's, E> Parser<Input<'t, 's>, &'s str, E> for Text
where
    E: ParserError<Input<'t, 's>>,
{
    fn parse_next(&mut self, input: &mut Input<'t, 's>) -> PResult<&'s str, E> {
        let Range { start, .. } = self.kind.span().parse_next(input)?;
        Ok(input.state.positions.text(start))
    }
}

fn text(kind: SyntaxKind) -> Text {
    Text { kind }
}

fn module_header(input: &mut Input) -> PResult<()> {
    let module = text(MODULE).parse_next(input)?;

    let _ = GreenNode::new(MODULE.into(), vec![]);

    let mut builder = GreenNodeBuilder::with_cache(&mut input.state.cache);
    builder.token(MODULE.into(), module);

    let b = whitespace.parse_next(input)?;
    dbg!((module, b));
    Ok(())
}

#[test]
fn __() {
    let (tokens, positions) = lexer::tokenize("module Main where ");
    let mut input = Stateful {
        input: Located::new(tokens.as_slice()),
        state: State { cache: NodeCache::default(), positions },
    };
    dbg!(module_header.take().parse_next(&mut input).unwrap());
}

// Would it be easier if we traversed through green tokens instead? Potentially.
// For one, it would mean that we're only concerned about allocating memory for
// the green nodes instead which we'll already be doing anyways—the difference
// is that we're going to be allocating memory on each parse step for the children
// instead of in one go.
