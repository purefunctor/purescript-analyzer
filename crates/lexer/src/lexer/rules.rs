use std::ops::Range;

use memchr::memmem;
use unicode_categories::UnicodeCategories;
use winnow::{
    combinator::{alt, eof, fail, opt, repeat, repeat_till},
    error::{AddContext, ContextError, ErrMode, StrContext},
    stream::Stream,
    token::{any, one_of, take_until, take_while},
    PResult, Parser,
};

use crate::syntax::SyntaxKind;

use super::{Input, Lexed};

fn identifier<'s>(input: &mut Input<'s>) -> PResult<&'s str> {
    let h = any.verify(|c: &char| *c == '_' || c.is_letter_lowercase());
    let t = take_while(0.., |c: char| c == '_' || c == '\'' || c.is_letter() || c.is_ascii_digit());
    (h, t).take().parse_next(input)
}

fn proper<'s>(input: &mut Input<'s>) -> PResult<&'s str> {
    let h = any.verify(|c: &char| c.is_letter_uppercase());
    let t = take_while(0.., |c: char| c == '_' || c == '\'' || c.is_letter() || c.is_ascii_digit());
    (h, t).take().parse_next(input)
}

fn hole(input: &mut Input<'_>) -> PResult<Lexed> {
    let range = ('?', alt((identifier, proper))).span().parse_next(input)?;
    Ok(Lexed::token(SyntaxKind::SOURCE_HOLE, range))
}

fn lower(input: &mut Input<'_>) -> PResult<SyntaxKind> {
    let text = identifier.parse_next(input)?;
    let kind = match text {
        "_" => SyntaxKind::UNDERSCORE,
        "ado" => SyntaxKind::ADO,
        "as" => SyntaxKind::AS,
        "case" => SyntaxKind::CASE,
        "class" => SyntaxKind::CLASS,
        "data" => SyntaxKind::DATA,
        "derive" => SyntaxKind::DERIVE,
        "do" => SyntaxKind::DO,
        "else" => SyntaxKind::ELSE,
        "false" => SyntaxKind::FALSE,
        "forall" => SyntaxKind::FORALL,
        "foreign" => SyntaxKind::FOREIGN,
        "hiding" => SyntaxKind::HIDING,
        "if" => SyntaxKind::IF,
        "import" => SyntaxKind::IMPORT,
        "in" => SyntaxKind::IN,
        "infix" => SyntaxKind::INFIX,
        "infixl" => SyntaxKind::INFIXL,
        "infixr" => SyntaxKind::INFIXR,
        "instance" => SyntaxKind::INSTANCE,
        "let" => SyntaxKind::LET,
        "module" => SyntaxKind::MODULE,
        "newtype" => SyntaxKind::NEWTYPE,
        "of" => SyntaxKind::OF,
        "then" => SyntaxKind::THEN,
        "true" => SyntaxKind::TRUE,
        "type" => SyntaxKind::TYPE,
        "where" => SyntaxKind::WHERE,
        _ => SyntaxKind::SOURCE_IDENTIFIER,
    };
    Ok(kind)
}

fn upper(input: &mut Input<'_>) -> PResult<SyntaxKind> {
    let _ = proper.parse_next(input)?;
    Ok(SyntaxKind::SOURCE_PROPER)
}

fn symbol_identifier<'s>(input: &mut Input<'s>) -> PResult<&'s str> {
    let is_ascii = |c: char| ":!#$%&*+./<=>?@\\^|~-".contains(c);
    let is_unicode = |c: char| !c.is_punctuation() && c.is_symbol();
    let is_symbol = |c: char| is_ascii(c) || is_unicode(c);
    let text = take_while(1.., is_symbol).parse_next(input)?;
    Ok(text)
}

fn operator(input: &mut Input<'_>) -> PResult<SyntaxKind> {
    let text = symbol_identifier.parse_next(input)?;
    let kind = match text {
        "<-" | "←" => SyntaxKind::LEFT_ARROW,
        "<=" | "⇐" => SyntaxKind::LEFT_DOUBLE_ARROW,
        "->" | "→" => SyntaxKind::RIGHT_ARROW,
        "=>" | "⇒" => SyntaxKind::RIGHT_DOUBLE_ARROW,
        "::" | "∷" => SyntaxKind::DOUBLE_COLON,
        "∀" => SyntaxKind::FORALL,
        "=" => SyntaxKind::EQUALS,
        "." => SyntaxKind::DOT,
        "\\" => SyntaxKind::BACKSLASH,
        "|" => SyntaxKind::PIPE,
        "@" => SyntaxKind::AT,
        "`" => SyntaxKind::TICK,
        _ => SyntaxKind::SOURCE_OPERATOR,
    };
    Ok(kind)
}

fn symbol(input: &mut Input<'_>) -> PResult<SyntaxKind> {
    let _ = ('(', symbol_identifier, ')').parse_next(input)?;
    Ok(SyntaxKind::SOURCE_SYMBOL)
}

fn name(input: &mut Input<'_>) -> PResult<SyntaxKind> {
    alt((lower, upper, symbol, operator)).parse_next(input)
}

fn prefix(input: &mut Input<'_>) -> PResult<SyntaxKind> {
    let module_name = repeat(1.., (proper, '.')).map(|()| ());
    module_name.map(|_| SyntaxKind::MODULE_NAME).parse_next(input)
}

fn qualified(input: &mut Input<'_>) -> PResult<Lexed> {
    let prefix = opt(prefix.with_span()).parse_next(input)?;
    let (kind, range) = name.with_span().parse_next(input)?;
    Ok(Lexed::qualified(prefix, kind, range))
}

fn is_escape(c: char) -> bool {
    matches!(c, 't' | 'r' | 'n' | '"' | '\'' | '\\')
}

fn string(input: &mut Input<'_>) -> PResult<Lexed> {
    let Range { start, .. } = '"'.span().parse_next(input)?;
    let Range { end, .. } = loop {
        let (next, range) = any.with_span().parse_next(input)?;
        match next {
            '"' => {
                break range;
            }
            '\\' => {
                let next = any.parse_next(input)?;
                match next {
                    _ if is_escape(next) => {
                        continue;
                    }
                    ' ' | '\r' | '\n' => {
                        (take_until(0.., '\\'), '\\').parse_next(input)?;
                    }
                    'x' => {
                        take_while(1..=6, |c: char| c.is_ascii_hexdigit()).parse_next(input)?;
                    }
                    _ => {
                        fail.parse_next(input)?;
                    }
                }
            }
            _ => {
                take_until(0.., ('"', '\\')).map(|_| ()).parse_next(input)?;
            }
        }
    };
    Ok(Lexed::token(SyntaxKind::STRING, start..end))
}

fn raw_string(input: &mut Input<'_>) -> PResult<Lexed> {
    let checkpoint = input.checkpoint();

    let Range { start, .. } = "\"\"\"".span().parse_next(input)?;
    let Range { end, .. } = loop {
        take_while(0.., |c| c != '"').parse_next(input)?;
        let (quotes, range) = take_while(..=5, |c| c == '"').with_span().parse_next(input)?;

        match quotes.len() {
            0 => {
                return Err(ErrMode::Cut(ContextError::new().add_context(
                    input,
                    &checkpoint,
                    StrContext::Label("incomplete raw string"),
                )))
            }
            // e.g.
            // * """ " """  - single at the middle
            // * """ "" """ - double at the middle
            1 | 2 => continue,
            // e.g.
            // * """ """   - found the end three
            // * """ """"  - single at the end
            // * """ """"" - double at the end
            3..=5 => break range,
            _ => unreachable!(),
        }
    };

    Ok(Lexed::token(SyntaxKind::RAW_STRING, start..end))
}

fn character(input: &mut Input<'_>) -> PResult<Lexed> {
    let checkpoint = input.checkpoint();

    let Range { start, .. } = '\''.span().parse_next(input)?;

    let character = any.parse_next(input)?;
    if character == '\\' {
        let next = any.parse_next(input)?;
        if next == 'x' {
            take_while(1..=6, |c: char| c.is_ascii_hexdigit()).parse_next(input)?;
        } else if !is_escape(next) {
            return Err(ErrMode::Cut(ContextError::new().add_context(
                input,
                &checkpoint,
                StrContext::Label("invalid escape character"),
            )));
        }
    } else if character == '\'' {
        return Err(ErrMode::Cut(ContextError::new().add_context(
            input,
            &checkpoint,
            StrContext::Label("empty character"),
        )));
    }

    let Range { end, .. } = '\''.span().parse_next(input)?;

    Ok(Lexed::token(SyntaxKind::CHAR, start..end))
}

fn int_part<'s>(input: &mut Input<'s>) -> PResult<&'s str> {
    alt(('0'.take(), (one_of('1'..='9'), take_while(0.., ('_', '0'..='9'))).take()))
        .parse_next(input)
}

fn fraction_part<'s>(input: &mut Input<'s>) -> PResult<&'s str> {
    take_while(1.., ('_', '0'..='9')).parse_next(input)
}

fn exponent_part<'s>(input: &mut Input<'s>) -> PResult<&'s str> {
    (opt(alt(('+', '-'))), int_part).take().parse_next(input)
}

fn number(input: &mut Input<'_>) -> PResult<Lexed> {
    let integer = int_part.span().parse_next(input)?;
    let fraction = opt(('.', fraction_part).span()).parse_next(input)?;
    let exponent = opt(('e', exponent_part).span()).parse_next(input)?;

    let (kind, range) = match (fraction, exponent) {
        (None, None) => (SyntaxKind::INTEGER, integer),
        (Some(fraction), None) => (SyntaxKind::NUMBER, integer.start..fraction.end),
        (None, Some(exponent)) => (SyntaxKind::NUMBER, integer.start..exponent.end),
        (Some(_), Some(exponent)) => (SyntaxKind::NUMBER, integer.start..exponent.end),
    };

    Ok(Lexed::token(kind, range))
}

fn bracket(input: &mut Input<'_>) -> PResult<Lexed> {
    let (kind, range) = alt((
        '('.value(SyntaxKind::LEFT_PARENTHESIS),
        ')'.value(SyntaxKind::RIGHT_PARENTHESIS),
        '['.value(SyntaxKind::LEFT_SQUARE),
        ']'.value(SyntaxKind::RIGHT_SQUARE),
        '{'.value(SyntaxKind::LEFT_CURLY),
        '}'.value(SyntaxKind::RIGHT_CURLY),
    ))
    .with_span()
    .parse_next(input)?;
    Ok(Lexed::token(kind, range))
}

fn end_of_file(input: &mut Input<'_>) -> PResult<Lexed> {
    let range = eof.span().parse_next(input)?;
    Ok(Lexed::token(SyntaxKind::END_OF_FILE, range))
}

fn whitespace(input: &mut Input<'_>) -> PResult<Lexed> {
    let range = take_while(1.., char::is_whitespace).span().parse_next(input)?;
    Ok(Lexed::token(SyntaxKind::WHITESPACE, range))
}

fn line_comment(input: &mut Input<'_>) -> PResult<Lexed> {
    let is_content = |c: char| c != '\r' && c != '\n';
    let range = ("--", take_while(0.., is_content)).span().parse_next(input)?;
    Ok(Lexed::token(SyntaxKind::LINE_COMMENT, range))
}

fn block_comment(input: &mut Input<'_>) -> PResult<Lexed> {
    let Range { start, .. } = "{-".span().parse_next(input)?;
    let Range { end, .. } = loop {
        let content = take_until(0.., "-}").parse_next(input)?;
        let end = "-}".span().parse_next(input)?;
        if memmem::find_iter(content.as_bytes(), "{-").count() == 0 {
            break end;
        }
    };
    Ok(Lexed::token(SyntaxKind::BLOCK_COMMENT, start..end))
}

// TODO: determine if `dispatch!` is an optimisation.
fn token(input: &mut Input<'_>) -> PResult<Lexed> {
    alt((
        whitespace,
        line_comment,
        block_comment,
        hole,
        qualified,
        number,
        character,
        raw_string,
        string,
        bracket,
    ))
    .parse_next(input)
}

pub(super) fn tokens(input: &mut Input<'_>) -> PResult<Vec<Lexed>> {
    let (mut tokens, token): (Vec<Lexed>, Lexed) =
        repeat_till(0.., token, end_of_file).parse_next(input)?;

    tokens.push(token);

    Ok(tokens)
}
