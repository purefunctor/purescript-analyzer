//! Text-based lexer into a sequence of [`SyntaxKind`]s.
//!
//! Tokens are based off of the compiler's [`lexer`], while the lexer
//! core borrows from [`rustc_lexer`] and [`rust-analyzer`].
//!
//! [`lexer`]: https://github.com/purescript/purescript/blob/master/src/Language/PureScript/CST/Lexer.hs
//! [`rustc_lexer`]: https://doc.rust-lang.org/stable/nightly-rustc/rustc_lexer/
//! [`rust-analyzer`]: https://github.com/rust-lang/rust-analyzer/

use std::{ops::Range, str::Chars};

use syntax::SyntaxKind;
use unicode_categories::UnicodeCategories;

use crate::position::Position;

const EOF_CHAR: char = '\0';

/// A sequence of [`SyntaxKind`]s.
pub struct Lexed<'a> {
    source: &'a str,
    kinds: Vec<SyntaxKind>,
    positions: Vec<Position>,
    errors: Vec<LexError>,
}

#[derive(Debug)]
#[allow(dead_code)]
struct LexError {
    message: String,
    index: u32,
}

impl<'a> Lexed<'a> {
    fn new(source: &'a str) -> Lexed<'a> {
        let kinds = vec![];
        let positions = vec![];
        let errors = vec![];
        Lexed { source, kinds, positions, errors }
    }

    fn push(&mut self, kind: SyntaxKind, position: Position, error: Option<&str>) {
        self.kinds.push(kind);
        self.positions.push(position);

        if let Some(error) = error {
            let message = error.to_string();
            let index = self.len() as u32;
            self.errors.push(LexError { message, index });
        }
    }

    /// # Invariant
    ///
    /// [`Lexed`] always contains [`SyntaxKind::EndOfFile`] as its final element.
    /// It exclusively serves as an anchor for the final offset such that methods
    /// can compute the range for a token in a given index. With this in mind, we
    /// make sure that this token is hidden.
    ///
    /// For example, given `"hello"`
    ///
    /// ```rs
    /// // with EndOfFile, 0..5
    /// [(Lower, 0), (EndOfFile, 5)]
    ///
    /// // without EndOfFile, 0..?
    /// [(Lower, 0)]
    /// ```
    pub fn len(&self) -> usize {
        self.kinds.len() - 1
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the kind for an index.
    pub fn kind(&self, index: usize) -> SyntaxKind {
        assert!(index < self.len());
        self.kinds[index]
    }

    /// Returns the position for an index.
    pub fn position(&self, index: usize) -> Position {
        assert!(index < self.len());
        self.positions[index]
    }

    /// Returns the text for an index.
    pub fn text(&self, index: usize) -> &str {
        self.text_in_range(index..index + 1)
    }

    /// Returns the text for a range.
    pub fn text_in_range(&self, range: Range<usize>) -> &str {
        assert!(range.start < range.end && range.end <= self.len());
        let low = self.positions[range.start].offset;
        let high = self.positions[range.end].offset;
        &self.source[low..high]
    }
}

/// Character-based lexer, inspired by `rustc_lexer`.
struct Lexer<'a> {
    source: &'a str,
    chars: Chars<'a>,
    line: u32,
    column: u32,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Lexer<'a> {
        let chars = source.chars();
        let line = 1;
        let column = 1;
        Lexer { source, chars, line, column }
    }

    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    fn consumed(&self) -> usize {
        self.source.len() - self.chars.as_str().len()
    }

    fn position(&self) -> Position {
        let offset = self.consumed();
        let line = self.line;
        let column = self.column;
        Position { offset, line, column }
    }

    fn first(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    fn second(&self) -> char {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next().unwrap_or(EOF_CHAR)
    }

    fn take(&mut self) -> char {
        let result = self.chars.next().unwrap_or(EOF_CHAR);
        if result == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        result
    }

    fn take_while(&mut self, predicate: impl Fn(char) -> bool) {
        while predicate(self.first()) && !self.is_eof() {
            self.take();
        }
    }

    fn take_while_max(&mut self, predicate: impl Fn(char) -> bool, max: u8) {
        for _ in 0..max {
            if predicate(self.first()) && !self.is_eof() {
                self.take();
            } else {
                return;
            }
        }
    }
}

type Token = (SyntaxKind, Position, Option<&'static str>);

impl<'a> Lexer<'a> {
    fn take_token(&mut self) -> Token {
        match self.first() {
            '-' if self.second() == '-' => self.take_line_comment(),
            '{' if self.second() == '-' => self.take_block_comment(),

            '(' => self.take_single(SyntaxKind::LeftParenthesis),
            ')' => self.take_single(SyntaxKind::RightParenthesis),
            '{' => self.take_single(SyntaxKind::LeftBracket),
            '}' => self.take_single(SyntaxKind::RightBracket),
            '[' => self.take_single(SyntaxKind::LeftSquare),
            ']' => self.take_single(SyntaxKind::RightSquare),

            '`' => self.take_single(SyntaxKind::Tick),
            ',' => self.take_single(SyntaxKind::Comma),

            '\'' => self.take_char(),
            '"' => self.take_string(),

            '?' if is_ident(self.second()) => self.take_hole(),

            identifier => {
                if is_ident_start(identifier) {
                    self.take_lower()
                } else if identifier.is_letter_uppercase() {
                    self.take_upper()
                } else if is_operator(identifier) {
                    self.take_operator()
                } else if identifier.is_whitespace() {
                    self.take_whitespace()
                } else if identifier.is_ascii_digit() {
                    self.take_integer_or_number()
                } else {
                    panic!("Unknown token!")
                }
            }
        }
    }

    #[inline]
    fn take_single(&mut self, kind: SyntaxKind) -> Token {
        let position = self.position();
        self.take();
        (kind, position, None)
    }

    #[inline]
    fn take_lower(&mut self) -> Token {
        let position @ Position { offset, .. } = self.position();
        self.take_while(is_ident);
        let end_offset = self.consumed();
        let kind = match &self.source[offset..end_offset] {
            // NOTE: Not all of these are treated as keywords by PureScript. e.g. `f as = as` is valid
            "as" => SyntaxKind::AsKw,
            "class" => SyntaxKind::ClassKw,
            "data" => SyntaxKind::DataKw,
            "derive" => SyntaxKind::DeriveKw,
            "else" => SyntaxKind::ElseKw,
            "false" => SyntaxKind::LiteralFalse,
            "forall" => SyntaxKind::ForallKw,
            "foreign" => SyntaxKind::ForeignKw,
            "if" => SyntaxKind::IfKw,
            "import" => SyntaxKind::ImportKw,
            "infix" => SyntaxKind::InfixKw,
            "infixl" => SyntaxKind::InfixlKw,
            "infixr" => SyntaxKind::InfixrKw,
            "instance" => SyntaxKind::InstanceKw,
            "module" => SyntaxKind::ModuleKw,
            "newtype" => SyntaxKind::NewtypeKw,
            "then" => SyntaxKind::ThenKw,
            "true" => SyntaxKind::LiteralTrue,
            "type" => SyntaxKind::TypeKw,
            "where" => SyntaxKind::WhereKw,
            _ => SyntaxKind::Lower,
        };
        (kind, position, None)
    }

    #[inline]
    fn take_upper(&mut self) -> Token {
        let position = self.position();
        self.take_while(|c| c.is_letter());
        (SyntaxKind::Upper, position, None)
    }

    #[inline]
    fn take_operator(&mut self) -> Token {
        let position @ Position { offset, .. } = self.position();
        self.take_while(is_operator);
        let offset_end = self.consumed();
        let kind = match &self.source[offset..offset_end] {
            "∷" => SyntaxKind::Colon2,
            "←" => SyntaxKind::LeftArrow,
            "→" => SyntaxKind::RightArrow,
            "⇒" => SyntaxKind::RightThickArrow,
            "∀" => SyntaxKind::ForallKw,
            "=" => SyntaxKind::Equal,
            ":" => SyntaxKind::Colon,
            "::" => SyntaxKind::Colon2,
            "." => SyntaxKind::Period,
            ".." => SyntaxKind::Period2,
            "<-" => SyntaxKind::LeftArrow,
            "->" => SyntaxKind::RightArrow,
            "<=" => SyntaxKind::LeftThickArrow,
            "=>" => SyntaxKind::RightThickArrow,
            "|" => SyntaxKind::Pipe,
            "@" => SyntaxKind::At,
            "-" => SyntaxKind::Minus,
            _ => SyntaxKind::Operator,
        };
        (kind, position, None)
    }

    #[inline]
    fn take_char(&mut self) -> Token {
        let position = self.position();
        assert_eq!(self.take(), '\'');
        let c = self.take();
        if c == '\\' {
            if let Some(err) = self.take_escape() {
                return (SyntaxKind::Error, position, Some(err));
            }
        }
        if self.first() == '\'' {
            self.take();
            (SyntaxKind::LiteralChar, position, None)
        } else {
            (SyntaxKind::Error, position, Some("invalid character literal"))
        }
    }

    #[inline]
    fn take_escape(&mut self) -> Option<&'static str> {
        match self.first() {
            't' | 'r' | 'n' | '"' | '\'' | '\\' => {
                self.take();
                None
            }
            'x' => {
                assert!(self.take() == 'x');
                self.take_while_max(is_hex_digit, 6);
                None
            }

            _ => Some("invalid escaped character literal"),
        }
    }

    #[inline]
    fn take_string(&mut self) -> (SyntaxKind, Position, Option<&'static str>) {
        let position @ Position { offset, .. } = self.position();
        self.take_while_max(|c| c == '"', 8);
        let leading_quotes = self.consumed() - offset;
        match leading_quotes {
            0 => panic!("Caller garantees leading quote!"),
            // "..." => a string
            1 => self.take_normal_string(position),
            // "" => an empty string
            2 => (SyntaxKind::LiteralString, position, None),
            // """...""" => a raw string with leading quotes
            3 | 4 | 5 => self.take_raw_string(position),
            // """"""" => Trailing and leading quotes in a raw string
            6 | 7 | 8 => (SyntaxKind::LiteralRawString, position, None),
            _ => unreachable!(),
        }
    }

    fn take_normal_string(
        &mut self,
        position: Position,
    ) -> (SyntaxKind, Position, Option<&'static str>) {
        loop {
            match self.take() {
                '\r' | '\n' => {
                    break (
                        SyntaxKind::Error,
                        position,
                        Some(
                            "invalid string literal - newlines are not allowed in string literals",
                        ),
                    )
                }
                '\\' => {
                    if let Some(err) = self.take_escape() {
                        break (SyntaxKind::Error, position, Some(err));
                    } else {
                        continue;
                    }
                }
                '"' => break (SyntaxKind::LiteralString, position, None),
                EOF_CHAR => {
                    break (
                        SyntaxKind::Error,
                        position,
                        Some("invalid string literal - end of file"),
                    )
                }
                _ => continue,
            }
        }
    }

    fn take_raw_string(
        &mut self,
        position: Position,
    ) -> (SyntaxKind, Position, Option<&'static str>) {
        loop {
            self.take_while(|c| c != '"');
            let start_of_quotes = self.consumed();
            self.take_while_max(|c| c == '"', 5);
            let end_of_quotes = self.consumed();
            let num_quotes = end_of_quotes - start_of_quotes;
            match num_quotes {
                0 => {
                    break (
                        SyntaxKind::Error,
                        position,
                        Some("invalid raw string literal - end of file"),
                    )
                }
                1 | 2 => continue,
                3 | 4 | 5 => break (SyntaxKind::LiteralRawString, position, None),
                _ => unreachable!(),
            }
        }
    }

    #[inline]
    fn take_integer_or_number(&mut self) -> Token {
        let position = self.position();
        // NOTE: The PureScript parser does not allow multiple leading 0s - the best way to handle
        // it is maybe to report the same errors as PureScript or we try to parse a super-set.
        // NOTE: The first rune has to be a digit, but after that underscores are allowed in
        // PureScript number- and int-literals.
        self.take_while(|c| c.is_ascii_digit() || c == '_');

        if self.first() == '.' {
            // `1..x` => [LiteralInteger, Period2, Lower]
            if self.second() == '.' {
                return (SyntaxKind::LiteralInteger, position, None);
            }

            // `1.2` => [LiteralNumber]
            if self.second().is_ascii_digit() {
                assert_eq!(self.take(), '.');
                self.take_while(|c| c.is_ascii_digit() || c == '_');
                // Scientific notation
                if matches!(self.first(), 'e' | 'E') {
                    self.take();
                    if matches!(self.first(), '+' | '-') {
                        self.take();
                    }
                    self.take_while(|c| c.is_ascii_digit() || c == '_');
                }
                return (SyntaxKind::LiteralNumber, position, None);
            }

            // `1.` => [Error]
            assert_eq!(self.take(), '.');
            return (SyntaxKind::Error, position, Some("invalid number literal"));
        }

        (SyntaxKind::LiteralInteger, position, None)
    }

    #[inline]
    fn take_whitespace(&mut self) -> Token {
        let position = self.position();
        self.take_while(|c| c.is_whitespace());
        (SyntaxKind::Whitespace, position, None)
    }

    #[inline]
    fn take_line_comment(&mut self) -> Token {
        let position = self.position();
        assert_eq!(self.take(), '-');
        assert_eq!(self.take(), '-');
        self.take_while(|c| c != '\n');
        (SyntaxKind::LineComment, position, None)
    }

    #[inline]
    fn take_block_comment(&mut self) -> Token {
        let position = self.position();
        assert_eq!(self.take(), '{');
        assert_eq!(self.take(), '-');
        let mut level = 1;
        loop {
            match (self.first(), self.second()) {
                ('{', '-') => {
                    level += 1;
                    self.take();
                    self.take();
                }
                ('-', '}') => {
                    level -= 1;
                    self.take();
                    self.take();
                }
                _ => (),
            }
            if level == 0 {
                break;
            }
            self.take();
        }
        (SyntaxKind::BlockComment, position, None)
    }

    #[inline]
    fn take_hole(&mut self) -> Token {
        let position = self.position();
        assert_eq!(self.take(), '?');
        assert!(is_ident(self.take()));
        self.take_while(is_ident);
        (SyntaxKind::Hole, position, None)
    }
}

fn is_operator(c: char) -> bool {
    match c {
        // These are the only valid ASCII operators
        '!' | '#' | '$' | '%' | '&' | '*' | '+' | '.' | '/' | '<' | '=' | '>' | '?' | '@'
        | '\\' | '^' | '|' | '-' | '~' | ':' => true,
        _ => c.is_symbol() && !c.is_ascii(),
    }
}

fn is_ident(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '\''
}

fn is_ident_start(c: char) -> bool {
    c.is_letter_lowercase() || c == '_'
}

fn is_hex_digit(c: char) -> bool {
    matches!(c, 'a'..='f' | 'A'..='F' | '0'..='9')
}

/// Lexes a `&str` into [`Lexed`].
pub fn lex(source: &str) -> Lexed {
    let mut lexer = Lexer::new(source);
    let mut lexed = Lexed::new(source);
    loop {
        if lexer.is_eof() {
            lexed.push(SyntaxKind::EndOfFile, lexer.position(), None);
            break;
        }
        let (kind, offset, error) = lexer.take_token();
        lexed.push(kind, offset, error);
    }
    lexed
}

#[cfg(test)]
mod tests {
    // Reading a failing test output is a lot easier with this:
    //  $ cargo test -- --test-threads 1
    use syntax::SyntaxKind;
    use syntax::SyntaxKind::*;

    use super::lex;

    fn expect_tokens(source: &str, expected: &[SyntaxKind]) {
        println!();
        let lexed = lex(source);
        let mut success = true;
        for (i, (actual, expected)) in lexed.kinds.iter().zip(expected).enumerate() {
            println!(
                "{} {:>2} {:3}@{:<18} {} {:<18}",
                if actual == expected { " " } else { "X" },
                i,
                lexed.positions[i].offset,
                format!("{:?}", actual),
                if actual == expected { " " } else { "X" },
                format!("{:?}", expected),
            );
            success = success && actual == expected;
        }
        if lexed.kinds.len() != expected.len() {
            let got_len = lexed.kinds.len();
            let expected_len = expected.len();
            println!("Got {got_len} tokens but expected {expected_len} tokens");
            success = false
        }
        for error in lexed.errors.iter() {
            println!("Error: {:?}", error);
            success = false;
        }
        if !success {
            panic!("test failed for source: <{source}>");
        }
    }

    #[test]
    fn lex_test_lower_and_ints() {
        expect_tokens(
            "a  b   c  1  2  3",
            &[
                Lower,
                Whitespace,
                Lower,
                Whitespace,
                Lower,
                Whitespace,
                LiteralInteger,
                Whitespace,
                LiteralInteger,
                Whitespace,
                LiteralInteger,
                EndOfFile,
            ],
        )
    }

    #[test]
    fn lex_test_numbers() {
        expect_tokens(
            "1.2e-3 0.4e+5 66.7e8",
            &[LiteralNumber, Whitespace, LiteralNumber, Whitespace, LiteralNumber, EndOfFile],
        )
    }

    #[test]
    fn lex_test_numbers_and_ints_with_underscores() {
        expect_tokens(
            "1_2_3__4 1_2_3.4_3_2e12",
            &[LiteralInteger, Whitespace, LiteralNumber, EndOfFile],
        )
    }

    #[test]
    fn lex_escaped_chars() {
        expect_tokens(
            "'a' '\\r' '\\xDEAD'",
            &[LiteralChar, Whitespace, LiteralChar, Whitespace, LiteralChar, EndOfFile],
        )
    }

    #[test]
    fn lex_double_period() {
        expect_tokens("1..5", &[LiteralInteger, Period2, LiteralInteger, EndOfFile])
    }

    #[test]
    fn lex_holes_and_lower() {
        expect_tokens(
            "?abc abc ?a123b''_ abc''' _ignore",
            &[
                Hole, Whitespace, Lower, Whitespace, Hole, Whitespace, Lower, Whitespace, Lower,
                EndOfFile,
            ],
        )
    }

    #[test]
    fn lex_strings() {
        expect_tokens(
            r#" "abc" "\"" """""x""""" """"ABC"""" "\x012qqqqq" "#,
            &[
                Whitespace,
                LiteralString,
                Whitespace,
                LiteralString,
                Whitespace,
                LiteralRawString,
                Whitespace,
                LiteralRawString,
                Whitespace,
                LiteralString,
                Whitespace,
                EndOfFile,
            ],
        )
    }

    #[test]
    fn lex_tick_comma_pipe() {
        expect_tokens(
            "`,| || |+|",
            &[Tick, Comma, Pipe, Whitespace, Operator, Whitespace, Operator, EndOfFile],
        )
    }

    #[test]
    fn lex_noise() {
        lex("@jKUpg7LjW9$cPyb#b3iek1S17BvUSOIP0HfBuvv^3UF#w3UpRy@a$");
    }

    #[test]
    fn lex_some_keywords() {
        expect_tokens(
            "data type forall foreign ∷ ← → ⇒ ∀",
            &[
                DataKw,
                Whitespace,
                TypeKw,
                Whitespace,
                ForallKw,
                Whitespace,
                ForeignKw,
                Whitespace,
                Colon2,
                Whitespace,
                LeftArrow,
                Whitespace,
                RightArrow,
                Whitespace,
                RightThickArrow,
                Whitespace,
                ForallKw,
                EndOfFile,
            ],
        );
    }

    #[test]
    fn lex_emojis() {
        lex("👋🌟😊🚀🔥");
    }

    #[test]
    fn lex_upper() {
        expect_tokens("Hello World", &[Upper, Whitespace, Upper, EndOfFile])
    }
}
