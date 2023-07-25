//! Text-based lexer into a sequence of [`SyntaxKind`]s.

use std::{ops::Range, str::Chars};

use syntax::SyntaxKind;
use unicode_categories::UnicodeCategories;

const EOF_CHAR: char = '\0';

/// A sequence of [`SyntaxKind`]s.
pub struct Lexed<'a> {
    source: &'a str,
    kinds: Vec<SyntaxKind>,
    offsets: Vec<u32>,
    errors: Vec<LexError>,
}

#[derive(Debug)]
struct LexError {
    message: String,
    index: u32,
}

impl<'a> Lexed<'a> {
    fn new(source: &'a str) -> Lexed<'a> {
        let kinds = vec![];
        let offsets = vec![];
        let errors = vec![];
        Lexed { source, kinds, offsets, errors }
    }

    fn push(&mut self, kind: SyntaxKind, offset: usize, error: Option<&str>) {
        self.kinds.push(kind);
        self.offsets.push(offset as u32);

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

    /// Returns the kind for an index.
    pub fn kind(&self, index: usize) -> SyntaxKind {
        assert!(index < self.len());
        self.kinds[index]
    }

    /// Returns the text for an index.
    pub fn text(&self, index: usize) -> &str {
        self.text_in_range(index..index + 1)
    }

    /// Returns the text for a range.
    pub fn text_in_range(&self, range: Range<usize>) -> &str {
        assert!(range.start < range.end && range.end <= self.len());
        let low = self.offsets[range.start] as usize;
        let high = self.offsets[range.end] as usize;
        &self.source[low..high]
    }
}

/// Character-based lexer, inspired by `rustc_lexer`.
struct Lexer<'a> {
    source: &'a str,
    chars: Chars<'a>,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Lexer<'a> {
        let chars = source.chars();
        Lexer { source, chars }
    }

    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    fn consumed(&self) -> usize {
        self.source.len() - self.chars.as_str().len()
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
        self.chars.next().unwrap_or(EOF_CHAR)
    }

    fn take_while(&mut self, predicate: impl Fn(char) -> bool) {
        while predicate(self.first()) && !self.is_eof() {
            self.take();
        }
    }
}

impl<'a> Lexer<'a> {
    fn take_token(&mut self) -> (SyntaxKind, usize, Option<&str>) {
        match self.first() {
            '-' if self.second() == '-' => self.take_line_comment(),
            '{' if self.second() == '-' => self.take_block_comment(),

            '(' => self.take_single(SyntaxKind::LeftParenthesis),
            ')' => self.take_single(SyntaxKind::RightParenthesis),
            '{' => self.take_single(SyntaxKind::LeftBracket),
            '}' => self.take_single(SyntaxKind::RightBracket),
            '[' => self.take_single(SyntaxKind::LeftBrace),
            ']' => self.take_single(SyntaxKind::RightBrace),

            '\'' => self.take_char(),
            '"' => self.take_string(),

            identifier => {
                if identifier.is_letter_lowercase() {
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
                    todo!("Unknown token!")
                }
            }
        }
    }

    #[inline]
    fn take_single(&mut self, kind: SyntaxKind) -> (SyntaxKind, usize, Option<&str>) {
        let offset = self.consumed();
        self.take();
        (kind, offset, None)
    }

    #[inline]
    fn take_lower(&mut self) -> (SyntaxKind, usize, Option<&str>) {
        let offset = self.consumed();
        self.take_while(|c| c.is_letter());
        let end_offset = self.consumed();
        let kind = match &self.source[offset..end_offset] {
            // NOTE: Not all of these are treated as keywords by PureScript. e.g. `f as = as` is valid
            "as" => SyntaxKind::AsKw,
            "class" => SyntaxKind::ClassKw,
            "data" => SyntaxKind::DataKw,
            "derive" => SyntaxKind::DeriveKw,
            "false" => SyntaxKind::LiteralFalse,
            "foreign" => SyntaxKind::ForeignKw,
            "import" => SyntaxKind::ImportKw,
            "infix" => SyntaxKind::InfixKw,
            "infixl" => SyntaxKind::InfixlKw,
            "infixr" => SyntaxKind::InfixrKw,
            "instance" => SyntaxKind::InstanceKw,
            "module" => SyntaxKind::ModuleKw,
            "newtype" => SyntaxKind::NewtypeKw,
            "true" => SyntaxKind::LiteralTrue,
            "type" => SyntaxKind::TypeKw,
            "where" => SyntaxKind::WhereKw,
            _ => SyntaxKind::Lower,
        };
        (kind, offset, None)
    }

    #[inline]
    fn take_upper(&mut self) -> (SyntaxKind, usize, Option<&str>) {
        let offset = self.consumed();
        self.take_while(|c| c.is_letter());
        (SyntaxKind::Upper, offset, None)
    }

    #[inline]
    fn take_operator(&mut self) -> (SyntaxKind, usize, Option<&str>) {
        let offset = self.consumed();
        self.take_while(is_operator);
        let offset_end = self.consumed();
        let kind = match &self.source[offset..offset_end] {
            "=" => SyntaxKind::Equal,
            ":" => SyntaxKind::Colon,
            "::" => SyntaxKind::Colon2,
            "." => SyntaxKind::Period,
            ".." => SyntaxKind::Period2,
            "<-" => SyntaxKind::LeftArrow,
            "->" => SyntaxKind::RightArrow,
            "<=" => SyntaxKind::LeftThickArrow,
            "=>" => SyntaxKind::RightThickArrow,
            _ => SyntaxKind::Operator,
        };
        (kind, offset, None)
    }

    #[inline]
    fn take_char(&mut self) -> (SyntaxKind, usize, Option<&str>) {
        let offset = self.consumed();
        assert_eq!(self.take(), '\'');
        self.take();
        if self.first() == '\'' {
            self.take();
            (SyntaxKind::LiteralChar, offset, None)
        } else {
            (SyntaxKind::Error, offset, Some("invalid character literal"))
        }
    }

    #[inline]
    fn take_string(&mut self) -> (SyntaxKind, usize, Option<&str>) {
        let offset = self.consumed();
        assert_eq!(self.take(), '"');
        self.take_while(|c| c != '"');
        if self.first() == '"' {
            self.take();
            (SyntaxKind::LiteralString, offset, None)
        } else {
            (SyntaxKind::Error, offset, Some("invalid string literal"))
        }
    }

    #[inline]
    fn take_integer_or_number(&mut self) -> (SyntaxKind, usize, Option<&str>) {
        let offset = self.consumed();
        self.take_while(|c| c.is_ascii_digit() || c == '_');

        if self.first() == '.' {
            // `1..x` => [LiteralInteger, Period2, Lower]
            if self.second() == '.' {
                return (SyntaxKind::LiteralInteger, offset, None);
            }

            // `1.2` => [LiteralNumber]
            if self.second().is_ascii_digit() {
                assert_eq!(self.take(), '.');
                self.take_while(|c| c.is_ascii_digit() || c == '_');
                if matches!(self.first(), 'e' | 'E') {
                    self.take();
                    if matches!(self.first(), '+' | '-') { self.take(); }
                    self.take_while(|c| c.is_ascii_digit() || c == '_');
                }
                return (SyntaxKind::LiteralNumber, offset, None);
            }

            // `1.` => [Error]
            assert_eq!(self.take(), '.');
            return (SyntaxKind::Error, offset, Some("invalid number literal"));
        }

        return (SyntaxKind::LiteralInteger, offset, None);
    }

    #[inline]
    fn take_whitespace(&mut self) -> (SyntaxKind, usize, Option<&str>) {
        let offset = self.consumed();
        self.take_while(|c| c.is_whitespace());
        (SyntaxKind::Whitespace, offset, None)
    }

    #[inline]
    fn take_line_comment(&mut self) -> (SyntaxKind, usize, Option<&str>) {
        let offset = self.consumed();
        assert_eq!(self.take(), '-');
        assert_eq!(self.take(), '-');
        self.take_while(|c| c != '\n');
        (SyntaxKind::LineComment, offset, None)
    }

    #[inline]
    fn take_block_comment(&mut self) -> (SyntaxKind, usize, Option<&str>) {
        let offset = self.consumed();
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
        (SyntaxKind::BlockComment, offset, None)
    }
}

fn is_operator(c: char) -> bool {
    c.is_symbol() || c.is_punctuation()
}

/// Lexes a `&str` into [`Lexed`].
pub fn lex(source: &str) -> Lexed {
    let mut lexer = Lexer::new(source);
    let mut lexed = Lexed::new(source);
    loop {
        if lexer.is_eof() {
            lexed.push(SyntaxKind::EndOfFile, lexer.consumed(), None);
            break;
        }
        let (kind, offset, error) = lexer.take_token();
        lexed.push(kind, offset, error);
    }
    lexed
}

#[test]
fn lexer_test() {
    let lexed = lex("1..5");
    dbg!(lexed.kinds);
    dbg!(lexed.offsets);
    dbg!(lexed.errors);
}

#[cfg(test)]
mod tests {
    // Reading a failing test output is a lot easier with this:
    //  $ cargo test -- --test-threads 1
    use syntax::SyntaxKind;
    use syntax::SyntaxKind::*;

    use super::lex;

    fn expect_tokens<'a>(source: &'a str, expected: &[SyntaxKind]) {
        println!();
        let lexed = lex(source);
        let mut success = true;
        for (i, (actual, expected)) in lexed.kinds.iter().zip(expected).enumerate() {
            if actual == expected && success {
                continue;
            }
            success = false;
            println!("{:?}: {:?}@{:?} != {:?}", i, actual, lexed.offsets[i], expected);
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
            assert!(false, "test failed for source: {source}");
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
}
