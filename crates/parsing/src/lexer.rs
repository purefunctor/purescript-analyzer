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
}

impl<'a> Lexed<'a> {
    fn new(source: &'a str) -> Lexed<'a> {
        let kinds = vec![];
        let offsets = vec![];
        Lexed { source, kinds, offsets }
    }

    fn push(&mut self, kind: SyntaxKind, offset: usize) {
        self.kinds.push(kind);
        self.offsets.push(offset as u32);
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
    fn take_token(&mut self) -> (SyntaxKind, usize) {
        match self.first() {
            '-' if self.second() == '-' => {
                self.take_line_comment()
            }
            '{' if self.second() == '-' => {
                self.take_block_comment()
            }

            '(' => self.take_single(SyntaxKind::LeftParenthesis),
            ')' => self.take_single(SyntaxKind::RightParenthesis),
            '{' => self.take_single(SyntaxKind::LeftBracket),
            '}' => self.take_single(SyntaxKind::RightBracket),
            '[' => self.take_single(SyntaxKind::LeftBrace),
            ']' => self.take_single(SyntaxKind::RightBrace),

            identifier => {
                if identifier.is_letter_lowercase() {
                    self.take_lower_or_kw()
                } else if identifier.is_letter_uppercase() {
                    self.take_upper()
                } else if is_operator(identifier) {
                    self.take_operator()
                } else if identifier.is_whitespace() {
                    self.take_whitespace()
                } else {
                    todo!("Unknown token!")
                }
            }
        }
    }

    #[inline]
    fn take_single(&mut self, kind: SyntaxKind) -> (SyntaxKind, usize) {
        let offset = self.consumed();
        self.take();
        (kind, offset)
    }

    #[inline]
    fn take_lower_or_kw(&mut self) -> (SyntaxKind, usize) {
        let offset = self.consumed();
        self.take_while(|c| c.is_letter());
        let end_offset = self.consumed();
        let kind = match &self.source[offset..end_offset] {
            "as" => SyntaxKind::AsKw,
            "class" => SyntaxKind::ClassKw,
            "data" => SyntaxKind::DataKw,
            "derive" => SyntaxKind::DeriveKw,
            "foreign" => SyntaxKind::ForeignKw,
            "import" => SyntaxKind::ImportKw,
            "infix" => SyntaxKind::InfixKw,
            "infixl" => SyntaxKind::InfixlKw,
            "infixr" => SyntaxKind::InfixrKw,
            "instance" => SyntaxKind::InstanceKw,
            "module" => SyntaxKind::ModuleKw,
            "newtype" => SyntaxKind::NewtypeKw,
            "type" => SyntaxKind::TypeKw,
            "where" => SyntaxKind::WhereKw,
            _ => SyntaxKind::Lower,
        };
        (kind, offset)
    }

    #[inline]
    fn take_upper(&mut self) -> (SyntaxKind, usize) {
        let offset = self.consumed();
        self.take_while(|c| c.is_letter());
        (SyntaxKind::Upper, offset)
    }

    #[inline]
    fn take_operator(&mut self) -> (SyntaxKind, usize) {
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
            _ => SyntaxKind::Operator
        };
        (kind, offset)
    }

    #[inline]
    fn take_whitespace(&mut self) -> (SyntaxKind, usize) {
        let offset = self.consumed();
        self.take_while(|c| c.is_whitespace());
        (SyntaxKind::Whitespace, offset)
    }

    #[inline]
    fn take_line_comment(&mut self) -> (SyntaxKind, usize) {
        let offset = self.consumed();
        assert_eq!(self.take(), '-');
        assert_eq!(self.take(), '-');
        self.take_while(|c| c != '\n');
        (SyntaxKind::LineComment, offset)
    }

    #[inline]
    fn take_block_comment(&mut self) -> (SyntaxKind, usize) {
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
        (SyntaxKind::BlockComment, offset)
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
            lexed.push(SyntaxKind::EndOfFile, lexer.consumed());
            break;
        }
        let (kind, offset) = lexer.take_token();
        lexed.push(kind, offset);
    }
    lexed
}

#[test]
fn lexer_test() {
    let lexed = lex(". .. ... : :: ::: <- -> <= => {- hello -} -- final");
    dbg!(lexed.kinds);
    dbg!(lexed.offsets);
}
