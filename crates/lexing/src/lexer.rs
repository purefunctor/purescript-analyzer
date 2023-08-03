//! The core character-based lexer, inspired by `rustc_lexer`.

use std::str::Chars;

use syntax::SyntaxKind;
use unicode_categories::UnicodeCategories;

use crate::{lexed::Lexed, position::Position};

const EOF_CHAR: char = '\0';

pub(crate) struct Lexer<'a> {
    source: &'a str,
    chars: Chars<'a>,
    line: u32,
    column: u32,
    lexed: Lexed<'a>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(source: &'a str) -> Lexer<'a> {
        let chars = source.chars();
        let line = 1;
        let column = 1;
        let lexed = Lexed::new(source);
        Lexer { source, chars, line, column, lexed }
    }

    pub(crate) fn finalize(mut self) -> Lexed<'a> {
        self.lexed.push(SyntaxKind::EndOfFile, self.position(), None);
        self.lexed
    }

    pub(crate) fn is_eof(&self) -> bool {
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

impl<'a> Lexer<'a> {
    pub(crate) fn take_token(&mut self) {
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
            '?' => self.take_single(SyntaxKind::Question),
            '_' if !is_lower(self.second()) => self.take_single(SyntaxKind::Underscore),

            '\'' => self.take_char(),
            '"' => self.take_string(),

            i => {
                if is_lower_start(i) {
                    self.take_lower()
                } else if i.is_letter_uppercase() {
                    self.take_upper()
                } else if is_operator(i) {
                    self.take_operator()
                } else if i.is_whitespace() {
                    self.take_whitespace()
                } else if i.is_ascii_digit() {
                    self.take_integer_or_number()
                } else {
                    panic!("Unknown token!")
                }
            }
        }
    }

    #[inline]
    fn take_single(&mut self, kind: SyntaxKind) {
        let position = self.position();
        self.take();
        self.lexed.push(kind, position, None)
    }

    #[inline]
    fn take_lower(&mut self) {
        let position @ Position { offset, .. } = self.position();
        self.take_while(is_lower);
        let end_offset = self.consumed();
        let kind = match &self.source[offset..end_offset] {
            // NOTE: Not all of these are treated as keywords by PureScript. e.g. `f as = as` is valid
            "ado" => SyntaxKind::AdoKw,
            "as" => SyntaxKind::AsKw,
            "case" => SyntaxKind::CaseKw,
            "class" => SyntaxKind::ClassKw,
            "data" => SyntaxKind::DataKw,
            "derive" => SyntaxKind::DeriveKw,
            "do" => SyntaxKind::DoKw,
            "else" => SyntaxKind::ElseKw,
            "false" => SyntaxKind::LiteralFalse,
            "forall" => SyntaxKind::ForallKw,
            "foreign" => SyntaxKind::ForeignKw,
            "if" => SyntaxKind::IfKw,
            "import" => SyntaxKind::ImportKw,
            "in" => SyntaxKind::InKw,
            "infix" => SyntaxKind::InfixKw,
            "infixl" => SyntaxKind::InfixlKw,
            "infixr" => SyntaxKind::InfixrKw,
            "instance" => SyntaxKind::InstanceKw,
            "let" => SyntaxKind::LetKw,
            "module" => SyntaxKind::ModuleKw,
            "newtype" => SyntaxKind::NewtypeKw,
            "of" => SyntaxKind::OfKw,
            "then" => SyntaxKind::ThenKw,
            "true" => SyntaxKind::LiteralTrue,
            "type" => SyntaxKind::TypeKw,
            "where" => SyntaxKind::WhereKw,
            _ => SyntaxKind::Lower,
        };
        self.lexed.push(kind, position, None)
    }

    #[inline]
    fn take_upper(&mut self) {
        let position = self.position();
        self.take_while(|c| c.is_letter());
        self.lexed.push(SyntaxKind::Upper, position, None)
    }

    #[inline]
    fn take_operator(&mut self) {
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
            "\\" => SyntaxKind::Backslash,
            _ => SyntaxKind::Operator,
        };
        self.lexed.push(kind, position, None)
    }

    #[inline]
    fn take_char(&mut self) {
        let position = self.position();
        assert_eq!(self.take(), '\'');
        let c = self.take();
        if c == '\\' {
            if let Some(err) = self.take_escape() {
                return self.lexed.push(SyntaxKind::Error, position, Some(err));
            }
        }
        if self.first() == '\'' {
            self.take();
            self.lexed.push(SyntaxKind::LiteralChar, position, None)
        } else {
            self.lexed.push(SyntaxKind::Error, position, Some("invalid character literal"))
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
    fn take_string(&mut self) {
        let position @ Position { offset, .. } = self.position();
        self.take_while_max(|c| c == '"', 8);
        let leading_quotes = self.consumed() - offset;
        match leading_quotes {
            0 => panic!("Caller garantees leading quote!"),
            // "..." => a string
            1 => self.take_normal_string(position),
            // "" => an empty string
            2 => self.lexed.push(SyntaxKind::LiteralString, position, None),
            // """...""" => a raw string with leading quotes
            3 | 4 | 5 => self.take_raw_string(position),
            // """"""" => Trailing and leading quotes in a raw string
            6 | 7 | 8 => self.lexed.push(SyntaxKind::LiteralRawString, position, None),
            _ => unreachable!(),
        }
    }

    fn take_normal_string(&mut self, position: Position) {
        loop {
            match self.take() {
                '\r' | '\n' => {
                    break self.lexed.push(
                        SyntaxKind::Error,
                        position,
                        Some(
                            "invalid string literal - newlines are not allowed in string literals",
                        ),
                    )
                }
                '\\' => {
                    if let Some(err) = self.take_escape() {
                        break self.lexed.push(SyntaxKind::Error, position, Some(err));
                    } else {
                        continue;
                    }
                }
                '"' => break self.lexed.push(SyntaxKind::LiteralString, position, None),
                EOF_CHAR => {
                    break self.lexed.push(
                        SyntaxKind::Error,
                        position,
                        Some("invalid string literal - end of file"),
                    )
                }
                _ => continue,
            }
        }
    }

    fn take_raw_string(&mut self, position: Position) {
        loop {
            self.take_while(|c| c != '"');
            let start_of_quotes = self.consumed();
            self.take_while_max(|c| c == '"', 5);
            let end_of_quotes = self.consumed();
            let num_quotes = end_of_quotes - start_of_quotes;
            match num_quotes {
                0 => {
                    break self.lexed.push(
                        SyntaxKind::Error,
                        position,
                        Some("invalid raw string literal - end of file"),
                    )
                }
                1 | 2 => continue,
                3 | 4 | 5 => break self.lexed.push(SyntaxKind::LiteralRawString, position, None),
                _ => unreachable!(),
            }
        }
    }

    #[inline]
    fn take_integer_or_number(&mut self) {
        let position = self.position();
        // NOTE: The PureScript parser does not allow multiple leading 0s - the best way to handle
        // it is maybe to report the same errors as PureScript or we try to parse a super-set.
        // NOTE: The first rune has to be a digit, but after that underscores are allowed in
        // PureScript number- and int-literals.
        self.take_while(|c| c.is_ascii_digit() || c == '_');

        if self.first() == '.' {
            // `1..x` => [LiteralInteger, Period2, Lower]
            if self.second() == '.' {
                return self.lexed.push(SyntaxKind::LiteralInteger, position, None);
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
                return self.lexed.push(SyntaxKind::LiteralNumber, position, None);
            }

            // `1.` => [Error]
            assert_eq!(self.take(), '.');
            return self.lexed.push(SyntaxKind::Error, position, Some("invalid number literal"));
        }

        self.lexed.push(SyntaxKind::LiteralInteger, position, None)
    }

    #[inline]
    fn take_whitespace(&mut self) {
        let position = self.position();
        self.take_while(|c| c.is_whitespace());
        self.lexed.push(SyntaxKind::Whitespace, position, None)
    }

    #[inline]
    fn take_line_comment(&mut self) {
        let position = self.position();
        assert_eq!(self.take(), '-');
        assert_eq!(self.take(), '-');
        self.take_while(|c| c != '\n');
        self.lexed.push(SyntaxKind::LineComment, position, None)
    }

    #[inline]
    fn take_block_comment(&mut self) {
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
        self.lexed.push(SyntaxKind::BlockComment, position, None)
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

fn is_lower(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '\''
}

fn is_lower_start(c: char) -> bool {
    c.is_letter_lowercase() || c == '_'
}

fn is_hex_digit(c: char) -> bool {
    matches!(c, 'a'..='f' | 'A'..='F' | '0'..='9')
}
