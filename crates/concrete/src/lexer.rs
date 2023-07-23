use std::str::Chars;

use unicode_categories::UnicodeCategories;

use crate::syntax::SyntaxKind;

struct Cursor<'a> {
    source: &'a str,
    length: usize,
    chars: Chars<'a>,
}

const EOF_CHAR: char = '\0';

impl<'a> Cursor<'a> {
    fn new(source: &'a str) -> Cursor<'a> {
        let length = source.len();
        let chars = source.chars();
        Cursor { source, length, chars }
    }

    #[must_use]
    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    fn consumed(&self) -> usize {
        self.length - self.chars.as_str().len()
    }

    fn peek_1(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    #[allow(unused)]
    fn peek_2(&self) -> char {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next().unwrap_or(EOF_CHAR)
    }

    fn take(&mut self) -> char {
        self.chars.next().unwrap_or(EOF_CHAR)
    }

    fn take_while(&mut self, predicate: impl Fn(char) -> bool) {
        while !self.is_eof() && predicate(self.peek_1()) {
            self.take();
        }
    }
}

impl<'a> Cursor<'a> {
    fn take_token(&mut self) -> (usize, usize, SyntaxKind) {
        match self.peek_1() {
            '.' => self.take_kind(SyntaxKind::Period),
            '=' => self.take_kind(SyntaxKind::Period),

            initial => {
                if initial.is_letter_uppercase() {
                    self.take_upper()
                } else if initial.is_letter_lowercase() {
                    self.take_kw_or_lower()
                } else if initial.is_whitespace() {
                    self.take_whitespace()
                } else {
                    todo!("ERROR!");
                }
            }
        }
    }

    #[inline]
    fn take_kind(&mut self, kind: SyntaxKind) -> (usize, usize, SyntaxKind) {
        let begin = self.consumed();
        let _ = self.take();
        let end = self.consumed();
        (begin, end, kind)
    }

    #[inline]
    fn take_upper(&mut self) -> (usize, usize, SyntaxKind) {
        let begin = self.consumed();
        self.take_while(|c| c.is_letter());
        let end = self.consumed();
        (begin, end, SyntaxKind::Upper)
    }

    #[inline]
    fn take_kw_or_lower(&mut self) -> (usize, usize, SyntaxKind) {
        let begin = self.consumed();
        self.take_while(|c| c.is_letter());
        let end = self.consumed();
        let kind = match &self.source[begin..end] {
            "module" => SyntaxKind::ModuleKw,
            "where" => SyntaxKind::WhereKw,
            _ => SyntaxKind::Lower,
        };
        (begin, end, kind)
    }

    #[inline]
    fn take_whitespace(&mut self) -> (usize, usize, SyntaxKind) {
        let begin = self.consumed();
        self.take_while(|c| c.is_whitespace());
        let end = self.consumed();
        (begin, end, SyntaxKind::Whitespace)
    }

    fn finalize(self) -> (usize, usize, SyntaxKind) {
        assert!(self.is_eof());
        let offset = self.consumed();
        (offset, offset, SyntaxKind::EndOfFile)
    }
}

pub fn lex(source: &str) -> Vec<(usize, usize, SyntaxKind)> {
    let mut tokens = vec![];
    let mut cursor = Cursor::new(source);

    loop {
        if cursor.is_eof() {
            tokens.push(cursor.finalize());
            break tokens;
        }
        tokens.push(cursor.take_token());
    }
}

#[cfg(test)]
mod tests {
    use super::lex;

    #[test]
    fn basic_lexing() {
        dbg!(lex("module Hello.World where"));
    }
}
