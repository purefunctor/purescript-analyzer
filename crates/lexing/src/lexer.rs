//! The core character-based lexer, inspired by `rustc_lexer`.

use std::str::Chars;

use position::Position;
use syntax::SyntaxKind;
use unicode_categories::UnicodeCategories;

use crate::lexed::Lexed;

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

    pub(crate) fn finish(mut self) -> Lexed<'a> {
        self.lexed.push(SyntaxKind::END_OF_FILE, self.position(), None);
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

            '(' => self.take_operator_name_or_left_parenthesis(),
            ')' => self.take_single(SyntaxKind::RIGHT_PARENTHESIS),
            '{' => self.take_single(SyntaxKind::LEFT_CURLY),
            '}' => self.take_single(SyntaxKind::RIGHT_CURLY),
            '[' => self.take_single(SyntaxKind::LEFT_SQUARE),
            ']' => self.take_single(SyntaxKind::RIGHT_SQUARE),

            '`' => self.take_single(SyntaxKind::TICK),
            ',' => self.take_single(SyntaxKind::COMMA),
            '?' if !is_name(self.second()) => self.take_single(SyntaxKind::OPERATOR),
            '_' if !is_name(self.second()) => self.take_single(SyntaxKind::UNDERSCORE),

            '\'' => self.take_char(),
            '"' => self.take_string(),

            i => {
                if i == '?' {
                    self.take_hole();
                } else if is_lower_start(i) {
                    self.take_lower();
                } else if i.is_letter_uppercase() {
                    self.take_prefix_or_upper()
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
    fn take_hole(&mut self) {
        let position = self.position();
        assert_eq!(self.take(), '?');
        self.take_while(is_name);
        self.lexed.push(SyntaxKind::HOLE, position, None)
    }

    #[inline]
    fn take_lower(&mut self) {
        let position @ Position { offset, .. } = self.position();
        self.take_while(is_name);
        let end_offset = self.consumed();
        let kind = match &self.source[offset..end_offset] {
            // NOTE: Not all of these are treated as keywords by PureScript. e.g. `f as = as` is valid
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
            _ => SyntaxKind::LOWER,
        };
        self.lexed.push(kind, position, None)
    }

    #[inline]
    fn take_prefix_or_upper(&mut self) {
        let prefix = self.position();
        let mut has_prefix = false;

        // It's best to follow through this block with a few examples:
        //
        //>================================================================
        //
        // Hooks.do => [PREFIX, LOWER]
        // Main.main => [UPPER, LOWER]
        //
        // 1. (A) takes 'Hooks'
        // 2. (B) takes '.'
        // 3. (D) pushes [PREFIX] and finishes
        //
        //>================================================================
        //
        // List.Cons => [PREFIX, UPPER]
        //
        // 1. (A) takes 'List'
        // 2. (B) takes '.'
        // 3. (A) takes 'Cons'
        // 4. (B) takes nothing
        // 5. (C) pushes [PREFIX, UPPER] and finishes
        //
        //>================================================================
        loop {
            // (D)
            if !is_upper_start(self.first()) && has_prefix {
                return self.lexed.push(SyntaxKind::PREFIX, prefix, None);
            }

            // (A)
            let proper = self.position();
            self.take_while(is_name);

            // (B)
            if self.first() == '.' {
                self.take();
                has_prefix = true;
            } else {
                // (C)
                if has_prefix {
                    self.lexed.push(SyntaxKind::PREFIX, prefix, None);
                }
                return self.lexed.push(SyntaxKind::UPPER, proper, None);
            }
        }
    }

    #[inline]
    fn take_operator_kind(&mut self) -> (SyntaxKind, Position) {
        let position @ Position { offset, .. } = self.position();
        self.take_while(is_operator);
        let offset_end = self.consumed();
        let kind = match &self.source[offset..offset_end] {
            "∷" => SyntaxKind::DOUBLE_COLON,
            "←" => SyntaxKind::LEFT_ARROW,
            "→" => SyntaxKind::RIGHT_ARROW,
            "⇒" => SyntaxKind::RIGHT_THICK_ARROW,
            "∀" => SyntaxKind::FORALL,
            "=" => SyntaxKind::EQUAL,
            ":" => SyntaxKind::COLON,
            "::" => SyntaxKind::DOUBLE_COLON,
            "." => SyntaxKind::PERIOD,
            ".." => SyntaxKind::DOUBLE_PERIOD,
            "<-" => SyntaxKind::LEFT_ARROW,
            "->" => SyntaxKind::RIGHT_ARROW,
            "<=" => SyntaxKind::LEFT_THICK_ARROW,
            "=>" => SyntaxKind::RIGHT_THICK_ARROW,
            "|" => SyntaxKind::PIPE,
            "@" => SyntaxKind::AT,
            "-" => SyntaxKind::MINUS,
            "\\" => SyntaxKind::BACKSLASH,
            _ => SyntaxKind::OPERATOR,
        };
        (kind, position)
    }

    #[inline]
    fn take_operator(&mut self) {
        let (kind, position) = self.take_operator_kind();
        self.lexed.push(kind, position, None)
    }

    #[inline]
    fn take_operator_name_or_left_parenthesis(&mut self) {
        let lp_position = self.position();
        assert_eq!(self.take(), '(');
        if is_operator(self.first()) {
            let (op_kind, op_position) = self.take_operator_kind();
            if self.first() == ')' {
                self.take();
                let op_kind = match op_kind {
                    SyntaxKind::DOUBLE_PERIOD => SyntaxKind::DOUBLE_PERIOD_OPERATOR_NAME,
                    _ => SyntaxKind::OPERATOR_NAME,
                };
                self.lexed.push(op_kind, lp_position, None);
            } else {
                self.lexed.push(SyntaxKind::LEFT_PARENTHESIS, lp_position, None);
                self.lexed.push(op_kind, op_position, None);
            }
        } else {
            self.lexed.push(SyntaxKind::LEFT_PARENTHESIS, lp_position, None);
        }
    }

    #[inline]
    fn take_char(&mut self) {
        let position = self.position();

        assert_eq!(self.take(), '\'');
        self.take_while(|c| c != '\'');

        let error = if self.take() != '\'' { Some("unexpected end of file") } else { None };
        self.lexed.push(SyntaxKind::CHAR, position, error);
    }

    #[inline]
    fn take_string(&mut self) {
        let position @ Position { offset, .. } = self.position();
        self.take_while_max(|c| c == '"', 8);
        let leading_quotes = self.consumed() - offset;
        match leading_quotes {
            // "..." => a string
            1 => {
                let error = self.take_normal_string();
                self.lexed.push(SyntaxKind::STRING, position, error);
            }
            // "" => an empty string
            2 => {
                self.lexed.push(SyntaxKind::STRING, position, None);
            }
            // """...""" => a raw string with leading quotes
            3..=5 => {
                let error = self.take_raw_string();
                self.lexed.push(SyntaxKind::RAW_STRING, position, error);
            }
            // """"""" => Trailing and leading quotes in a raw string
            6..=8 => {
                self.lexed.push(SyntaxKind::RAW_STRING, position, None);
            }
            _ => {
                unreachable!();
            }
        }
    }

    fn take_normal_string(&mut self) -> Option<&'static str> {
        self.take_while(|c| c != '"');
        if self.take() != '"' {
            Some("unexpected end of file")
        } else {
            None
        }
    }

    fn take_raw_string(&mut self) -> Option<&'static str> {
        loop {
            self.take_while(|c| c != '"');
            let start = self.consumed();
            self.take_while_max(|c| c == '"', 5);
            let end = self.consumed();
            match end - start {
                0 => {
                    return Some("unexpected end of file");
                }
                1 | 2 => {
                    continue;
                }
                3..=5 => {
                    return None;
                }
                _ => {
                    unreachable!();
                }
            }
        }
    }

    #[inline]
    fn take_integer_or_number(&mut self) {
        let position = self.position();

        let error = if self.first() == '0' && self.second() == '0' {
            Some("Too many leading zeros")
        } else {
            None
        };

        let mut kind = SyntaxKind::INTEGER;
        self.take_while(|c| c.is_ascii_digit() || c == '_');

        // lex(1..2) = [INTEGER, DOUBLE_PERIOD, INTEGER]
        if self.first() == '.' && self.second() == '.' {
            return self.lexed.push(SyntaxKind::INTEGER, position, error);
        }

        if self.first() == '.' && self.second().is_ascii_digit() {
            self.take();
            self.take_while(|c| c.is_ascii_digit() || c == '_');
            kind = SyntaxKind::NUMBER;
        }

        if self.first() == 'e' {
            self.take();
            if self.first() == '+' || self.first() == '-' {
                self.take();
            }
            self.take_while(|c| c.is_ascii_digit() || c == '_');
            kind = SyntaxKind::NUMBER;
        }

        self.lexed.push(kind, position, error);
    }

    #[inline]
    fn take_whitespace(&mut self) {
        let position = self.position();
        self.take_while(|c| c.is_whitespace());
        self.lexed.push(SyntaxKind::WHITESPACE, position, None)
    }

    #[inline]
    fn take_line_comment(&mut self) {
        let position = self.position();
        assert_eq!(self.take(), '-');
        assert_eq!(self.take(), '-');
        self.take_while(|c| c != '\n');
        self.lexed.push(SyntaxKind::LINE_COMMENT, position, None)
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
        self.lexed.push(SyntaxKind::BLOCK_COMMENT, position, None)
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

fn is_lower_start(c: char) -> bool {
    c.is_letter_lowercase() || c == '_'
}

fn is_upper_start(c: char) -> bool {
    c.is_letter_uppercase()
}

fn is_name(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '\''
}
