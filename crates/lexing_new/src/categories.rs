use unicode_categories::UnicodeCategories;

#[allow(clippy::wrong_self_convention)]
pub(super) trait LexerCategories {
    fn is_lower_start(self) -> bool;
    fn is_upper_start(self) -> bool;
    fn is_name(self) -> bool;
    fn is_operator(self) -> bool;
}

impl LexerCategories for char {
    fn is_lower_start(self) -> bool {
        self.is_letter_lowercase() || self == '_'
    }

    fn is_upper_start(self) -> bool {
        self.is_letter_uppercase()
    }

    fn is_name(self) -> bool {
        self.is_alphanumeric() || self == '_' || self == '\''
    }

    fn is_operator(self) -> bool {
        match self {
            // These are the only valid ASCII operators
            '!' | '#' | '$' | '%' | '&' | '*' | '+' | '.' | '/' | '<' | '=' | '>' | '?' | '@'
            | '\\' | '^' | '|' | '-' | ':' => true,
            _ => self.is_symbol() && !self.is_ascii(),
        }
    }
}
