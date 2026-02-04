use crate::{SyntaxKind, TokenSet};

pub const ROLE: TokenSet =
    TokenSet::new(&[SyntaxKind::NOMINAL, SyntaxKind::PHANTOM, SyntaxKind::REPRESENTATIONAL]);

pub const LOWER: TokenSet =
    TokenSet::new(&[SyntaxKind::LOWER, SyntaxKind::AS, SyntaxKind::HIDING, SyntaxKind::ROLE])
        .union(ROLE);

pub const OPERATOR: TokenSet = TokenSet::new(&[
    SyntaxKind::OPERATOR,
    SyntaxKind::COLON,
    SyntaxKind::MINUS,
    SyntaxKind::DOUBLE_PERIOD,
    SyntaxKind::LEFT_THICK_ARROW,
]);

pub const OPERATOR_NAME: TokenSet =
    TokenSet::new(&[SyntaxKind::OPERATOR_NAME, SyntaxKind::DOUBLE_PERIOD_OPERATOR_NAME]);

pub const KEYWORD: TokenSet = TokenSet::new(&[
    SyntaxKind::MODULE,
    SyntaxKind::WHERE,
    SyntaxKind::IMPORT,
    SyntaxKind::ADO,
    SyntaxKind::DO,
    SyntaxKind::IF,
    SyntaxKind::THEN,
    SyntaxKind::ELSE,
    SyntaxKind::LET,
    SyntaxKind::IN,
    SyntaxKind::CASE,
    SyntaxKind::OF,
    SyntaxKind::DATA,
    SyntaxKind::NEWTYPE,
    SyntaxKind::FORALL,
    SyntaxKind::TYPE,
    SyntaxKind::CLASS,
    SyntaxKind::INSTANCE,
    SyntaxKind::DERIVE,
    SyntaxKind::FOREIGN,
    SyntaxKind::INFIXL,
    SyntaxKind::INFIXR,
    SyntaxKind::INFIX,
    SyntaxKind::TRUE,
    SyntaxKind::FALSE,
]);

pub const RECORD_LABEL: TokenSet =
    TokenSet::new(&[SyntaxKind::STRING, SyntaxKind::RAW_STRING]).union(LOWER).union(KEYWORD);
