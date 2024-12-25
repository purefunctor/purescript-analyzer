use crate::SyntaxKind;

const LAST_TOKEN: usize = SyntaxKind::ERROR as usize;

pub struct TokenSet([u64; 3]);

impl TokenSet {
    pub const fn new(tokens: &[SyntaxKind]) -> TokenSet {
        let mut set = [0; 3];
        let mut index = 0;
        while index < tokens.len() {
            let token = tokens[index] as usize;
            debug_assert!(token <= LAST_TOKEN, "Invalid token");

            let i = token / 64;
            let b = token % 64;

            set[i] |= 1 << b;
            index += 1;
        }
        TokenSet(set)
    }

    pub const fn union(self, other: TokenSet) -> TokenSet {
        TokenSet([self.0[0] | other.0[0], self.0[1] | other.0[1], self.0[2] | other.0[2]])
    }

    pub const fn contains(&self, kind: SyntaxKind) -> bool {
        let token = kind as usize;
        debug_assert!(token <= LAST_TOKEN, "Invalid token");

        let i = token / 64;
        let b = token % 64;

        let m = 1 << b;
        self.0[i] & m != 0
    }
}

#[test]
fn test_token_set() {
    let set = TokenSet::new(&[
        SyntaxKind::WHITESPACE,
        SyntaxKind::LINE_COMMENT,
        SyntaxKind::BLOCK_COMMENT,
        SyntaxKind::MODULE,
    ]);
    assert!(set.contains(SyntaxKind::WHITESPACE));
    assert!(set.contains(SyntaxKind::LINE_COMMENT));
    assert!(set.contains(SyntaxKind::BLOCK_COMMENT));
    assert!(set.contains(SyntaxKind::MODULE));
    assert!(!set.contains(SyntaxKind::LEFT_PARENTHESIS));
}
