use position::Position;
use syntax::SyntaxKind;

use super::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Delimiter {
    Root,
    TopDecl,
    TopDeclHead,
    DeclGuard,
    Case,
    CaseBinders,
    CaseGuard,
    LambdaBinders,
    Paren,
    Brace,
    Square,
    If,
    Then,
    Property,
    Forall,
    Tick,
    Let,
    LetStmt,
    Where,
    Of,
    Do,
    Ado,
}

impl Delimiter {
    fn is_indented(&self) -> bool {
        matches!(
            self,
            Delimiter::Let
                | Delimiter::LetStmt
                | Delimiter::Where
                | Delimiter::Of
                | Delimiter::Do
                | Delimiter::Ado
        )
    }
}

struct Layout<'s, 't> {
    source: &'s str,
    tokens: &'t [Token],
    index: usize,
    stack: Vec<(Position, Delimiter)>,
    output: Vec<SyntaxKind>,
    whitespace: Vec<SyntaxKind>,
}

impl<'s, 't> Layout<'s, 't> {
    fn new(source: &'s str, tokens: &'t [Token]) -> Layout<'s, 't> {
        let index = 0;
        let stack = vec![(Position { offset: 0, line: 1, column: 1 }, Delimiter::Root)];
        let output = vec![];
        let whitespace = vec![];
        Layout { source, tokens, index, stack, output, whitespace }
    }

    fn insert(&mut self, token: Token, position: Position, next: Position) {
        Insert { layout: self, token, position, next }.call()
    }

    fn is_eof(&self) -> bool {
        self.index >= self.tokens.len()
    }

    fn step(&mut self) {
        assert!(self.index < self.tokens.len());

        let mut tokens = self.tokens[self.index..].iter().copied();

        let (token, position) = loop {
            let Some(token) = tokens.next() else {
                return;
            };
            self.index += 1;
            if token.kind.is_whitespace_or_comment() {
                self.whitespace.push(token.kind);
            } else {
                break (token, Position::from_source(&self.source, token.offset));
            }
        };

        let next = loop {
            let Some(token) = tokens.next() else {
                return;
            };
            if !token.kind.is_whitespace_or_comment() {
                break Position::from_source(&self.source, token.offset);
            }
        };

        self.insert(token, position, next);
    }

    fn finish(mut self) -> Vec<SyntaxKind> {
        while let Some((_, delimiter)) = self.stack.pop() {
            if delimiter.is_indented() {
                self.output.push(SyntaxKind::LAYOUT_END);
            }
        }
        self.output.push(SyntaxKind::END_OF_FILE);
        self.output
    }
}

struct Insert<'l, 's, 't> {
    layout: &'l mut Layout<'s, 't>,
    token: Token,
    position: Position,
    next: Position,
}

impl<'l, 's, 't> Insert<'l, 's, 't> {
    fn call(&mut self) {
        match self.token.kind {
            SyntaxKind::DATA => {
                self.insert_default();
                if self.is_top_declaration(self.position) {
                    self.push_stack(self.position, Delimiter::TopDecl);
                } else {
                    self.pop_stack_if(|delimiter| delimiter == Delimiter::Property);
                }
            }
            SyntaxKind::CLASS => {
                self.insert_default();
                if self.is_top_declaration(self.position) {
                    self.push_stack(self.position, Delimiter::TopDeclHead);
                } else {
                    self.pop_stack_if(|delimiter| delimiter == Delimiter::Property);
                }
            }
            SyntaxKind::WHERE => match &self.layout.stack[..] {
                [.., (_, Delimiter::TopDeclHead)] => {
                    self.pop_stack();
                    self.insert_token(SyntaxKind::WHERE);
                    self.insert_start(Delimiter::Where);
                }
                [.., (_, Delimiter::Property)] => {
                    self.pop_stack();
                    self.insert_token(SyntaxKind::WHERE);
                }
                _ => {
                    self.collapse_and_commit(Self::where_p);
                    self.insert_token(SyntaxKind::WHERE);
                    self.insert_start(Delimiter::Where);
                }
            },
            SyntaxKind::IN => {
                let collapse = self.collapse(Self::in_p);
                match collapse.preview(self.layout) {
                    [.., (_, Delimiter::Ado), (_, Delimiter::LetStmt)] => {
                        collapse.commit(self.layout);
                        self.pop_stack();
                        self.pop_stack();
                        self.insert_end();
                        self.insert_end();
                        self.insert_token(SyntaxKind::IN);
                    }
                    [.., (_, delimiter)] if delimiter.is_indented() => {
                        collapse.commit(self.layout);
                        self.pop_stack();
                        self.insert_end();
                        self.insert_token(SyntaxKind::IN);
                    }
                    _ => {
                        self.insert_default();
                        self.pop_stack_if(|delimiter| delimiter == Delimiter::Property);
                    }
                }
            }
            SyntaxKind::LET => {
                self.insert_keyword_property(|this| match &this.layout.stack[..] {
                    [.., (position, Delimiter::Do)] if position.column == this.position.column => {
                        this.insert_start(Delimiter::LetStmt);
                    }
                    [.., (position, Delimiter::Ado)] if position.column == this.position.column => {
                        this.insert_start(Delimiter::LetStmt);
                    }
                    _ => {
                        this.insert_start(Delimiter::Let);
                    }
                });
            }
            SyntaxKind::DO => {
                self.insert_keyword_property(|this| {
                    this.insert_start(Delimiter::Do);
                });
            }

            SyntaxKind::ADO => {
                self.insert_keyword_property(|this| {
                    this.insert_start(Delimiter::Ado);
                });
            }
            SyntaxKind::CASE => {
                self.insert_keyword_property(|this| {
                    this.push_stack(this.position, Delimiter::Case);
                });
            }
            SyntaxKind::OF => {
                let collapse = self.collapse(Self::indented_p);
                match collapse.preview(self.layout) {
                    [.., (_, Delimiter::Case)] => {
                        collapse.commit(self.layout);
                        self.pop_stack();
                        self.insert_token(SyntaxKind::OF);
                        self.insert_start(Delimiter::Of);
                        self.push_stack(self.next, Delimiter::CaseBinders);
                    }
                    _ => {
                        self.insert_default();
                        self.pop_stack_if(|delimiter| delimiter == Delimiter::Property);
                    }
                }
            }
            SyntaxKind::IF => {
                self.insert_keyword_property(|this| {
                    this.push_stack(this.position, Delimiter::If);
                });
            }
            SyntaxKind::THEN => {
                let collapse = self.collapse(Self::indented_p);
                match collapse.preview(self.layout) {
                    [.., (_, Delimiter::If)] => {
                        collapse.commit(self.layout);
                        self.pop_stack();
                        self.insert_token(SyntaxKind::THEN);
                        self.push_stack(self.position, Delimiter::Then);
                    }
                    _ => {
                        self.insert_default();
                        self.pop_stack_if(|delimiter| delimiter == Delimiter::Property);
                    }
                }
            }
            SyntaxKind::ELSE => {
                let collapse = self.collapse(Self::indented_p);
                match collapse.preview(self.layout) {
                    [.., (_, Delimiter::Then)] => {
                        collapse.commit(self.layout);
                        self.pop_stack();
                        self.insert_token(SyntaxKind::ELSE);
                    }
                    _ => {
                        self.collapse_and_commit(Self::offside_p);
                        if self.is_top_declaration(self.position) {
                            self.insert_token(SyntaxKind::ELSE);
                        } else {
                            self.insert_sep();
                            self.insert_token(SyntaxKind::ELSE);
                            self.pop_stack_if(|delimiter| delimiter == Delimiter::Property);
                        }
                    }
                }
            }
            SyntaxKind::FORALL => {
                self.insert_keyword_property(|this| {
                    this.push_stack(this.position, Delimiter::Forall);
                });
            }
            SyntaxKind::BACKSLASH => {
                self.insert_default();
                self.push_stack(self.position, Delimiter::LambdaBinders);
            }
            SyntaxKind::RIGHT_ARROW => {
                self.collapse_and_commit(Self::arrow_p);
                self.pop_stack_if(|delimiter| {
                    matches!(
                        delimiter,
                        Delimiter::CaseBinders | Delimiter::CaseGuard | Delimiter::LambdaBinders
                    )
                });
                self.insert_token(SyntaxKind::RIGHT_ARROW);
            }
            SyntaxKind::EQUALS => {
                let collapse = self.collapse(Self::equals_p);
                match collapse.preview(self.layout) {
                    [.., (_, Delimiter::DeclGuard)] => {
                        collapse.commit(self.layout);
                        self.pop_stack();
                        self.insert_token(SyntaxKind::EQUALS);
                    }
                    _ => {
                        self.insert_default();
                    }
                }
            }
            SyntaxKind::PIPE => {
                let collapse = self.collapse(Self::offside_end_p);
                match collapse.preview(self.layout) {
                    [.., (_, Delimiter::Of)] => {
                        collapse.commit(self.layout);
                        self.push_stack(self.position, Delimiter::CaseGuard);
                        self.insert_token(SyntaxKind::PIPE);
                    }
                    [.., (_, Delimiter::Let | Delimiter::LetStmt | Delimiter::Where)] => {
                        collapse.commit(self.layout);
                        self.push_stack(self.position, Delimiter::DeclGuard);
                        self.insert_token(SyntaxKind::PIPE);
                    }
                    _ => {
                        self.insert_default();
                    }
                }
            }
            SyntaxKind::TICK => {
                let collapse = self.collapse(Self::indented_p);
                match collapse.preview(self.layout) {
                    [.., (_, Delimiter::Tick)] => {
                        collapse.commit(self.layout);
                        self.pop_stack();
                        self.insert_token(SyntaxKind::TICK);
                    }
                    _ => {
                        self.collapse_and_commit(Self::offside_end_p);
                        self.insert_sep();
                        self.insert_token(SyntaxKind::TICK);
                        self.push_stack(self.position, Delimiter::Tick);
                    }
                }
            }
            SyntaxKind::COMMA => {
                self.collapse_and_commit(Self::indented_p);
                if let Some((_, Delimiter::Brace)) = self.layout.stack.last() {
                    self.insert_token(SyntaxKind::COMMA);
                    self.push_stack(self.position, Delimiter::Property);
                } else {
                    self.insert_token(SyntaxKind::COMMA);
                }
            }
            SyntaxKind::DOT => {
                if let Some((_, Delimiter::Forall)) = self.layout.stack.last() {
                    self.pop_stack();
                } else {
                    self.push_stack(self.position, Delimiter::Property);
                }
            }
            SyntaxKind::LEFT_PARENTHESIS => {
                self.insert_default();
                self.push_stack(self.position, Delimiter::Paren);
            }
            SyntaxKind::LEFT_CURLY => {
                self.insert_default();
                self.push_stack(self.position, Delimiter::Brace);
                self.push_stack(self.position, Delimiter::Property);
            }
            SyntaxKind::LEFT_SQUARE => {
                self.insert_default();
                self.push_stack(self.position, Delimiter::Square);
            }
            SyntaxKind::RIGHT_PARENTHESIS => {
                self.collapse_and_commit(Self::indented_p);
                self.pop_stack_if(|delimiter| delimiter == Delimiter::Paren);
                self.insert_token(SyntaxKind::RIGHT_PARENTHESIS);
            }
            SyntaxKind::RIGHT_CURLY => {
                self.collapse_and_commit(Self::indented_p);
                self.pop_stack_if(|delimiter| delimiter == Delimiter::Property);
                self.pop_stack_if(|delimiter| delimiter == Delimiter::Brace);
                self.insert_token(SyntaxKind::RIGHT_CURLY);
            }
            SyntaxKind::RIGHT_SQUARE => {
                self.collapse_and_commit(Self::indented_p);
                self.pop_stack_if(|delimiter| delimiter == Delimiter::Square);
                self.insert_token(SyntaxKind::RIGHT_SQUARE);
            }
            SyntaxKind::STRING | SyntaxKind::RAW_STRING | SyntaxKind::SOURCE_IDENTIFIER => {
                self.insert_default();
                self.pop_stack_if(|delimiter| delimiter == Delimiter::Property);
            }
            SyntaxKind::SOURCE_OPERATOR => {
                self.collapse_and_commit(Self::offside_end_p);
                self.insert_sep();
                self.insert_token(SyntaxKind::SOURCE_OPERATOR);
            }
            _ => {
                self.insert_default();
            }
        }
    }

    fn is_top_declaration(&self, position: Position) -> bool {
        matches!(&self.layout.stack[..], [(_, Delimiter::Root), (leading, Delimiter::Where)] if position.column == leading.column)
    }

    fn insert_default(&mut self) {
        self.collapse_and_commit(Self::offside_p);
        self.insert_sep();
        self.insert_token(self.token.kind);
    }

    fn insert_start(&mut self, delimiter: Delimiter) {
        if let Some((previous, _)) =
            self.layout.stack.iter().rfind(|(_, delimiter)| delimiter.is_indented())
        {
            if self.next.column <= previous.column {
                return;
            }
        }
        self.push_stack(self.next, delimiter);
        self.insert_token(SyntaxKind::LAYOUT_START);
    }

    #[inline]
    fn insert_delimiter_token(&mut self) {
        self.layout.output.push(SyntaxKind::LAYOUT_DELIMITER);
        self.layout.output.extend(self.layout.whitespace.drain(..));
    }

    fn insert_sep(&mut self) {
        match &self.layout.stack[..] {
            [.., (position, Delimiter::TopDecl)] if self.sep_p(*position) => {
                self.pop_stack();
                self.insert_delimiter_token();
            }
            [.., (position, Delimiter::TopDeclHead)] if self.sep_p(*position) => {
                self.pop_stack();
                self.insert_delimiter_token();
            }
            [.., (position, delimiter)] if self.indent_sep_p(*position, *delimiter) => {
                match delimiter {
                    Delimiter::Of => {
                        self.insert_delimiter_token();
                        self.push_stack(self.position, Delimiter::CaseBinders);
                    }
                    _ => {
                        self.insert_delimiter_token();
                    }
                }
            }
            _ => {}
        }
    }

    fn insert_end(&mut self) {
        self.layout.output.push(SyntaxKind::LAYOUT_END);
        self.layout.output.extend(self.layout.whitespace.drain(..));
    }

    fn insert_keyword_property(&mut self, callback: impl Fn(&mut Self)) {
        self.insert_default();
        if let Some((_, Delimiter::Property)) = self.layout.stack.last() {
            self.pop_stack();
        } else {
            callback(self);
        }
    }

    // region: utils
    #[inline]
    fn insert_token(&mut self, token: SyntaxKind) {
        self.layout.output.extend(self.layout.whitespace.drain(..));
        self.layout.output.push(token);
    }

    #[inline]
    fn push_stack(&mut self, position: Position, delimiter: Delimiter) {
        self.layout.stack.push((position, delimiter));
    }

    #[inline]
    fn pop_stack(&mut self) {
        self.layout.stack.pop();
    }

    fn pop_stack_if(&mut self, predicate: impl Fn(Delimiter) -> bool) {
        match self.layout.stack.last() {
            Some((_, delimiter)) => {
                if predicate(*delimiter) {
                    self.layout.stack.pop();
                }
            }
            _ => unreachable!(),
        }
    }
    // endregion: utils

    // region: collapse
    fn collapse(&self, predicate: impl Fn(&Self, Position, Delimiter) -> bool) -> Collapse {
        let mut index = self.layout.stack.len();
        let mut count = 0;
        for (position, delimiter) in self.layout.stack.iter().rev() {
            if predicate(self, *position, *delimiter) {
                index = index.saturating_sub(1);
                if delimiter.is_indented() {
                    count += 1;
                }
            } else {
                break;
            }
        }
        Collapse { index, count }
    }

    fn collapse_and_commit(&mut self, predicate: impl Fn(&Self, Position, Delimiter) -> bool) {
        self.collapse(predicate).commit(self.layout);
    }
    // endregion: collapse

    // region: predicates
    fn indented_p(&self, _: Position, delimiter: Delimiter) -> bool {
        delimiter.is_indented()
    }

    fn offside_p(&self, position: Position, delimiter: Delimiter) -> bool {
        delimiter.is_indented() && self.position.column < position.column
    }

    fn offside_end_p(&self, position: Position, delimiter: Delimiter) -> bool {
        delimiter.is_indented() && self.position.column <= position.column
    }

    fn indent_sep_p(&self, position: Position, delimiter: Delimiter) -> bool {
        delimiter.is_indented() && self.sep_p(position)
    }

    fn sep_p(&self, position: Position) -> bool {
        self.position.column == position.column && self.position.line != position.line
    }

    fn where_p(&self, position: Position, delimiter: Delimiter) -> bool {
        if let Delimiter::Do = delimiter {
            true
        } else {
            self.offside_end_p(position, delimiter)
        }
    }

    fn in_p(&self, _: Position, delimiter: Delimiter) -> bool {
        if let Delimiter::Let | Delimiter::Ado = delimiter {
            false
        } else {
            delimiter.is_indented()
        }
    }

    fn arrow_p(&self, position: Position, delimiter: Delimiter) -> bool {
        match delimiter {
            Delimiter::Do => true,
            Delimiter::Of => false,
            _ => self.offside_end_p(position, delimiter),
        }
    }

    fn equals_p(&self, _: Position, delimiter: Delimiter) -> bool {
        matches!(delimiter, Delimiter::Where | Delimiter::Let | Delimiter::LetStmt)
    }
    // endregion: predicates
}

struct Collapse {
    index: usize,
    count: usize,
}

impl Collapse {
    fn preview<'l>(&self, layout: &'l Layout) -> &'l [(Position, Delimiter)] {
        &layout.stack[..self.index]
    }

    fn commit(self, layout: &mut Layout) {
        layout.stack.truncate(self.index);
        for _ in 0..self.count {
            layout.output.push(SyntaxKind::LAYOUT_END);
        }
    }
}

pub(crate) fn layout(source: &str, tokens: &[Token]) -> Vec<SyntaxKind> {
    let mut layout = Layout::new(source, tokens);
    while !layout.is_eof() {
        layout.step();
    }
    layout.finish()
}
