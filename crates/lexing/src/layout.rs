//! Implements the layout algorithm.

use position::Position;
use syntax::SyntaxKind;

use crate::lexed::Lexed;

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

pub(crate) struct Layout<'a> {
    lexed: &'a Lexed<'a>,
    index: usize,

    stack: Vec<(Position, Delimiter)>,
    output: Vec<SyntaxKind>,
    whitespace: Vec<SyntaxKind>,
}

impl<'a> Layout<'a> {
    pub(crate) fn new(lexed: &'a Lexed<'a>) -> Layout<'a> {
        let index = 0;
        let stack = vec![(Position { offset: 0, line: 0, column: 0 }, Delimiter::Root)];
        let output = vec![];
        let whitespace = vec![];
        Layout { lexed, index, stack, output, whitespace }
    }

    pub(crate) fn is_eof(&self) -> bool {
        self.lexed.kind(self.index).is_end()
    }

    pub(crate) fn take_token(&mut self) {
        let (token, position) = loop {
            let kind = self.lexed.kind(self.index);
            let position = self.lexed.position(self.index);
            if kind.is_whitespace_or_comment() {
                self.whitespace.push(kind);
                self.index += 1;
            } else {
                break (kind, position);
            }
            if self.is_eof() {
                return;
            }
        };

        let mut index = self.index;
        let next = loop {
            index += 1;
            if !self.lexed.kind(index).is_whitespace_or_comment() {
                break self.lexed.position(index);
            }
        };

        self.insert(token, position, next);
        self.index += 1;
    }

    pub(crate) fn finish(mut self) -> Vec<SyntaxKind> {
        while let Some((_, delimiter)) = self.stack.pop() {
            if delimiter.is_indented() {
                self.output.push(SyntaxKind::LAYOUT_END);
            }
        }
        self.output.extend(self.whitespace.drain(..));
        self.output.push(SyntaxKind::END_OF_FILE);
        self.output
    }
}

impl Layout<'_> {
    fn insert(&mut self, token: SyntaxKind, position: Position, next: Position) {
        InsertWithLayout::new(self, token, position, next).invoke();
    }
}

struct InsertWithLayout<'a, 'b> {
    layout: &'a mut Layout<'b>,
    token: SyntaxKind,
    position: Position,
    next: Position,
}

impl<'a, 'b> InsertWithLayout<'a, 'b> {
    fn new(
        machine: &'a mut Layout<'b>,
        token: SyntaxKind,
        position: Position,
        next: Position,
    ) -> InsertWithLayout<'a, 'b> {
        InsertWithLayout { layout: machine, token, position, next }
    }

    fn invoke(&mut self) {
        match dbg!(self.token) {
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
                    self.insert_token(self.token);
                    self.insert_start(Delimiter::Where);
                }
                [.., (_, Delimiter::Property)] => {
                    self.pop_stack();
                    self.insert_token(self.token);
                }
                _ => {
                    self.collapse_and_commit(InsertWithLayout::where_p);
                    self.insert_token(self.token);
                    self.insert_start(Delimiter::Where);
                }
            },

            SyntaxKind::IN => {
                let collapse = self.collapse(InsertWithLayout::in_p);
                match collapse.preview(self.layout) {
                    [.., (_, Delimiter::Ado), (_, Delimiter::LetStmt)] => {
                        collapse.commit(self.layout);
                        self.pop_stack();
                        self.pop_stack();
                        self.insert_end();
                        self.insert_end();
                        self.insert_token(self.token);
                    }
                    [.., (_, delimiter)] if delimiter.is_indented() => {
                        collapse.commit(self.layout);
                        self.pop_stack();
                        self.insert_end();
                        self.insert_token(self.token);
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
                let collapse = self.collapse(InsertWithLayout::indented_p);
                match collapse.preview(self.layout) {
                    [.., (_, Delimiter::Case)] => {
                        collapse.commit(self.layout);
                        self.pop_stack();
                        self.insert_token(self.token);
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
                let collapse = self.collapse(InsertWithLayout::indented_p);
                match collapse.preview(self.layout) {
                    [.., (_, Delimiter::If)] => {
                        collapse.commit(self.layout);
                        self.pop_stack();
                        self.insert_token(self.token);
                        self.push_stack(self.position, Delimiter::Then);
                    }
                    _ => {
                        self.insert_default();
                        self.pop_stack_if(|delimiter| delimiter == Delimiter::Property);
                    }
                }
            }

            SyntaxKind::ELSE => {
                let collapse = self.collapse(InsertWithLayout::indented_p);
                match collapse.preview(self.layout) {
                    [.., (_, Delimiter::Then)] => {
                        collapse.commit(self.layout);
                        self.pop_stack();
                        self.insert_token(self.token);
                    }
                    _ => {
                        self.collapse_and_commit(InsertWithLayout::offside_p);
                        if self.is_top_declaration(self.position) {
                            self.insert_token(self.token);
                        } else {
                            self.insert_sep();
                            self.insert_token(self.token);
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
                self.collapse_and_commit(InsertWithLayout::arrow_p);
                self.pop_stack_if(|delimiter| {
                    matches!(
                        delimiter,
                        Delimiter::CaseBinders | Delimiter::CaseGuard | Delimiter::LambdaBinders
                    )
                });
                self.insert_token(self.token);
            }

            SyntaxKind::EQUAL => {
                let collapse = self.collapse(InsertWithLayout::equals_p);
                match collapse.preview(self.layout) {
                    [.., (_, Delimiter::DeclGuard)] => {
                        collapse.commit(self.layout);
                        self.pop_stack();
                        self.insert_token(self.token);
                    }
                    _ => {
                        self.insert_default();
                    }
                }
            }

            SyntaxKind::PIPE => {
                let collapse = self.collapse(InsertWithLayout::offside_end_p);
                match collapse.preview(self.layout) {
                    [.., (_, Delimiter::Of)] => {
                        collapse.commit(self.layout);
                        self.push_stack(self.position, Delimiter::CaseGuard);
                        self.insert_token(self.token);
                    }
                    [.., (_, Delimiter::Let | Delimiter::LetStmt | Delimiter::Where)] => {
                        collapse.commit(self.layout);
                        self.push_stack(self.position, Delimiter::DeclGuard);
                        self.insert_token(self.token);
                    }
                    _ => {
                        self.insert_default();
                    }
                }
            }

            SyntaxKind::TICK => {
                let collapse = self.collapse(InsertWithLayout::indented_p);
                match collapse.preview(self.layout) {
                    [.., (_, Delimiter::Tick)] => {
                        collapse.commit(self.layout);
                        self.pop_stack();
                        self.insert_token(self.token);
                    }
                    _ => {
                        self.collapse_and_commit(InsertWithLayout::offside_end_p);
                        self.insert_sep();
                        self.insert_token(self.token);
                        self.push_stack(self.position, Delimiter::Tick);
                    }
                }
            }

            SyntaxKind::COMMA => {
                self.collapse_and_commit(InsertWithLayout::indented_p);
                if let Some((_, Delimiter::Brace)) = self.layout.stack.last() {
                    self.insert_token(self.token);
                    self.push_stack(self.position, Delimiter::Property);
                } else {
                    self.insert_token(self.token);
                }
            }

            SyntaxKind::PERIOD => {
                self.insert_default();
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
                self.collapse_and_commit(InsertWithLayout::indented_p);
                self.pop_stack_if(|delimiter| delimiter == Delimiter::Paren);
                self.insert_token(self.token);
            }

            SyntaxKind::RIGHT_CURLY => {
                self.collapse_and_commit(InsertWithLayout::indented_p);
                self.pop_stack_if(|delimiter| delimiter == Delimiter::Property);
                self.pop_stack_if(|delimiter| delimiter == Delimiter::Brace);
                self.insert_token(self.token);
            }

            SyntaxKind::RIGHT_SQUARE => {
                self.collapse_and_commit(InsertWithLayout::indented_p);
                self.pop_stack_if(|delimiter| delimiter == Delimiter::Square);
                self.insert_token(self.token);
            }

            SyntaxKind::STRING | SyntaxKind::RAW_STRING | SyntaxKind::LOWER => {
                self.insert_default();
                self.pop_stack_if(|delimiter| delimiter == Delimiter::Property);
            }

            k if k.is_operator() => {
                self.collapse_and_commit(InsertWithLayout::offside_end_p);
                self.insert_sep();
                self.insert_token(self.token);
            }

            _ => {
                self.insert_default();
            }
        }
    }

    fn is_top_declaration(&self, token_position: Position) -> bool {
        matches!(&self.layout.stack[..],
            [(_, Delimiter::Root), (layout_position, Delimiter::Where)]
                if token_position.column == layout_position.column
        )
    }

    fn insert_default(&mut self) {
        self.collapse_and_commit(Self::offside_p);
        self.insert_sep();
        self.insert_token(self.token);
    }

    fn insert_start(&mut self, delimiter: Delimiter) {
        if let Some((past_position, _)) =
            self.layout.stack.iter().rfind(|(_, delimiter)| delimiter.is_indented())
        {
            if self.next.column <= past_position.column {
                return;
            }
        }

        self.push_stack(self.next, delimiter);
        self.insert_token(SyntaxKind::LAYOUT_START);
    }

    fn insert_sep(&mut self) {
        match &self.layout.stack[..] {
            [.., (position, Delimiter::TopDecl)] if self.sep_p(*position) => {
                self.pop_stack();
                self.insert_separator();
            }
            [.., (position, Delimiter::TopDeclHead)] if self.sep_p(*position) => {
                self.pop_stack();
                self.insert_separator();
            }
            [.., (position, delimiter)] if self.indent_sep_p(*position, *delimiter) => {
                match delimiter {
                    Delimiter::Of => {
                        self.insert_separator();
                        self.push_stack(self.position, Delimiter::CaseBinders);
                    }
                    _ => {
                        self.insert_separator();
                    }
                }
            }
            _ => (),
        }
    }

    fn insert_keyword_property(&mut self, callback: impl Fn(&mut Self)) {
        self.insert_default();
        if let Some((_, Delimiter::Property)) = self.layout.stack.last() {
            self.pop_stack();
        } else {
            callback(self)
        }
    }

    fn insert_separator(&mut self) {
        self.layout.output.push(SyntaxKind::LAYOUT_SEPARATOR);
        self.layout.output.extend(self.layout.whitespace.drain(..));
    }

    fn insert_end(&mut self) {
        self.layout.output.push(SyntaxKind::LAYOUT_END);
        self.layout.output.extend(self.layout.whitespace.drain(..));
    }

    fn insert_token(&mut self, token: SyntaxKind) {
        self.layout.output.extend(self.layout.whitespace.drain(..));
        self.layout.output.push(token);
    }

    fn push_stack(&mut self, position: Position, delimiter: Delimiter) {
        self.layout.stack.push((position, delimiter));
    }

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
            None => unreachable!(),
        };
    }

    fn collapse(&self, predicate: impl Fn(&Self, Position, Delimiter) -> bool) -> Collapse {
        let mut stack_len = self.layout.stack.len();
        let mut end_tokens = 0;
        for (position, delimiter) in self.layout.stack.iter().rev() {
            if predicate(self, *position, *delimiter) {
                stack_len = stack_len.saturating_sub(1);
                if delimiter.is_indented() {
                    end_tokens += 1;
                }
            } else {
                break;
            }
        }
        Collapse { stack_len, end_tokens }
    }

    fn collapse_and_commit(&mut self, predicate: impl Fn(&Self, Position, Delimiter) -> bool) {
        self.collapse(predicate).commit(self.layout);
    }

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
}

struct Collapse {
    stack_len: usize,
    end_tokens: usize,
}

impl Collapse {
    fn preview<'a>(&self, machine: &'a Layout) -> &'a [(Position, Delimiter)] {
        &machine.stack[..self.stack_len]
    }

    fn commit(self, machine: &mut Layout) {
        machine.stack.truncate(self.stack_len);
        for _ in 0..self.end_tokens {
            machine.output.push(SyntaxKind::LAYOUT_END);
        }
    }
}
