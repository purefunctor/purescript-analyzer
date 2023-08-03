use syntax::SyntaxKind;

use crate::{lexed::Lexed, position::Position};

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

pub struct Machine<'a> {
    lexed: &'a Lexed<'a>,
    now_index: usize,

    stack: Vec<(Position, Delimiter)>,
    output: Vec<SyntaxKind>,
}

impl<'a> Machine<'a> {
    pub(crate) fn new(lexed: &'a Lexed<'a>) -> Machine<'a> {
        let now_index = 0;
        let stack = vec![(Position { offset: 0, line: 0, column: 0 }, Delimiter::Root)];
        let output = vec![];
        Machine { lexed, now_index, stack, output }
    }

    pub(crate) fn is_eof(&self) -> bool {
        self.now_index >= self.lexed.len()
    }

    pub(crate) fn take_token(&mut self) {
        loop {
            if self.lexed.kind(self.now_index).is_whitespace_or_comment() {
                self.now_index += 1;
            } else {
                break;
            }
            if self.is_eof() {
                return;
            }
        }

        let now_token = self.lexed.kind(self.now_index);
        let now_position = self.lexed.position(self.now_index);

        let mut next_index = self.now_index;
        let next_position = loop {
            next_index += 1;
            if next_index >= self.lexed.len() {
                break self.lexed.eof_position();
            }
            if !self.lexed.kind(next_index).is_whitespace_or_comment() {
                break self.lexed.position(next_index);
            }
        };

        self.insert_with_layout(now_token, now_position, next_position);
        self.now_index += 1;
    }

    pub(crate) fn finalize(mut self) -> Vec<SyntaxKind> {
        while let Some((_, delimiter)) = self.stack.pop() {
            if delimiter.is_indented() {
                self.output.push(SyntaxKind::LayoutEnd);
            }
        }
        self.output
    }
}

impl Machine<'_> {
    fn insert_with_layout(
        &mut self,
        now_token: SyntaxKind,
        now_position: Position,
        next_position: Position,
    ) {
        InsertWithLayout::new(self, now_token, now_position, next_position).invoke();
    }
}

struct InsertWithLayout<'a, 'b> {
    machine: &'a mut Machine<'b>,
    now_token: SyntaxKind,
    now_position: Position,
    next_position: Position,
}

impl<'a, 'b> InsertWithLayout<'a, 'b> {
    fn new(
        machine: &'a mut Machine<'b>,
        now_token: SyntaxKind,
        now_position: Position,
        next_position: Position,
    ) -> InsertWithLayout<'a, 'b> {
        InsertWithLayout { machine, now_token, now_position, next_position }
    }

    fn invoke(&mut self) {
        match self.now_token {
            SyntaxKind::DataKw => {
                self.insert_default();
                if self.is_top_declaration(self.now_position) {
                    self.push_stack(self.now_position, Delimiter::TopDecl);
                } else {
                    self.pop_stack_if(|delimiter| delimiter == Delimiter::Property);
                }
            }

            SyntaxKind::ClassKw => {
                self.insert_default();
                if self.is_top_declaration(self.now_position) {
                    self.push_stack(self.now_position, Delimiter::TopDeclHead);
                } else {
                    self.pop_stack_if(|delimiter| delimiter == Delimiter::Property);
                }
            }

            SyntaxKind::WhereKw => match &self.machine.stack[..] {
                [.., (_, Delimiter::TopDeclHead)] => {
                    self.pop_stack();
                    self.insert_token(self.now_token);
                    self.insert_start(Delimiter::Where);
                }
                [.., (_, Delimiter::Property)] => {
                    self.pop_stack();
                    self.insert_token(self.now_token);
                }
                _ => {
                    self.collapse_and_commit(InsertWithLayout::where_p);
                }
            },

            SyntaxKind::InKw => match self.collapse_and_preview(InsertWithLayout::in_p) {
                [.., (_, Delimiter::Ado), (_, Delimiter::LetStmt)] => {
                    self.pop_stack();
                    self.pop_stack();
                    self.insert_end();
                    self.insert_end();
                    self.insert_token(self.now_token);
                }
                [.., (_, delimiter)] if delimiter.is_indented() => {
                    self.pop_stack();
                    self.insert_end();
                    self.insert_token(self.now_token);
                }
                _ => {
                    self.insert_default();
                    self.pop_stack_if(|delimiter| delimiter == Delimiter::Property);
                }
            },

            SyntaxKind::LetKw => {
                self.insert_keyword_property(|this| match &this.machine.stack[..] {
                    [.., (position, Delimiter::Do)]
                        if position.column == this.now_position.column =>
                    {
                        this.insert_start(Delimiter::LetStmt);
                    }
                    [.., (position, Delimiter::Ado)]
                        if position.column == this.now_position.column =>
                    {
                        this.insert_start(Delimiter::LetStmt);
                    }
                    _ => {
                        this.insert_start(Delimiter::Let);
                    }
                });
            }

            SyntaxKind::DoKw => {
                self.insert_keyword_property(|this| {
                    this.insert_start(Delimiter::Do);
                });
            }

            SyntaxKind::AdoKw => {
                self.insert_keyword_property(|this| {
                    this.insert_start(Delimiter::Ado);
                });
            }

            SyntaxKind::CaseKw => {
                self.insert_keyword_property(|this| {
                    this.push_stack(this.now_position, Delimiter::Case);
                });
            }

            SyntaxKind::OfKw => match self.collapse_and_preview(InsertWithLayout::indented_p) {
                [.., (_, Delimiter::Case)] => {
                    self.pop_stack();
                    self.insert_token(self.now_token);
                    self.insert_start(Delimiter::Of);
                    self.push_stack(self.next_position, Delimiter::CaseBinders);
                }
                _ => {
                    self.insert_default();
                    self.pop_stack_if(|delimiter| delimiter == Delimiter::Property);
                }
            },

            SyntaxKind::IfKw => {
                self.insert_keyword_property(|this| {
                    this.push_stack(this.now_position, Delimiter::If);
                });
            }

            SyntaxKind::ThenKw => match self.collapse_and_preview(InsertWithLayout::indented_p) {
                [.., (_, Delimiter::If)] => {
                    self.pop_stack();
                    self.insert_token(self.now_token);
                    self.push_stack(self.now_position, Delimiter::Then);
                }
                _ => {
                    self.insert_default();
                    self.pop_stack_if(|delimiter| delimiter == Delimiter::Property);
                }
            },

            SyntaxKind::ElseKw => match self.collapse_and_preview(InsertWithLayout::indented_p) {
                [.., (_, Delimiter::Then)] => {
                    self.pop_stack();
                    self.insert_token(self.now_token);
                }
                _ => {
                    self.collapse_and_commit(InsertWithLayout::offside_p);
                    if self.is_top_declaration(self.now_position) {
                        self.insert_token(self.now_token);
                    } else {
                        self.insert_sep();
                        self.insert_token(self.now_token);
                        self.pop_stack_if(|delimiter| delimiter == Delimiter::Property);
                    }
                }
            }

            _ => {
                self.insert_default();
            }
        }
    }

    fn is_top_declaration(&self, token_position: Position) -> bool {
        matches!(&self.machine.stack[..],
            [(_, Delimiter::Root), (layout_position, Delimiter::Where)]
                if token_position.column == layout_position.column
        )
    }

    fn insert_default(&mut self) {
        self.collapse_and_commit(Self::offside_p);
        self.insert_sep();
        self.insert_token(self.now_token);
    }

    fn insert_start(&mut self, delimiter: Delimiter) {
        if let Some((past_position, _)) =
            self.machine.stack.iter().rfind(|(_, delimiter)| delimiter.is_indented())
        {
            if self.next_position.column <= past_position.column {
                return;
            }
        }

        self.push_stack(self.next_position, delimiter);
        self.insert_token(SyntaxKind::LayoutStart);
    }

    fn insert_sep(&mut self) {
        match &self.machine.stack[..] {
            [.., (position, Delimiter::TopDecl)] if self.sep_p(*position) => {
                self.pop_stack();
                self.insert_token(SyntaxKind::LayoutSep);
            }
            [.., (position, Delimiter::TopDeclHead)] if self.sep_p(*position) => {
                self.pop_stack();
                self.insert_token(SyntaxKind::LayoutSep);
            }
            [.., (position, delimiter)] if self.indent_sep_p(*position, *delimiter) => {
                match delimiter {
                    Delimiter::Of => {
                        self.insert_token(SyntaxKind::LayoutSep);
                        self.push_stack(self.now_position, Delimiter::CaseBinders);
                    }
                    _ => {
                        self.insert_token(SyntaxKind::LayoutSep);
                    }
                }
            }
            _ => (),
        }
    }

    fn insert_keyword_property(&mut self, callback: impl Fn(&mut Self)) {
        self.insert_default();
        if let Some((_, Delimiter::Property)) = self.machine.stack.last() {
            self.pop_stack();
        } else {
            callback(self)
        }
    }

    fn insert_end(&mut self) {
        self.insert_token(SyntaxKind::LayoutEnd);
    }

    fn insert_token(&mut self, token: SyntaxKind) {
        self.machine.output.push(token);
    }

    fn push_stack(&mut self, position: Position, delimiter: Delimiter) {
        self.machine.stack.push((position, delimiter));
    }

    fn pop_stack(&mut self) {
        self.machine.stack.pop();
    }

    fn pop_stack_if(&mut self, predicate: impl Fn(Delimiter) -> bool) {
        match self.machine.stack.last() {
            Some((_, delimiter)) => {
                if predicate(*delimiter) {
                    self.machine.stack.pop();
                }
            }
            None => unreachable!(),
        };
    }

    fn collapse(&self, predicate: impl Fn(&Self, Position, Delimiter) -> bool) -> Collapse {
        let mut stack_len = self.machine.stack.len();
        let mut end_tokens = 0;
        loop {
            match self.machine.stack.last() {
                Some((position, delimiter)) => {
                    if predicate(self, *position, *delimiter) {
                        stack_len = stack_len.saturating_sub(1);
                        if delimiter.is_indented() {
                            end_tokens += 1;
                        }
                    } else {
                        break;
                    }
                }
                None => unreachable!(),
            }
        }
        Collapse { stack_len, end_tokens }
    }

    fn collapse_and_commit(&mut self, predicate: impl Fn(&Self, Position, Delimiter) -> bool) {
        self.collapse(predicate).commit(self.machine);
    }

    fn collapse_and_preview(
        &mut self,
        predicate: impl Fn(&Self, Position, Delimiter) -> bool,
    ) -> &[(Position, Delimiter)] {
        self.collapse(predicate).preview(self.machine)
    }

    fn indented_p(&self, _: Position, delimiter: Delimiter) -> bool {
        delimiter.is_indented()
    }

    fn offside_p(&self, position: Position, delimiter: Delimiter) -> bool {
        delimiter.is_indented() && self.now_position.column < position.column
    }

    fn offside_end_p(&self, position: Position, delimiter: Delimiter) -> bool {
        delimiter.is_indented() && self.now_position.column <= position.column
    }

    fn indent_sep_p(&self, position: Position, delimiter: Delimiter) -> bool {
        delimiter.is_indented() && self.sep_p(position)
    }

    fn sep_p(&self, position: Position) -> bool {
        self.now_position.column == position.column && self.now_position.line != position.line
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
}

struct Collapse {
    stack_len: usize,
    end_tokens: usize,
}

impl Collapse {
    fn preview<'a>(&self, machine: &'a Machine) -> &'a [(Position, Delimiter)] {
        &machine.stack[..self.stack_len]
    }

    fn commit(self, machine: &mut Machine) {
        machine.stack.truncate(self.stack_len);
        for _ in 0..self.end_tokens {
            machine.output.push(SyntaxKind::LayoutEnd);
        }
    }
}
