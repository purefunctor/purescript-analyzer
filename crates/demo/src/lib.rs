use std::sync::Arc;

use files::FileId;
use parsing::{error::ParseError, parse_module};
use smol_str::SmolStr;
use syntax::{SyntaxKind, SyntaxNode};

#[salsa::query_group(CompilerDatabase)]
trait Compiler {
    #[salsa::input]
    fn file_contents(&self, file_id: FileId) -> Arc<str>;

    #[salsa::invoke(file_syntax_query)]
    fn file_syntax(&self, file_id: FileId) -> (SyntaxNode, Arc<Vec<ParseError>>);
}

fn file_syntax_query(db: &dyn Compiler, file_id: FileId) -> (SyntaxNode, Arc<Vec<ParseError>>) {
    let contents = db.file_contents(file_id);
    let (node, errors) = parse_module(&contents);
    (node, Arc::new(errors))
}

#[salsa::database(CompilerDatabase)]
#[derive(Default)]
pub struct CompilerImpl {
    storage: salsa::Storage<CompilerImpl>,
}

impl salsa::Database for CompilerImpl {}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use files::{ChangedFile, Files};
    use rowan::ast::AstNode;
    use syntax::ast::{Binding, Declaration, Module};

    use crate::{Compiler, CompilerImpl};

    #[test]
    fn server_loop() {
        let mut db = CompilerImpl::default();
        let mut files = Files::default();

        files.set_file_contents(
            "./Main.purs".into(),
            Some(
                "
module Main where

a (-1) = 0

b | false = true
  | true = false
"
                .into(),
            ),
        );
        if files.has_changes() {
            for ChangedFile { file_id, .. } in files.take_changes() {
                let contents = std::str::from_utf8(files.file_contents(file_id)).unwrap();
                db.set_file_contents(file_id, Arc::from(contents));
            }
        }

        let file_id = files.file_id("./Main.purs".into()).unwrap();
        let (node, _) = db.file_syntax(file_id);

        println!("{:#?}", node);

        let module_node = Module::cast(node.children().next().unwrap()).unwrap();
        for declaration in module_node.body().unwrap().declarations().unwrap().children() {
            if let Declaration::ValueDeclaration(declaration) = declaration {
                match declaration.binding().unwrap() {
                    Binding::UnconditionalBinding(t) => {
                        dbg!(t.where_expression());
                    }
                    Binding::GuardedBinding(t) => {
                        for t in t.guarded_expressions().unwrap().children() {
                            dbg!(t.where_expression());
                        }
                    }
                }
            }
        }
    }
}

fn lower_expr(node: SyntaxNode) -> Expr {
    match node.kind() {
        SyntaxKind::LiteralExpression => {
            let literal = node.first_token().unwrap();
            match literal.kind() {
                SyntaxKind::LiteralInteger => {
                    Expr::Literal(Literal::Int(literal.text().parse().unwrap()))
                }
                SyntaxKind::LiteralNumber => Expr::Literal(Literal::Number(literal.text().into())),
                _ => todo!("I cannot lower, halp!"),
            }
        }
        _ => todo!("I cannot lower, halp!"),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Literal {
    Int(usize),
    Number(SmolStr),
    String(SmolStr),
    Char(char),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Expr {
    Literal(Literal),
}

/*

Lowering:

Before performning any semantic analysis, lowering has to be performed first
on the syntax. One of the steps in lowering is name resolution, where we make
it so that name refs resolve to what they're actually talking about. Note that
this step has to be resilient to changes as well, such that we do not repeat
computations.

We usually do name resolution by performing a traversal of the syntax tree.
Let's say that we want our top-level value declarations to be assigned with
IDs:

a = ...  -- ID 0
b = ...  -- ID 1
c = ...  -- ID 2

More generally, when a name is introduced, we assign it an ever-incrementing
ID.

Recursing into the declaration bodies, we may see references to other names:

a = b + b
b = ...
c = ...

During name resolution, we add more information saying that `b` _actually_
points to ID 1.

We may also see introductions that perform shadowing:

a = let b = 0 in b + b
b = ...
c = ...

In this case, `b` in `b + b` should resolve to the inner `b` instead. One
way to accomplish this is to resume ID assignment when recursing into definitions,
which gives us ID 4 for the inner `b`.

Assuming that we're doing name resolution as a single pass, this approach works
quite well. However, since we're incrementally editing the file, declarations
may come and go, and in order to reuse information, we now treat the set of
declarations not as in input to the resolver but as part of its state.

Take for example adding a declaration:

a = ...
b = ...
c = ...
+ d = ...

We've already assigned IDs for `a`, `b`, and `c`. Since we're adding a new
declaration, `d`, we have to assign an ID for it as well. We also have to
think about this in an inverted manner, for example, let's think of some LSP
service that relies on the declaration body like type-on-hover.

Type-on-hover would want to query the syntax node which the cursor is pointing
to. Internally, a query to locate the corresponding HIR item is performed. Then,
a query to determine what it actually points to is performed. Then, type checking
is performed from the bottom-up.

Alternative, type-on-hover would want to query the syntax node which the cursor
is pointing to, then internally, a query is performed to locate the corresponding
declaration for that node, then a query is performed to collect type information
for the entire subtree, before a search is performed. This way, type checking
is still performed top-down despite starting from the bottom.

One thing that this setup entails is that name resolution can effectively become
lazy to a certain extent. Specifically, since we can do top-down type checking
starting from the declaration level, we only need an ephemeral local environment
for each, in that while we still have the whole setup with binding introductions
and usages, they're only performed if the entire declaration has to be rechecked.

*/
