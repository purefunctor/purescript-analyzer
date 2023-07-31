use syntax::SyntaxKind;

use crate::parser::Parser;

/*

To build the parsing rules for expressions, we have to consider
how "tight" the bindings are for each type of expression. For
example, we can say that application expressions bind tighter
than operator expressions in that the parse tree that you'd
expect from something like `f x + g y` would be:

Start(OperatorExpression)
  Start(ApplicationExpression)
    Token
    Token
  Finish
  Start(ApplicationExpression)
    Token
    Token
  Finish
Finish

With this in mind, we can build our rules like so:

Expr1 = Expr2 ( Operator Expr2 )*

Expr2 = Expr3 Expr3*

Expr3 = Integer | Number | String | ...

Given our example once more, we know that `f x` matches with
`Expr2`, and that `+ g y` matches with `(Operator Expr2)*`,
which allows us to build the tree above.

On the other hand, if our input were only `f x`, then it
would produce a parse tree similar to the following:

Start(Sentinel) -- from Expr1, but canceled and ignored
  Start(ApplicationExpression) -- from Expr2, but finishes
    Token
    Token
  Finish

*/

pub fn expression_1(parser: &mut Parser) {
    let mut operator = parser.start();
    expression_2(parser);

    let mut entries = 0;
    loop {
        if parser.is_eof() {
            break;
        }

        if parser.at(SyntaxKind::Operator) {
            parser.consume();
            expression_2(parser);
            entries += 1;
        } else {
            break;
        }
    }

    if entries > 0 {
        operator.end(parser, SyntaxKind::OperatorChain);
    } else {
        operator.cancel(parser);
    }
}

pub fn expression_2(parser: &mut Parser) {
    let mut application = parser.start();
    expression_atom(parser);

    let mut arguments = 0;
    loop {
        if parser.is_eof() {
            break;
        }

        if expression_atom(parser) {
            arguments += 1;
        } else {
            break;
        }
    }

    if arguments > 0 {
        application.end(parser, SyntaxKind::ApplicationExpression);
    } else {
        application.cancel(parser);
    }
}

pub fn expression_atom(parser: &mut Parser) -> bool {
    match parser.current() {
        SyntaxKind::LiteralChar
        | SyntaxKind::LiteralString
        | SyntaxKind::LiteralRawString
        | SyntaxKind::LiteralInteger
        | SyntaxKind::LiteralNumber
        | SyntaxKind::LiteralTrue
        | SyntaxKind::LiteralFalse => {
            parser.consume();
            return true;
        }
        _ => (),
    }

    false
}

pub fn qualified_prefix(parser: &mut Parser) {
    let mut prefix = parser.start();
    loop {
        if parser.at(SyntaxKind::Upper) && parser.nth_at(1, SyntaxKind::Period) {
            let mut name = parser.start();
            parser.consume();
            name.end(parser, SyntaxKind::NameRef);
            parser.consume();
        } else {
            break;
        }
    }
    prefix.end(parser, SyntaxKind::QualifiedPrefix);
}

pub fn qualified_name(parser: &mut Parser) {
    let mut qualified = parser.start();

    qualified_prefix(parser);

    match parser.current() {
        SyntaxKind::Upper | SyntaxKind::Lower | SyntaxKind::AsKw => {
            let mut name = parser.start();
            if parser.at(SyntaxKind::AsKw) {
                parser.consume_as(SyntaxKind::Lower);
            } else {
                parser.consume();
            }
            name.end(parser, SyntaxKind::NameRef);
        }
        SyntaxKind::LeftParenthesis => {
            parser.consume();

            let mut name = parser.start();
            if parser.at(SyntaxKind::Operator) {
                parser.consume();
            } else {
                parser.error_recover("expected an operator");
            }
            name.end(parser, SyntaxKind::NameRef);

            parser.expect(SyntaxKind::RightParenthesis);
        }
        _ => {
            parser.error_recover("expected a name");
        }
    }

    qualified.end(parser, SyntaxKind::QualifiedName);
}
