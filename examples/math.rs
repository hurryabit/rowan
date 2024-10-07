//! Example that takes the input
//! 1 + 2 * 3 + 4
//! and builds the tree
//! - Marker(Root)
//!   - Marker(Operation)
//!     - Marker(Operation)
//!       - "1" Token(Number)
//!       - "+" Token(Add)
//!       - Marker(Operation)
//!         - "2" Token(Number)
//!         - "*" Token(Mul)
//!         - "3" Token(Number)
//!     - "+" Token(Add)
//!     - "4" Token(Number)

use rowan::{GreenNodeBuilder, NodeOrToken};
use std::iter::Peekable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
enum TokenKind {
    WHITESPACE = 0,

    ADD,
    SUB,
    MUL,
    DIV,

    NUMBER,
    UNKNOWN,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
enum NodeKind {
    ERROR,
    OPERATION,
    ROOT,
}
use NodeKind::*;
use TokenKind::*;

impl From<NodeKind> for rowan::NodeKind {
    fn from(kind: NodeKind) -> Self {
        Self(kind as u16)
    }
}

impl From<TokenKind> for rowan::TokenKind {
    fn from(kind: TokenKind) -> Self {
        Self(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Lang {}
impl rowan::Language for Lang {
    type NodeKind = NodeKind;
    type TokenKind = TokenKind;
    fn node_kind_from_raw(raw: rowan::NodeKind) -> Self::NodeKind {
        assert!(raw.0 <= NodeKind::ROOT as u16);
        unsafe { std::mem::transmute::<u16, NodeKind>(raw.0) }
    }
    fn node_kind_to_raw(kind: Self::NodeKind) -> rowan::NodeKind {
        kind.into()
    }
    fn token_kind_from_raw(raw: rowan::TokenKind) -> Self::TokenKind {
        assert!(raw.0 <= TokenKind::UNKNOWN as u16);
        unsafe { std::mem::transmute::<u16, TokenKind>(raw.0) }
    }
    fn token_kind_to_raw(kind: Self::TokenKind) -> rowan::TokenKind {
        kind.into()
    }
}

type SyntaxNode = rowan::SyntaxNode<Lang>;
#[allow(unused)]
type SyntaxToken = rowan::SyntaxToken<Lang>;
#[allow(unused)]
type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

struct Parser<I: Iterator<Item = (TokenKind, String)>> {
    builder: GreenNodeBuilder<'static>,
    iter: Peekable<I>,
}
impl<I: Iterator<Item = (TokenKind, String)>> Parser<I> {
    fn peek(&mut self) -> Option<TokenKind> {
        while self.iter.peek().map(|&(t, _)| t == WHITESPACE).unwrap_or(false) {
            self.bump();
        }
        self.iter.peek().map(|&(t, _)| t)
    }
    fn bump(&mut self) {
        if let Some((token, string)) = self.iter.next() {
            self.builder.token(token.into(), string.as_str());
        }
    }
    fn parse_val(&mut self) {
        match self.peek() {
            Some(NUMBER) => self.bump(),
            _ => {
                self.builder.start_node(ERROR.into());
                self.bump();
                self.builder.finish_node();
            }
        }
    }
    fn handle_operation(&mut self, tokens: &[TokenKind], next: fn(&mut Self)) {
        let checkpoint = self.builder.checkpoint();
        next(self);
        while self.peek().map(|t| tokens.contains(&t)).unwrap_or(false) {
            self.builder.start_node_at(checkpoint, OPERATION.into());
            self.bump();
            next(self);
            self.builder.finish_node();
        }
    }
    fn parse_mul(&mut self) {
        self.handle_operation(&[MUL, DIV], Self::parse_val)
    }
    fn parse_add(&mut self) {
        self.handle_operation(&[ADD, SUB], Self::parse_mul)
    }
    fn parse(mut self) -> SyntaxNode {
        self.builder.start_node(ROOT.into());
        self.parse_add();
        self.builder.finish_node();

        SyntaxNode::new_root(self.builder.finish())
    }
}

fn print(indent: usize, element: SyntaxElement) {
    print!("{:indent$}", "", indent = indent);
    match element {
        NodeOrToken::Node(node) => {
            println!("- {:?}", node.kind());
            for child in node.children_with_tokens() {
                print(indent + 2, child);
            }
        }

        NodeOrToken::Token(token) => println!("- {:?} {:?}", token.text(), token.kind()),
    }
}

fn main() {
    let ast = Parser {
        builder: GreenNodeBuilder::new(),
        iter: vec![
            // 1 + 2 * 3 + 4
            (NUMBER, "1".into()),
            (WHITESPACE, " ".into()),
            (ADD, "+".into()),
            (WHITESPACE, " ".into()),
            (NUMBER, "2".into()),
            (WHITESPACE, " ".into()),
            (MUL, "*".into()),
            (WHITESPACE, " ".into()),
            (NUMBER, "3".into()),
            (WHITESPACE, " ".into()),
            (ADD, "+".into()),
            (WHITESPACE, " ".into()),
            (NUMBER, "4".into()),
        ]
        .into_iter()
        .peekable(),
    }
    .parse();
    print(0, ast.into());
}
