use std::fmt::Display;

use crate::{ast::Node, token::Token};

use super::Expression;

/// 整型字面量表达式
/// 5 10 23 ...
#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token, // token::INT
    pub value: i64,
}

impl Expression for IntegerLiteral {}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}
