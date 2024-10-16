use crate::{ast::Node, token::Token};

use super::Expression;

/// 整型字面量表达式
/// 5 10 23 ...
#[derive(Debug)]
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

impl ToString for IntegerLiteral {
    fn to_string(&self) -> String {
        self.token.literal.clone()
    }
}
