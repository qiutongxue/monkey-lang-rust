use std::fmt::Display;

use crate::{ast::Node, token::Token};

use super::Expression;

/// 布尔表达式
///
/// true false
#[derive(Debug, Clone)]
pub struct Boolean {
    pub token: Token, // token::TRUE or token::FALSE
    pub value: bool,
}

impl Expression for Boolean {}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}
