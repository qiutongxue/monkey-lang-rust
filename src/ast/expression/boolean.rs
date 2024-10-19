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

impl ToString for Boolean {
    fn to_string(&self) -> String {
        self.token.literal.clone()
    }
}
