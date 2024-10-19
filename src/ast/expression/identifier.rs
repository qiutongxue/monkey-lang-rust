use crate::{ast::Node, token::Token};

use super::Expression;

/// <identifier> 标识符
#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Expression for Identifier {}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl ToString for Identifier {
    fn to_string(&self) -> String {
        self.value.clone()
    }
}
