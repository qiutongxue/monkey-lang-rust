use std::fmt::Display;

use crate::{
    ast::{Node, StatementEnum},
    token::Token,
};

use super::Expression;

/// 块表达式
///
/// { <statement 1> <statement 2> ... }
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockStatement {
    pub token: Token, // token::LBRACE
    pub statements: Vec<StatementEnum>,
}

impl Expression for BlockStatement {}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?;
        }
        Ok(())
    }
}
