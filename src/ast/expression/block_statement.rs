use crate::{
    ast::{Node, StatementEnum},
    token::Token,
};

use super::Expression;

/// 块表达式
///
/// { <statement 1> <statement 2> ... }
#[derive(Debug)]
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

impl ToString for BlockStatement {
    fn to_string(&self) -> String {
        let mut out = String::new();

        out.push_str("{ ");
        for statement in &self.statements {
            out.push_str(&statement.to_string());
        }
        out.push_str(" }");

        out
    }
}
