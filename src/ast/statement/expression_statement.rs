use std::fmt::Display;

use crate::ast::expression::ExpressionEnum;
use crate::ast::Node;
use crate::token::Token;

use super::Statement;

/// 表达式语句
/// <expression>;
#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub(crate) token: Token, // 语句的第一个 token
    pub(crate) expression: Option<ExpressionEnum>,
}

impl Statement for ExpressionStatement {}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.expression.as_ref() {
            Some(e) => write!(f, "{}", e),
            _ => Ok(()),
        }
    }
}
