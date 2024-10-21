use std::fmt::Display;

use crate::{ast::Node, token::Token};

use super::{block_statement::BlockStatement, Expression, ExpressionEnum};

/// if 表达式 (注意，if 是表达式，可以返回值的，如 if (x > y) {x} else {y})
///
/// if (<condition>) <consequence> else <alternative>
#[derive(Debug, Clone)]

pub struct IfExpression {
    pub token: Token, // token::IF
    pub condition: Box<ExpressionEnum>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Expression for IfExpression {}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if ({}) {}", self.condition, self.consequence)?;
        if self.alternative.is_some() {
            write!(f, " else {}", self.alternative.as_ref().unwrap())?;
        }
        Ok(())
    }
}
