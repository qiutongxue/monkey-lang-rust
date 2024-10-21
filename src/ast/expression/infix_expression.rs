use std::fmt::Display;

use crate::{ast::Node, token::Token};

use super::{Expression, ExpressionEnum};

/// 中缀表达式
///
/// <left expression> <infix operator> <right expression>
///
/// 5 + 5
#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub token: Token, // 中缀操作符，比如 +
    pub left: Box<ExpressionEnum>,
    pub operator: String,
    pub right: Box<ExpressionEnum>,
}

impl Expression for InfixExpression {}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({} {} {})",
            self.left.as_ref(),
            self.operator,
            self.right.as_ref()
        )
    }
}
