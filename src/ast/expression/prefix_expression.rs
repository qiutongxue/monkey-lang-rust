use std::fmt::Display;

use crate::{ast::Node, token::Token};

use super::{Expression, ExpressionEnum};

/// 前缀表达式
///
/// <prefix operator><expression>
///
/// -5 !true
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PrefixExpression {
    pub token: Token, // 前缀操作符，比如 !
    pub operator: String,
    pub right: Box<ExpressionEnum>,
}

impl Expression for PrefixExpression {}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right.as_ref())
    }
}
