use std::fmt::Display;

use crate::{ast::Node, token::Token};

use super::{Expression, ExpressionEnum};

/// 函数调用表达式
///
/// <expression>(<comma separated expressions>)
/// add(2, 3)
/// fn(x, y) {x + y;}(1, 2)
#[derive(Debug, Clone)]
pub struct CallExpression {
    pub token: Token, // token::LPAREN
    pub function: Box<ExpressionEnum>,
    pub arguments: Vec<ExpressionEnum>,
}

impl Expression for CallExpression {}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut args = Vec::with_capacity(self.arguments.len());
        for arg in &self.arguments {
            args.push(arg.to_string());
        }
        write!(f, "{}({})", self.function, args.join(", "))
    }
}
