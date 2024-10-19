use crate::{ast::Node, token::Token};

use super::{Expression, ExpressionEnum};

/// 前缀表达式
///
/// <prefix operator><expression>
///
/// -5 !true
#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Token, // 前缀操作符，比如 !
    pub operator: String,
    pub right: Option<Box<ExpressionEnum>>,
}

impl Expression for PrefixExpression {}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl ToString for PrefixExpression {
    fn to_string(&self) -> String {
        let mut out = String::new();

        // 为了区分优先级，使用括号包裹
        out.push('('); // (
        out.push_str(&self.operator); // <prefix operator>
        out.push_str(&self.right.as_ref().unwrap().to_string()); // <expression>
        out.push(')'); // )

        out
    }
}
