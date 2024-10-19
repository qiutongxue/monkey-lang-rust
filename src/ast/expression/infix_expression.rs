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
    pub left: Option<Box<ExpressionEnum>>,
    pub operator: String,
    pub right: Option<Box<ExpressionEnum>>,
}

impl Expression for InfixExpression {}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl ToString for InfixExpression {
    fn to_string(&self) -> String {
        let mut out = String::new();

        // 为了区分优先级，使用括号包裹
        out.push('('); // (
        out.push_str(&self.left.as_ref().unwrap().to_string()); // <left expression>

        out.push(' '); //
        out.push_str(&self.operator); // <infix operator>
        out.push(' '); //

        out.push_str(&self.right.as_ref().unwrap().to_string()); // <right expression>
        out.push(')'); // )

        out
    }
}
