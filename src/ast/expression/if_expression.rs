use crate::{ast::Node, token::Token};

use super::{block_statement::BlockStatement, Expression, ExpressionEnum};

/// if 表达式 (注意，if 是表达式，可以返回值的，如 if (x > y) {x} else {y})
///
/// if (<condition>) <consequence> else <alternative>
#[derive(Debug, Clone)]

pub struct IfExpression {
    pub token: Token, // token::IF
    pub condition: Option<Box<ExpressionEnum>>,
    pub consequence: Option<BlockStatement>,
    pub alternative: Option<BlockStatement>,
}

impl Expression for IfExpression {}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl ToString for IfExpression {
    fn to_string(&self) -> String {
        let mut out = String::new();

        out.push_str("if"); // if
        out.push(' '); //

        out.push('('); // (
        out.push_str(&self.condition.as_ref().unwrap().to_string()); // <condition>
        out.push(')'); // )

        out.push(' '); //
        out.push_str(&self.consequence.as_ref().unwrap().to_string()); // <consequence>

        if self.alternative.is_some() {
            out.push_str(" else "); // else
            out.push_str(&self.alternative.as_ref().unwrap().to_string()); // <alternative>
        }

        out
    }
}
