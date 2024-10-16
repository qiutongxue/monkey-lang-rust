use crate::{ast::Node, token::Token};

use super::{Expression, ExpressionEnum};

/// 函数调用表达式
///
/// <expression>(<comma separated expressions>)
/// add(2, 3)
/// fn(x, y) {x + y;}(1, 2)
#[derive(Debug)]
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

impl ToString for CallExpression {
    fn to_string(&self) -> String {
        let mut out = String::new();

        let mut args = Vec::new();
        for arg in &self.arguments {
            args.push(arg.to_string());
        }

        out.push_str(&self.function.to_string()); // <expression>
        out.push('('); // (

        out.push_str(&args.join(", ")); // <comma separated expressions>

        out.push(')'); // )

        out
    }
}
