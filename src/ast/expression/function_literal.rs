use std::fmt::Display;

use crate::{ast::Node, token::Token};

use super::{block_statement::BlockStatement, Expression, Identifier};

/// 函数表达式
///
/// fn (<parameters>) <block statement>
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionLiteral {
    pub token: Token, // token::FUNCTION
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Expression for FunctionLiteral {}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut params = Vec::with_capacity(self.parameters.len());
        for param in &self.parameters {
            params.push(param.to_string());
        }

        write!(
            f,
            "{}({}) {}",
            self.token.literal,
            params.join(", "),
            self.body
        )
    }
}
