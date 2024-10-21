use std::fmt::Display;

use crate::{ast::Node, token::Token};

use super::ExpressionEnum;

#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub token: Token, // the '[' token
    pub elements: Vec<ExpressionEnum>,
}

impl Node for ArrayLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elements = self
            .elements
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "[{}]", elements)
    }
}
