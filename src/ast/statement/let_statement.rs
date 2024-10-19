use std::fmt::Display;

use crate::{
    ast::{
        expression::{ExpressionEnum, Identifier},
        Node,
    },
    token::Token,
};

use super::Statement;

/// let 语句
///
/// let <identifier> = <expression>;
#[derive(Debug, Clone)]
pub struct LetStatement {
    pub(crate) token: Token, // token::LET
    pub(crate) name: Identifier,
    pub(crate) value: Option<ExpressionEnum>,
}

impl Statement for LetStatement {}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} = {};",
            self.token.literal,
            self.name,
            &self
                .value
                .as_ref()
                .map(|v| v.to_string())
                .unwrap_or_default()
        )
    }
}
