use std::fmt::Display;

use crate::{
    ast::{expression::ExpressionEnum, Node},
    token::Token,
};

use super::Statement;

/// return 语句
///
/// return <expression>;
#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub(crate) token: Token, // token::RETURN
    pub(crate) return_value: Option<ExpressionEnum>,
}

impl Statement for ReturnStatement {}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{};",
            self.token.literal,
            self.return_value
                .as_ref()
                .map(|v| format!(" {}", v))
                .unwrap_or_default()
        )
    }
}
