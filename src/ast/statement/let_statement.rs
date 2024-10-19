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

impl ToString for LetStatement {
    fn to_string(&self) -> String {
        let mut out = String::new();

        out.push_str(&(self.token_literal() + " ")); // let
        out.push_str(&self.name.to_string()); // <identifier>
        out.push_str(" = "); // =

        if self.value.is_some() {
            out.push_str(&self.value.as_ref().unwrap().to_string()); // <expression>
        }

        out.push_str(";"); // ;
        out
    }
}
