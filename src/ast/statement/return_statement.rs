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

impl ToString for ReturnStatement {
    fn to_string(&self) -> String {
        let mut out = String::new();

        out.push_str(&(self.token_literal() + " ")); // return

        if self.return_value.is_some() {
            out.push_str(&self.return_value.as_ref().unwrap().to_string()); // <expression>
        }

        out.push_str(";"); // ;
        out
    }
}
