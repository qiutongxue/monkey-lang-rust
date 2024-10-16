use crate::{ast::Node, token::Token};

use super::{block_statement::BlockStatement, Expression, Identifier};

/// 函数表达式
///
/// fn (<parameters>) <block statement>
#[derive(Debug)]
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

impl ToString for FunctionLiteral {
    fn to_string(&self) -> String {
        let mut out = String::new();

        let mut params = Vec::new();
        for param in &self.parameters {
            params.push(param.to_string());
        }

        out.push_str(&self.token_literal()); // fn
        out.push('('); // (

        out.push_str(&params.join(", ")); // <parameters>

        out.push(')'); // )
        out.push(' '); //

        out.push_str(&self.body.to_string()); // <block statement>

        out
    }
}
