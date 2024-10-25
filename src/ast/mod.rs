mod expression;
mod statement;

use std::fmt::Display;

pub use expression::{
    ArrayLiteral, BlockStatement, Boolean, CallExpression, DictLiteral, Expression, ExpressionEnum,
    FunctionLiteral, Identifier, IfExpression, IndexExpression, InfixExpression, IntegerLiteral,
    PrefixExpression, StringLiteral,
};
pub use statement::{ExpressionStatement, LetStatement, ReturnStatement, Statement, StatementEnum};

use crate::{impl_as_any, AsAny};

impl_as_any!(
    Program,
    CallExpression,
    BlockStatement,
    Identifier,
    LetStatement,
    ReturnStatement,
    IntegerLiteral,
    PrefixExpression,
    InfixExpression,
    IfExpression,
    Boolean,
    FunctionLiteral,
    ExpressionStatement,
);

pub trait Node: Display {
    fn token_literal(&self) -> String;
}

/// 语法树的根节点
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<StatementEnum>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        self.statements
            .first()
            .map_or(String::default(), |stmt| stmt.token_literal())
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {

    use crate::token::{Token, TokenType};

    use super::*;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![StatementEnum::LetStatement(LetStatement {
                token: Token::from_str(TokenType::Let, "let"),
                name: Identifier {
                    token: Token::from_str(TokenType::Identifier, "myVar"),
                    value: "myVar".to_string(),
                },
                value: Some(ExpressionEnum::Identifier(Identifier {
                    token: Token::from_str(TokenType::Identifier, "anotherVar"),
                    value: "anotherVar".to_string(),
                })),
            })],
        };

        assert_eq!(
            program.to_string(),
            "let myVar = anotherVar;",
            "program.to_string() wrong. got={}",
            program.to_string()
        );
    }
}
