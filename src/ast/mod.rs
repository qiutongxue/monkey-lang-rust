mod expression;
mod statement;

pub use expression::{
    BlockStatement, Boolean, CallExpression, Expression, ExpressionEnum, FunctionLiteral,
    Identifier, IfExpression, InfixExpression, IntegerLiteral, PrefixExpression,
};
pub use statement::{ExpressionStatement, LetStatement, ReturnStatement, Statement, StatementEnum};

use crate::{impl_as_any, impl_node_for_enum, AsAny};

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

#[derive(Debug)]

pub enum NodeEnum {
    Program(Program),
    StatementEnum(StatementEnum),
    ExpressionEnum(ExpressionEnum),
}

impl_node_for_enum!(NodeEnum {
    Program,
    StatementEnum,
    ExpressionEnum
});

pub trait Node: ToString {
    fn token_literal(&self) -> String;
}

/// 语法树的根节点
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<StatementEnum>,
}

impl ToString for Program {
    fn to_string(&self) -> String {
        let mut out = String::new();
        for statement in &self.statements {
            out.push_str(&statement.to_string());
        }
        out
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            return self.statements[0].token_literal();
        } else {
            return String::default();
        }
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
