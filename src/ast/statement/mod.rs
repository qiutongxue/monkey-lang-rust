mod expression_statement;
mod let_statement;
mod return_statement;

pub use expression_statement::ExpressionStatement;
pub use let_statement::LetStatement;
pub use return_statement::ReturnStatement;

use crate::impl_node_for_enum;

use super::Node;

/// 语句，比如 let x = 5;
/// 不产生值
pub trait Statement: Node + std::fmt::Debug {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StatementEnum {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl_node_for_enum!(StatementEnum {
    LetStatement,
    ReturnStatement,
    ExpressionStatement
});
