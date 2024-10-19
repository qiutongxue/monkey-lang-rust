mod block_statement;
mod boolean;
mod call_expression;
mod function_literal;
mod identifier;
mod if_expression;
mod infix_expression;
mod integer_literal;
mod prefix_expression;

use crate::impl_node_for_enum;
pub use block_statement::BlockStatement;
pub use boolean::Boolean;
pub use call_expression::CallExpression;
pub use function_literal::FunctionLiteral;
pub use identifier::Identifier;
pub use if_expression::IfExpression;
pub use infix_expression::InfixExpression;
pub use integer_literal::IntegerLiteral;
pub use prefix_expression::PrefixExpression;

use super::Node;

/// 表达式，比如 x + 5
/// 会产生值
pub trait Expression: Node + std::fmt::Debug {}

#[derive(Debug, Clone)]
pub enum ExpressionEnum {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Boolean(Boolean),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
    IfExpression(IfExpression),
    BlockStatement(BlockStatement),
}

impl_node_for_enum!(ExpressionEnum {
    Identifier,
    IntegerLiteral,
    PrefixExpression,
    InfixExpression,
    Boolean,
    FunctionLiteral,
    CallExpression,
    IfExpression,
    BlockStatement
});
