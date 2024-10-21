mod array_literal;
mod block_statement;
mod boolean;
mod call_expression;
mod function_literal;
mod identifier;
mod if_expression;
mod index_expression;
mod infix_expression;
mod integer_literal;
mod prefix_expression;
mod string_literal;

use crate::impl_node_for_enum;
pub use array_literal::ArrayLiteral;
pub use block_statement::BlockStatement;
pub use boolean::Boolean;
pub use call_expression::CallExpression;
pub use function_literal::FunctionLiteral;
pub use identifier::Identifier;
pub use if_expression::IfExpression;
pub use index_expression::IndexExpression;
pub use infix_expression::InfixExpression;
pub use integer_literal::IntegerLiteral;
pub use prefix_expression::PrefixExpression;
pub use string_literal::StringLiteral;

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
    StringLiteral(StringLiteral),
    ArrayLiteral(ArrayLiteral),
    IndexExpression(IndexExpression),
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
    BlockStatement,
    StringLiteral,
    ArrayLiteral,
    IndexExpression
});
