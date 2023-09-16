use crate::token::Token;

pub(crate) trait Node: ToString {
    fn token_literal(&self) -> String;
}

/// 语句，比如 let x = 5;
/// 不产生值
pub(crate) trait Statement: Node + std::fmt::Debug {
    fn as_any(&self) -> &dyn std::any::Any;
}

/// 表达式，比如 x + 5
/// 会产生值
pub(crate) trait Expression: Node + std::fmt::Debug {
    fn as_any(&self) -> &dyn std::any::Any;
}

/// 语法树的根节点
#[derive(Debug)]
pub struct Program {
    pub(crate) statements: Vec<Box<dyn Statement>>,
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

#[derive(Debug)]

/// <identifier> 标识符
pub(crate) struct Identifier {
    pub(crate) token: Token,
    pub(crate) value: String,
}

impl Expression for Identifier {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl ToString for Identifier {
    fn to_string(&self) -> String {
        self.value.clone()
    }
}
#[derive(Debug)]

/// let 语句
///
/// let <identifier> = <expression>;
pub struct LetStatement {
    pub(crate) token: Token, // token::LET
    pub(crate) name: Identifier,
    pub(crate) value: Option<Box<dyn Expression>>,
}

impl Statement for LetStatement {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

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
#[derive(Debug)]

/// return 语句
///
/// return <expression>;
pub struct ReturnStatement {
    pub(crate) token: Token, // token::RETURN
    pub(crate) return_value: Option<Box<dyn Expression>>,
}

impl Statement for ReturnStatement {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

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
#[derive(Debug)]

/// 表达式语句
/// <expression>;
pub struct ExpressionStatement {
    pub(crate) token: Token, // 语句的第一个 token
    pub(crate) expression: Option<Box<dyn Expression>>,
}

impl Statement for ExpressionStatement {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl ToString for ExpressionStatement {
    fn to_string(&self) -> String {
        match self.expression.as_ref() {
            Some(e) => e.to_string(),
            None => String::default(),
        }
    }
}
#[derive(Debug)]

/// 整型字面量表达式
/// 5 10 23 ...
pub struct IntegerLiteral {
    pub(crate) token: Token, // token::INT
    pub(crate) value: i64,
}

impl Expression for IntegerLiteral {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl ToString for IntegerLiteral {
    fn to_string(&self) -> String {
        self.token.literal.clone()
    }
}
#[derive(Debug)]

/// 前缀表达式
///
/// <prefix operator><expression>
///
/// -5 !true
pub struct PrefixExpression {
    pub(crate) token: Token, // 前缀操作符，比如 !
    pub(crate) operator: String,
    pub(crate) right: Option<Box<dyn Expression>>,
}

impl Expression for PrefixExpression {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl ToString for PrefixExpression {
    fn to_string(&self) -> String {
        let mut out = String::new();

        // 为了区分优先级，使用括号包裹
        out.push('('); // (
        out.push_str(&self.operator); // <prefix operator>
        out.push_str(&self.right.as_ref().unwrap().to_string()); // <expression>
        out.push(')'); // )

        out
    }
}
#[derive(Debug)]

/// 中缀表达式
///
/// <left expression> <infix operator> <right expression>
///
/// 5 + 5
pub struct InfixExpression {
    pub(crate) token: Token, // 中缀操作符，比如 +
    pub(crate) left: Option<Box<dyn Expression>>,
    pub(crate) operator: String,
    pub(crate) right: Option<Box<dyn Expression>>,
}

impl Expression for InfixExpression {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl ToString for InfixExpression {
    fn to_string(&self) -> String {
        let mut out = String::new();

        // 为了区分优先级，使用括号包裹
        out.push('('); // (
        out.push_str(&self.left.as_ref().unwrap().to_string()); // <left expression>

        out.push(' '); //
        out.push_str(&self.operator); // <infix operator>
        out.push(' '); //

        out.push_str(&self.right.as_ref().unwrap().to_string()); // <right expression>
        out.push(')'); // )

        out
    }
}
#[derive(Debug)]

/// 布尔表达式
///
/// true false
pub struct Boolean {
    pub(crate) token: Token, // token::TRUE or token::FALSE
    pub(crate) value: bool,
}

impl Expression for Boolean {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl ToString for Boolean {
    fn to_string(&self) -> String {
        self.token.literal.clone()
    }
}
#[derive(Debug)]

/// 块表达式
///
/// { <statement 1> <statement 2> ... }
pub struct BlockStatement {
    pub(crate) token: Token, // token::LBRACE
    pub(crate) statements: Vec<Box<dyn Statement>>,
}

impl Expression for BlockStatement {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl ToString for BlockStatement {
    fn to_string(&self) -> String {
        let mut out = String::new();

        out.push_str("{ ");
        for statement in &self.statements {
            out.push_str(&statement.to_string());
        }
        out.push_str(" }");

        out
    }
}
#[derive(Debug)]

/// if 表达式 (注意，if 是表达式，可以返回值的，如 if (x > y) {x} else {y})
///
/// if (<condition>) <consequence> else <alternative>
pub struct IfExpression {
    pub(crate) token: Token, // token::IF
    pub(crate) condition: Option<Box<dyn Expression>>,
    pub(crate) consequence: Option<BlockStatement>,
    pub(crate) alternative: Option<BlockStatement>,
}

impl Expression for IfExpression {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl ToString for IfExpression {
    fn to_string(&self) -> String {
        let mut out = String::new();

        out.push_str("if"); // if
        out.push(' '); //

        out.push('('); // (
        out.push_str(&self.condition.as_ref().unwrap().to_string()); // <condition>
        out.push(')'); // )

        out.push(' '); //
        out.push_str(&self.consequence.as_ref().unwrap().to_string()); // <consequence>

        if self.alternative.is_some() {
            out.push_str(" else "); // else
            out.push_str(&self.alternative.as_ref().unwrap().to_string()); // <alternative>
        }

        out
    }
}
#[derive(Debug)]

/// 函数表达式
///
/// fn (<parameters>) <block statement>
pub struct FunctionLiteral {
    pub(crate) token: Token, // token::FUNCTION
    pub(crate) parameters: Vec<Identifier>,
    pub(crate) body: BlockStatement,
}

impl Expression for FunctionLiteral {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

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
#[derive(Debug)]

/// 函数调用表达式
///
/// <expression>(<comma separated expressions>)
/// add(2, 3)
/// fn(x, y) {x + y;}(1, 2)
pub struct CallExpression {
    pub(crate) token: Token, // token::LPAREN
    pub(crate) function: Box<dyn Expression>,
    pub(crate) arguments: Vec<Box<dyn Expression>>,
}

impl Expression for CallExpression {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl ToString for CallExpression {
    fn to_string(&self) -> String {
        let mut out = String::new();

        let mut args = Vec::new();
        for arg in &self.arguments {
            args.push(arg.to_string());
        }

        out.push_str(&self.function.to_string()); // <expression>
        out.push('('); // (

        out.push_str(&args.join(", ")); // <comma separated expressions>

        out.push(')'); // )

        out
    }
}

#[cfg(test)]
mod test {

    use crate::token::TokenType;

    use super::*;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![Box::new(LetStatement {
                token: Token::from_str(TokenType::LET, "let"),
                name: Identifier {
                    token: Token::from_str(TokenType::IDENT, "myVar"),
                    value: "myVar".to_string(),
                },
                value: Some(Box::new(Identifier {
                    token: Token::from_str(TokenType::IDENT, "anotherVar"),
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
