use std::collections::HashMap;
use std::fmt::Display;
use std::sync::LazyLock;

use crate::token::TokenType;

#[derive(Debug)]
pub enum ParseError {
    ParseIntegerLiteralError(String),
    UnexpectedToken(TokenType, String),
    NoPrefixParseFn(TokenType),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ParseError:\n\t")?;
        match self {
            ParseError::ParseIntegerLiteralError(s) => {
                write!(f, "ParseIntegerLiteralError: can{}", s)
            }
            ParseError::UnexpectedToken(token_type, literal) => write!(
                f,
                "UnexpectedToken: expected next token to be {:?}, got {:?} instead",
                token_type, literal
            ),
            ParseError::NoPrefixParseFn(token_type) => write!(
                f,
                "NoPrefixParseFn: no prefix parse function for {:?} found",
                token_type
            ),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
    Index,       // array[index]
}

static PRECEDENCES: LazyLock<HashMap<TokenType, Precedence>> = LazyLock::new(|| {
    HashMap::from([
        (TokenType::EQ, Precedence::Equals),
        (TokenType::NotEQ, Precedence::Equals),
        (TokenType::LT, Precedence::LessGreater),
        (TokenType::GT, Precedence::LessGreater),
        (TokenType::Plus, Precedence::Sum),
        (TokenType::Minus, Precedence::Sum),
        (TokenType::Slash, Precedence::Product),
        (TokenType::Asterisk, Precedence::Product),
        (TokenType::LParen, Precedence::Call),
        (TokenType::LBracket, Precedence::Index),
    ])
});

use crate::ast::{
    ArrayLiteral, BlockStatement, Boolean, CallExpression, DictLiteral, ExpressionEnum,
    ExpressionStatement, FunctionLiteral, Identifier, IfExpression, IndexExpression,
    InfixExpression, IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement,
    StatementEnum, StringLiteral,
};

type PrefixParseFn = fn(&mut Parser) -> Result<ExpressionEnum, ParseError>;
type InfixParseFn = fn(&mut Parser, Box<ExpressionEnum>) -> Result<ExpressionEnum, ParseError>;

use crate::lexer::Lexer;
use crate::token::Token;

pub struct Parser {
    l: Lexer,

    cur_token: Token,
    peek_token: Token,

    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(mut l: Lexer) -> Self {
        let mut p = Parser {
            cur_token: l.next_token(),
            peek_token: l.next_token(),
            l,
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        p.register_prefix(TokenType::Identifier, Self::parse_indentifier);
        p.register_prefix(TokenType::Int, Self::parse_integer_literal);
        p.register_prefix(TokenType::String, Self::parse_string_literal);
        p.register_prefix(TokenType::True, Self::parse_boolean);
        p.register_prefix(TokenType::False, Self::parse_boolean);
        // 遇到 ! 和 - 时，当作整个前缀表达式 <prefix><expression>
        p.register_prefix(TokenType::Bang, Self::parse_prefix_expression);
        p.register_prefix(TokenType::Minus, Self::parse_prefix_expression);
        // 遇到 ( 时，启用分组
        p.register_prefix(TokenType::LParen, Self::parse_grouped_expression);
        // 遇到 <if> 时，调用 parseIfExpression
        p.register_prefix(TokenType::If, Self::parse_if_expression);

        // 遇到 <function> 时，调用 parseFunctionLiteral
        p.register_prefix(TokenType::Function, Self::parse_function_literal);
        // 遇到 [ 时，调用 parseArrayLiteral
        p.register_prefix(TokenType::LBracket, Self::parse_array_literal);
        p.register_prefix(TokenType::LBrace, Self::parse_dict_literal);

        // 遇到 ( 时，也有可能时函数调用
        // 例如：add(1, 2 * 3, 4 + 5)，此时左括号 ( 为中缀表达式
        // 程序是如何区分 prefix 的 ( 和 infix 的 ( 的呢？
        // 如果是函数调用，( 的左边一定是一个 prefix （如 identifier）
        // 如果是普通的分组，( 的左边一定是一个 infix（如 operator） 或空
        // 因为前面是 infix，所以此时左括号成为了 prefix
        p.register_infix(TokenType::LParen, Self::parse_call_expression);
        // 遇到 [ 时同理，可能是索引表达式
        // 例如：array[index]，此时 [ 为 infix
        p.register_infix(TokenType::LBracket, Self::parse_index_expression);

        // + - * / == != > < 时，当作中缀表达式 <expression><infix><expression>
        p.register_infix(TokenType::Plus, Self::parse_infix_expression);
        p.register_infix(TokenType::Minus, Self::parse_infix_expression);
        p.register_infix(TokenType::Slash, Self::parse_infix_expression);
        p.register_infix(TokenType::Asterisk, Self::parse_infix_expression);
        p.register_infix(TokenType::EQ, Self::parse_infix_expression);
        p.register_infix(TokenType::NotEQ, Self::parse_infix_expression);
        p.register_infix(TokenType::LT, Self::parse_infix_expression);
        p.register_infix(TokenType::GT, Self::parse_infix_expression);

        p
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.to_owned();
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut program = Program { statements: vec![] };

        while self.cur_token.token_type != TokenType::EOF {
            let stmt = self.parse_statement()?;
            program.statements.push(stmt);
            self.next_token();
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<StatementEnum, ParseError> {
        let stmt = match self.cur_token.token_type {
            TokenType::Let => StatementEnum::LetStatement(self.parse_let_statement()?),
            TokenType::Return => StatementEnum::ReturnStatement(self.parse_return_statement()?),
            _ => StatementEnum::ExpressionStatement(self.parse_expression_statement()?),
        };

        Ok(stmt)
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, ParseError> {
        let token = self.cur_token.clone();

        // 第一个一定是一个标识符
        self.expect_peek(TokenType::Identifier)?;

        let name = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        // 第二个一定是 =
        self.expect_peek(TokenType::Assign)?;

        // skip =
        self.next_token();

        let value = Some(self.parse_expression(Precedence::Lowest)?);

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(LetStatement { token, name, value })
    }

    /// 解析 return 语句
    ///
    /// return `expression`;
    fn parse_return_statement(&mut self) -> Result<ReturnStatement, ParseError> {
        let token = self.cur_token.clone();

        self.next_token();

        let return_value = Some(self.parse_expression(Precedence::Lowest)?);

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(ReturnStatement {
            token,
            return_value,
        })
    }

    /// 解析表达式语句
    ///
    /// <expression>;
    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, ParseError> {
        let stmt = ExpressionStatement {
            token: self.cur_token.clone(),
            expression: self.parse_expression(Precedence::Lowest)?,
        };

        // 表达式结尾可以没有分号（用于 REPL，如敲下 5 + 5 并回车）
        // 但是如果有分号，就要跳过分号
        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(stmt)
    }

    /// 解析表达式
    ///
    /// `expression`
    fn parse_expression(&mut self, precedence: Precedence) -> Result<ExpressionEnum, ParseError> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.token_type);

        match prefix {
            None => {
                // self.no_prefix_parse_fn_error(self.cur_token.token_type);
                Err(ParseError::NoPrefixParseFn(self.cur_token.token_type))
            }
            Some(prefix) => {
                let mut left_exp = prefix(self)?;
                while !self.peek_token_is(TokenType::Semicolon)
                    && precedence < self.peek_precedence()
                {
                    let infix = self.infix_parse_fns.get(&self.peek_token.token_type);

                    match infix {
                        None => return Ok(left_exp),
                        Some(&infix) => {
                            self.next_token();
                            left_exp = infix(self, Box::new(left_exp))?;
                        }
                    }
                }

                Ok(left_exp)
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone();

        self.next_token();

        let right = Box::new(self.parse_expression(Precedence::Prefix)?);

        Ok(ExpressionEnum::PrefixExpression(PrefixExpression {
            operator: token.literal.clone(),
            token,
            right,
        }))
    }

    fn parse_infix_expression(
        &mut self,
        left: Box<ExpressionEnum>,
    ) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone();

        let precedence = self.cur_precedence();

        self.next_token();

        let right = Box::new(self.parse_expression(precedence)?);

        Ok(ExpressionEnum::InfixExpression(InfixExpression {
            operator: token.literal.clone(),
            token,
            left,
            right,
        }))
    }

    fn parse_indentifier(&mut self) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone();

        Ok(ExpressionEnum::Identifier(Identifier {
            value: token.literal.clone(),
            token,
        }))
    }

    fn parse_integer_literal(&mut self) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone();

        match token.literal.parse::<i64>() {
            Ok(value) => Ok(ExpressionEnum::IntegerLiteral(IntegerLiteral {
                value,
                token,
            })),
            Err(_) => Err(ParseError::ParseIntegerLiteralError(token.literal.clone())),
        }
    }

    fn parse_string_literal(&mut self) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone();

        Ok(ExpressionEnum::StringLiteral(StringLiteral {
            value: token.literal.clone(),
            token,
        }))
    }

    fn parse_dict_literal(&mut self) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone();
        let mut pairs = HashMap::new();

        while !self.peek_token_is(TokenType::RBrace) {
            self.next_token();
            let key = self.parse_expression(Precedence::Lowest)?;
            self.expect_peek(TokenType::Colon)?;
            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;
            pairs.insert(key, value);
            if !self.peek_token_is(TokenType::RBrace) {
                self.expect_peek(TokenType::Comma)?;
            }
        }
        self.expect_peek(TokenType::RBrace)?;

        Ok(ExpressionEnum::DictLiteral(DictLiteral { token, pairs }))
    }

    fn parse_array_literal(&mut self) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone();

        let elements = self.parse_expression_list(TokenType::RBracket)?;

        Ok(ExpressionEnum::ArrayLiteral(ArrayLiteral {
            token,
            elements,
        }))
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Result<Vec<ExpressionEnum>, ParseError> {
        let mut list = vec![];

        if self.peek_token_is(end) {
            self.next_token();
            return Ok(list);
        }

        self.next_token();

        list.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token(); // 当前 -> COMMA
            self.next_token(); // COMMA -> 下一个表达式

            list.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(end)?;

        Ok(list)
    }

    fn parse_boolean(&mut self) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone();

        Ok(ExpressionEnum::Boolean(Boolean {
            value: self.cur_token_is(TokenType::True),
            token,
        }))
    }

    fn parse_grouped_expression(&mut self) -> Result<ExpressionEnum, ParseError> {
        // skip (
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest);

        self.expect_peek(TokenType::RParen)?;

        exp
    }

    /// 解析 if 表达式
    ///
    /// if (<condition>) <consequence> else <alternative>
    fn parse_if_expression(&mut self) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone();

        self.expect_peek(TokenType::LParen)?;

        self.next_token();

        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.expect_peek(TokenType::RParen)?;
        self.expect_peek(TokenType::LBrace)?;

        let consequence = self.parse_block_statement()?;
        let mut alternative = None;
        if self.peek_token_is(TokenType::Else) {
            self.next_token();

            self.expect_peek(TokenType::LBrace)?;

            alternative = self.parse_block_statement().ok();
        }

        Ok(ExpressionEnum::IfExpression(IfExpression {
            token,
            condition,
            consequence,
            alternative,
        }))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        let token = self.cur_token.clone();

        let mut statements = vec![];

        self.next_token();

        while !self.cur_token_is(TokenType::RBrace) && !self.cur_token_is(TokenType::EOF) {
            let stmt = self.parse_statement();

            if let Ok(stmt) = stmt {
                statements.push(stmt);
            }

            self.next_token();
        }

        Ok(BlockStatement { token, statements })
    }

    /// 解析函数字面量
    /// fn (<param1>, <param2>, ...) <block statement>
    fn parse_function_literal(&mut self) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone();

        self.expect_peek(TokenType::LParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(TokenType::LBrace)?;

        let body = self.parse_block_statement()?;

        Ok(ExpressionEnum::FunctionLiteral(FunctionLiteral {
            token,
            parameters,
            body,
        }))
    }

    /// 解析函数参数
    ///
    /// <param1>, <param2>, ...)
    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, ParseError> {
        let mut identifiers = vec![];

        // 空参数
        if self.peek_token_is(TokenType::RParen) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        let ident = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        identifiers.push(ident);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token(); // 当前 -> COMMA
            self.next_token(); // COMMA -> 下一个参数

            let ident = Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            };

            identifiers.push(ident);
        }

        self.expect_peek(TokenType::RParen)?;

        Ok(identifiers)
    }

    /// 解析函数调用
    ///
    /// <expression>(<param1>, <param2>, ...)
    fn parse_call_expression(
        &mut self,
        function: Box<ExpressionEnum>,
    ) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone();

        let arguments = self.parse_expression_list(TokenType::RParen)?;

        Ok(ExpressionEnum::CallExpression(CallExpression {
            token,
            function,
            arguments,
        }))
    }

    fn parse_index_expression(
        &mut self,
        left: Box<ExpressionEnum>,
    ) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone();

        self.next_token();

        let index = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.expect_peek(TokenType::RBracket)?;

        Ok(ExpressionEnum::IndexExpression(IndexExpression {
            token,
            left,
            index,
        }))
    }

    /// 检查下一个 token 是否是期望的类型（会消耗一个 token）
    fn expect_peek(&mut self, t: TokenType) -> Result<(), ParseError> {
        if self.peek_token_is(t) {
            self.next_token();
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken(
                t,
                self.peek_token.literal.clone(),
            ))
        }
    }

    /// 检查当前 token 是否是期望的类型
    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token.token_type == t
    }

    /// 检查下一个 token 是否是期望的类型
    fn cur_token_is(&self, t: TokenType) -> bool {
        self.cur_token.token_type == t
    }

    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
    }

    fn peek_precedence(&self) -> Precedence {
        PRECEDENCES
            .get(&self.peek_token.token_type)
            .map_or(Precedence::Lowest, |x| *x)
    }

    fn cur_precedence(&self) -> Precedence {
        PRECEDENCES
            .get(&self.cur_token.token_type)
            .map_or(Precedence::Lowest, |x| *x)
    }
}

#[cfg(test)]
mod test {

    use core::panic;

    use crate::{
        ast::{ExpressionEnum, Node, StatementEnum},
        test_utils::{parse_program, Value},
    };

    #[test]
    fn test_let_statement() {
        let tests = [
            ("let x = 5;", "x", Value::Integer(5)),
            ("let y = true;", "y", Value::Boolean(true)),
            ("let foobar = y;", "foobar", Value::String("y".to_string())),
        ];

        for tt in tests {
            let (input, expected_indent, expected_value) = tt;

            let program = parse_program(input);

            let stmt = &program.statements[0];

            assert!(_test_let_statement(stmt, expected_indent));

            if let StatementEnum::LetStatement(stmt) = stmt {
                let exp = stmt.value.as_ref().unwrap();
                assert!(_test_literal_expression(exp, expected_value));
            } else {
                panic!("stmt is not LetStatement, got={:?}", stmt);
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = [
            ("return 5;", Value::Integer(5)),
            ("return true;", Value::Boolean(true)),
            ("return foobar;", Value::String("foobar".to_string())),
        ];

        for tt in tests {
            let (input, expected_value) = tt;
            let program = parse_program(input);
            let stmt = &program.statements[0];

            assert_eq!(
                stmt.token_literal(),
                "return",
                "stmt.token_literal not 'return', got={}",
                stmt.token_literal()
            );

            if let StatementEnum::ReturnStatement(stmt) = stmt {
                let exp = stmt.return_value.as_ref().unwrap();
                assert!(_test_literal_expression(exp, expected_value));
            } else {
                panic!("stmt is not ReturnStatement, got={:?}", stmt);
            }
        }
    }

    #[test]
    fn test_identifier_expressions() {
        let input = "foobar;";

        let program = parse_program(input);

        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = &stmt.expression;
            if let ExpressionEnum::Identifier(ident) = exp {
                assert_eq!(
                    ident.value, "foobar",
                    "ident.value not 'foobar', got = {}",
                    ident.value
                );

                assert_eq!(
                    ident.token_literal(),
                    "foobar",
                    "ident.token_literal not 'foobar', got = {}",
                    ident.token_literal()
                );
            } else {
                panic!("exp is not Identifier, got={:?}", exp);
            }
        } else {
            panic!("stmt is not ExpressionStatement, got={:?}", stmt);
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let program = parse_program(input);

        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = &stmt.expression;
            if let ExpressionEnum::IntegerLiteral(lit) = exp {
                assert_eq!(lit.value, 5, "lit.value not '5', got = {}", lit.value);

                assert_eq!(
                    lit.token_literal(),
                    "5",
                    "lit.token_literal not '5', got = {}",
                    lit.token_literal()
                );
            } else {
                panic!("exp is not IntegerLiteral, got={:?}", exp);
            }
        } else {
            panic!("stmt is not ExpressionStatement, got={:?}", stmt);
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = [
            ("!5;", "!", Value::Integer(5)),
            ("-15;", "-", Value::Integer(15)),
            ("!true;", "!", Value::Boolean(true)),
            ("!false;", "!", Value::Boolean(false)),
        ];

        for tt in prefix_tests {
            let (input, operator, value) = tt;
            let program = parse_program(input);
            let stmt = &program.statements[0];

            if let StatementEnum::ExpressionStatement(stmt) = stmt {
                let exp = &stmt.expression;
                if let ExpressionEnum::PrefixExpression(exp) = exp {
                    assert_eq!(
                        exp.operator, operator,
                        "exp.operator is not '{}', got={}",
                        operator, exp.operator
                    );

                    assert!(_test_literal_expression(exp.right.as_ref(), value));
                } else {
                    panic!("exp is not PrefixExpression, got={:?}", exp);
                }
            } else {
                panic!("stmt is not ExpressionStatement, got={:?}", stmt);
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = [
            ("5 + 5;", Value::Integer(5), "+", Value::Integer(5)),
            ("5 - 5;", Value::Integer(5), "-", Value::Integer(5)),
            ("5 * 5;", Value::Integer(5), "*", Value::Integer(5)),
            ("5 / 5;", Value::Integer(5), "/", Value::Integer(5)),
            ("5 > 5;", Value::Integer(5), ">", Value::Integer(5)),
            ("5 < 5;", Value::Integer(5), "<", Value::Integer(5)),
            ("5 == 5;", Value::Integer(5), "==", Value::Integer(5)),
            ("5 != 5;", Value::Integer(5), "!=", Value::Integer(5)),
            (
                "true == true",
                Value::Boolean(true),
                "==",
                Value::Boolean(true),
            ),
            (
                "true != false",
                Value::Boolean(true),
                "!=",
                Value::Boolean(false),
            ),
            (
                "false == false",
                Value::Boolean(false),
                "==",
                Value::Boolean(false),
            ),
        ];

        for tt in infix_tests {
            let (input, left_value, operator, right_value) = tt;
            let program = parse_program(input);

            let stmt = &program.statements[0];

            if let StatementEnum::ExpressionStatement(exp_stmt) = stmt {
                assert!(_test_infix_expression(
                    &exp_stmt.expression,
                    left_value,
                    operator,
                    right_value,
                ));
            } else {
                panic!("stmt is not ExpressionStatement, got={:?}", stmt);
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for tt in tests {
            let (input, expected) = tt;
            let program = parse_program(input);

            let actual = program.to_string();

            assert_eq!(actual, expected, "expected={}, got={}", expected, actual);
        }
    }

    #[test]
    fn test_boolean_expression() {
        let input = "true;";
        let program = parse_program(input);

        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = &stmt.expression;
            if let ExpressionEnum::Boolean(boolean) = exp {
                assert_eq!(
                    boolean.value, true,
                    "boolean.value not 'true', got = {}",
                    boolean.value
                );

                assert_eq!(
                    boolean.token_literal(),
                    "true",
                    "boolean.token_literal not 'true', got = {}",
                    boolean.token_literal()
                );
            } else {
                panic!("exp is not Boolean, got={:?}", exp);
            }
        } else {
            panic!("stmt is not ExpressionStatement, got={:?}", stmt);
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let program = parse_program(input);

        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = &stmt.expression;
            if let ExpressionEnum::IfExpression(if_exp) = exp {
                assert!(_test_infix_expression(
                    if_exp.condition.as_ref(),
                    Value::String("x".to_string()),
                    "<",
                    Value::String("y".to_string()),
                ));

                assert_eq!(
                    if_exp.consequence.statements.len(),
                    1,
                    "consequence is not 1 statements, got={}",
                    if_exp.consequence.statements.len()
                );

                let consequence = &if_exp.consequence.statements[0];

                if let StatementEnum::ExpressionStatement(consequence) = consequence {
                    let consequence_exp = &consequence.expression;
                    assert!(_test_identifier(consequence_exp, "x"));

                    assert!(
                        if_exp.alternative.is_none(),
                        "if_exp.alternative.statements was not None, got={:?}",
                        if_exp.alternative
                    );
                } else {
                    panic!(
                        "consequence is not ExpressionStatement, got={:?}",
                        consequence
                    );
                }
            } else {
                panic!("exp is not IfExpression, got={:?}", exp);
            }
        } else {
            panic!("stmt is not ExpressionStatement, got={:?}", stmt);
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let program = parse_program(input);

        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = &stmt.expression;
            if let ExpressionEnum::IfExpression(if_exp) = exp {
                assert!(_test_infix_expression(
                    if_exp.condition.as_ref(),
                    Value::String("x".to_string()),
                    "<",
                    Value::String("y".to_string()),
                ));

                assert_eq!(
                    if_exp.consequence.statements.len(),
                    1,
                    "consequence is not 1 statements, got={}",
                    if_exp.consequence.statements.len()
                );

                let consequence = &if_exp.consequence.statements[0];

                if let StatementEnum::ExpressionStatement(consequence) = consequence {
                    let consequence_exp = &consequence.expression;
                    assert!(_test_identifier(consequence_exp, "x"));
                } else {
                    panic!(
                        "consequence is not ExpressionStatement, got={:?}",
                        consequence
                    );
                }

                assert_eq!(
                    if_exp.alternative.as_ref().unwrap().statements.len(),
                    1,
                    "alternative is not 1 statements, got={}",
                    if_exp.alternative.as_ref().unwrap().statements.len()
                );

                let alternative = &if_exp.alternative.as_ref().unwrap().statements[0];

                if let StatementEnum::ExpressionStatement(alternative) = alternative {
                    let alternative_exp = &alternative.expression;
                    assert!(_test_identifier(alternative_exp, "y"));
                } else {
                    panic!(
                        "alternative is not ExpressionStatement, got={:?}",
                        alternative
                    );
                }
            } else {
                panic!("exp is not IfExpression, got={:?}", exp);
            }
        } else {
            panic!("stmt is not ExpressionStatement, got={:?}", stmt);
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";

        let program = parse_program(input);

        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = &stmt.expression;
            if let ExpressionEnum::FunctionLiteral(function) = exp {
                assert_eq!(
                    function.parameters.len(),
                    2,
                    "function literal parameters wrong. want 2, got={}",
                    function.parameters.len()
                );

                assert_eq!(
                    function.parameters[0].value, "x",
                    "parameter is not 'x', got={}",
                    function.parameters[0].value
                );

                assert_eq!(
                    function.parameters[1].value, "y",
                    "parameter is not 'y', got={}",
                    function.parameters[1].value
                );

                assert_eq!(
                    function.body.statements.len(),
                    1,
                    "function.body.statements has not 1 statements. got={}",
                    function.body.statements.len()
                );

                let body_stmt = &function.body.statements[0];

                if let StatementEnum::ExpressionStatement(body_stmt) = body_stmt {
                    let body_exp = &body_stmt.expression;
                    assert!(_test_infix_expression(
                        body_exp,
                        Value::String("x".to_string()),
                        "+",
                        Value::String("y".to_string()),
                    ));
                } else {
                    panic!("function body stmt is not ExpressionStatement");
                }
            } else {
                panic!("exp is not FunctionLiteral, got={:?}", exp);
            }
        } else {
            panic!("stmt is not ExpressionStatement, got={:?}", stmt);
        }
    }

    #[test]
    fn test_parsing_function_parameters() {
        let tests = [
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        for tt in tests {
            let (input, expected_params) = tt;
            let program = parse_program(input);

            let stmt = &program.statements[0];

            if let StatementEnum::ExpressionStatement(stmt) = stmt {
                let exp = &stmt.expression;
                if let ExpressionEnum::FunctionLiteral(function) = exp {
                    assert_eq!(
                        function.parameters.len(),
                        expected_params.len(),
                        "function literal parameters wrong. want {}, got={}",
                        expected_params.len(),
                        function.parameters.len()
                    );

                    for (i, ident) in expected_params.iter().enumerate() {
                        assert_eq!(
                            function.parameters[i].value,
                            ident.to_string(),
                            "parameter is not {}, got={}",
                            ident,
                            function.parameters[i].value
                        );
                    }
                } else {
                    panic!("exp is not FunctionLiteral, got={:?}", exp);
                }
            } else {
                panic!("stmt is not ExpressionStatement, got={:?}", stmt);
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let program = parse_program(input);
        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = &stmt.expression;
            if let ExpressionEnum::CallExpression(call) = exp {
                assert!(_test_identifier(call.function.as_ref(), "add"));

                assert_eq!(
                    call.arguments.len(),
                    3,
                    "wrong length of arguments. want 3, got={}",
                    call.arguments.len()
                );

                assert!(_test_literal_expression(
                    &call.arguments[0],
                    Value::Integer(1)
                ));

                assert!(_test_infix_expression(
                    &call.arguments[1],
                    Value::Integer(2),
                    "*",
                    Value::Integer(3)
                ));

                assert!(_test_infix_expression(
                    &call.arguments[2],
                    Value::Integer(4),
                    "+",
                    Value::Integer(5)
                ));
            } else {
                panic!("exp is not CallExpression, got={:?}", exp);
            }
        } else {
            panic!("stmt is not ExpressionStatement, got={:?}", stmt);
        }
    }

    fn _test_infix_expression(
        exp: &ExpressionEnum,
        left: Value,
        operator: &str,
        right: Value,
    ) -> bool {
        if let ExpressionEnum::InfixExpression(op_exp) = exp {
            assert!(_test_literal_expression(op_exp.left.as_ref(), left));

            assert_eq!(
                op_exp.operator, operator,
                "exp.operator is not '{}', got={}",
                operator, op_exp.operator
            );

            assert!(_test_literal_expression(op_exp.right.as_ref(), right));

            true
        } else {
            panic!("exp is not InfixExpression, got={:?}", exp);
        }
    }

    fn _test_let_statement(s: &StatementEnum, expected_name: &str) -> bool {
        if let StatementEnum::LetStatement(s) = s {
            assert_eq!(
                s.token_literal(),
                "let",
                "s.token_literal not 'let'. got={}",
                s.token_literal()
            );

            assert_eq!(
                s.name.value, expected_name,
                "let_statement.name is not {}, got = {}",
                expected_name, s.name.value
            );

            assert_eq!(
                s.name.token_literal(),
                expected_name,
                "let_statement.name is not {}, got = {}",
                expected_name,
                s.name.token_literal()
            );

            true
        } else {
            panic!()
        }
    }

    fn _test_literal_expression(exp: &ExpressionEnum, expected: Value) -> bool {
        match expected {
            Value::Boolean(v) => _test_bool_literal(exp, v),
            Value::Integer(v) => _test_integer_literal(exp, v),
            Value::String(v) => _test_identifier(exp, &v),
            _ => unreachable!(),
        }
    }

    fn _test_identifier(exp: &ExpressionEnum, expected: &str) -> bool {
        if let ExpressionEnum::Identifier(ident) = exp {
            assert_eq!(
                ident.value, expected,
                "ident.value not {}. got={}",
                expected, ident.value
            );

            assert_eq!(
                ident.token_literal(),
                expected,
                "ident.token_literal not {}. got={}",
                expected,
                ident.token_literal()
            );

            true
        } else {
            return false;
        }
    }

    fn _test_bool_literal(exp: &ExpressionEnum, expected: bool) -> bool {
        if let ExpressionEnum::Boolean(bl) = exp {
            assert_eq!(
                bl.value, expected,
                "bl.value not {}. got={}",
                expected, bl.value
            );

            assert_eq!(
                bl.token_literal(),
                format!("{}", expected),
                "bl.token_literal not {}. got={}",
                expected,
                bl.token_literal()
            );

            true
        } else {
            return false;
        }
    }

    fn _test_integer_literal(exp: &ExpressionEnum, expected: i64) -> bool {
        if let ExpressionEnum::IntegerLiteral(il) = exp {
            assert_eq!(
                il.value, expected,
                "il.value not {}. got={}",
                expected, il.value
            );

            assert_eq!(
                il.token_literal(),
                format!("{}", expected),
                "il.token_literal not {}. got={}",
                expected,
                il.token_literal()
            );
        } else {
            return false;
        }

        true
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#""hello world";"#;
        let program = parse_program(input);
        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = &stmt.expression;
            if let ExpressionEnum::StringLiteral(string) = exp {
                assert_eq!(
                    string.value, "hello world",
                    "string.value not 'hello world'. got={}",
                    string.value
                );
            } else {
                panic!("exp is not StringLiteral, got={:?}", exp);
            }
        } else {
            panic!("stmt is not ExpressionStatement, got={:?}", stmt);
        }
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        let program = parse_program(input);

        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = &stmt.expression;
            if let ExpressionEnum::ArrayLiteral(array) = exp {
                assert_eq!(
                    array.elements.len(),
                    3,
                    "array.elements does not contain 3 elements. got={}",
                    array.elements.len()
                );

                assert!(_test_integer_literal(&array.elements[0], 1));

                assert!(_test_infix_expression(
                    &array.elements[1],
                    Value::Integer(2),
                    "*",
                    Value::Integer(2)
                ));

                assert!(_test_infix_expression(
                    &array.elements[2],
                    Value::Integer(3),
                    "+",
                    Value::Integer(3)
                ));
            } else {
                panic!("exp is not ArrayLiteral, got={:?}", exp);
            }
        } else {
            panic!("stmt is not ExpressionStatement, got={:?}", stmt);
        }
    }

    #[test]
    fn test_parsing_index_expressions() {
        let input = "myArray[1 + 1]";

        let program = parse_program(input);

        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = &stmt.expression;
            if let ExpressionEnum::IndexExpression(index) = exp {
                assert!(_test_identifier(&index.left, "myArray"));

                assert!(_test_infix_expression(
                    &index.index,
                    Value::Integer(1),
                    "+",
                    Value::Integer(1)
                ));
            } else {
                panic!("exp is not IndexExpression, got={:?}", exp);
            }
        } else {
            panic!("stmt is not ExpressionStatement, got={:?}", stmt);
        }
    }

    #[test]
    fn test_parsing_dict_literals_string_keys() {
        let input = r#"{"one": 1, "two": 2, "three": 3}"#;

        let program = parse_program(input);

        let stmt = &program.statements[0];

        let expected = [("one", 1), ("two", 2), ("three", 3)];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = &stmt.expression;
            if let ExpressionEnum::DictLiteral(dict) = exp {
                assert_eq!(
                    dict.pairs.len(),
                    3,
                    "dict.pairs does not contain 3 elements. got={}",
                    dict.pairs.len()
                );

                for (key, value) in dict.pairs.iter() {
                    if let ExpressionEnum::StringLiteral(key) = key {
                        let expected_key = expected.iter().find(|(k, _)| k == &key.to_string());
                        if let Some((_, expected_value)) = expected_key {
                            assert!(_test_integer_literal(value, *expected_value));
                        } else {
                            panic!("key not found in expected pairs. key={}", key.to_string());
                        }
                    } else {
                        panic!("key is not StringLiteral, got={:?}", key);
                    }
                }
            } else {
                panic!("exp is not DictLiteral, got={:?}", exp);
            }
        } else {
            panic!("stmt is not ExpressionStatement, got={:?}", stmt);
        }
    }

    #[test]
    fn test_parsing_empty_dict_literal() {
        let input = "{}";

        let program = parse_program(input);

        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = &stmt.expression;
            if let ExpressionEnum::DictLiteral(dict) = exp {
                assert_eq!(
                    dict.pairs.len(),
                    0,
                    "dict.pairs does not contain 0 elements. got={}",
                    dict.pairs.len()
                );
            } else {
                panic!("exp is not DictLiteral, got={:?}", exp);
            }
        } else {
            panic!("stmt is not ExpressionStatement, got={:?}", stmt);
        }
    }

    #[test]
    fn test_parsing_dict_literals_with_expressions() {
        let input = r#"{"one": 0 + 1, "two": 10 - 9, "three": 15 / 5}"#;

        let program = parse_program(input);

        let stmt = &program.statements[0];

        let expected: [(&str, fn(&ExpressionEnum) -> bool); 3] = [
            ("one", |e| {
                _test_infix_expression(e, Value::Integer(0), "+", Value::Integer(1))
            }),
            ("two", |e| {
                _test_infix_expression(e, Value::Integer(10), "-", Value::Integer(9))
            }),
            ("three", |e| {
                _test_infix_expression(e, Value::Integer(15), "/", Value::Integer(5))
            }),
        ];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = &stmt.expression;
            if let ExpressionEnum::DictLiteral(dict) = exp {
                assert_eq!(
                    dict.pairs.len(),
                    3,
                    "dict.pairs does not contain 3 elements. got={}",
                    dict.pairs.len()
                );

                for (key, value) in dict.pairs.iter() {
                    if let ExpressionEnum::StringLiteral(key) = key {
                        let expected_key = expected.iter().find(|(k, _)| k == &key.to_string());
                        if let Some((_, expected_value)) = expected_key {
                            assert!(expected_value(value));
                        } else {
                            panic!("key not found in expected pairs. key={}", key.to_string());
                        }
                    } else {
                        panic!("key is not StringLiteral, got={:?}", key);
                    }
                }
            } else {
                panic!("exp is not DictLiteral, got={:?}", exp);
            }
        } else {
            panic!("stmt is not ExpressionStatement, got={:?}", stmt);
        }
    }
}
