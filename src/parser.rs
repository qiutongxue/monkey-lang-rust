use std::collections::HashMap;
use std::sync::LazyLock;

use crate::token::TokenType;

#[derive(Debug)]
pub enum ParseError {
    ParseProgramError,
    ParseLetStatementError,
    ParseExpressionError,
    ParseGroupedExpressionError,
    ParseIntegerLiteralError,
    ParseIfExpressionError,
    ParseFunctionLiteralError,
    ParseCallArgumentsError,
    TokenIsNone,
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
}

static PRECEDENCES: LazyLock<HashMap<TokenType, Precedence>> = LazyLock::new(|| {
    let mut map = HashMap::new();
    map.insert(TokenType::EQ, Precedence::Equals);
    map.insert(TokenType::NotEQ, Precedence::Equals);
    map.insert(TokenType::LT, Precedence::LessGreater);
    map.insert(TokenType::GT, Precedence::LessGreater);
    map.insert(TokenType::Plus, Precedence::Sum);
    map.insert(TokenType::Minus, Precedence::Sum);
    map.insert(TokenType::Slash, Precedence::Product);
    map.insert(TokenType::Asterisk, Precedence::Product);
    map.insert(TokenType::LParen, Precedence::Call);
    map
});

use crate::ast::{
    BlockStatement, Boolean, CallExpression, ExpressionEnum, ExpressionStatement, FunctionLiteral,
    Identifier, IfExpression, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression,
    Program, ReturnStatement, StatementEnum, StringLiteral,
};

type PrefixParseFn = fn(&mut Parser) -> Result<ExpressionEnum, ParseError>;
type InfixParseFn =
    fn(&mut Parser, Option<Box<ExpressionEnum>>) -> Result<ExpressionEnum, ParseError>;

use crate::lexer::Lexer;
use crate::token::Token;

pub struct Parser {
    l: Lexer,

    cur_token: Option<Token>,
    peek_token: Option<Token>,

    errors: Vec<String>,

    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(l: Lexer) -> Self {
        let mut p = Parser {
            l,

            cur_token: None,
            peek_token: None,

            errors: Vec::new(),

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

        // 遇到 ( 时，也有可能时函数调用
        // 例如：add(1, 2 * 3, 4 + 5)，此时左括号 ( 为中缀表达式
        // 程序是如何区分 prefix 的 ( 和 infix 的 ( 的呢？
        // 如果是函数调用，( 的左边一定是一个 prefix （如 identifier）
        // 如果是普通的分组，( 的左边一定是一个 infix（如 operator） 或空
        // 因为前面是 infix，所以此时左括号成为了 prefix
        p.register_infix(TokenType::LParen, Self::parse_call_expression);

        // + - * / == != > < 时，当作中缀表达式 <expression><infix><expression>
        p.register_infix(TokenType::Plus, Self::parse_infix_expression);
        p.register_infix(TokenType::Minus, Self::parse_infix_expression);
        p.register_infix(TokenType::Slash, Self::parse_infix_expression);
        p.register_infix(TokenType::Asterisk, Self::parse_infix_expression);
        p.register_infix(TokenType::EQ, Self::parse_infix_expression);
        p.register_infix(TokenType::NotEQ, Self::parse_infix_expression);
        p.register_infix(TokenType::LT, Self::parse_infix_expression);
        p.register_infix(TokenType::GT, Self::parse_infix_expression);

        // 跳过两个 token，cur_token 和 peek_token 都会被赋值
        p.next_token();
        p.next_token();

        p
    }

    pub(crate) fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn peek_error(&mut self, token: TokenType) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            token,
            self.peek_token
                .as_ref()
                .map_or("none".to_string(), |t| t.token_type.to_string())
        ));
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.take();
        self.peek_token = Some(self.l.next_token());
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut program = Program { statements: vec![] };

        while self
            .cur_token
            .as_ref()
            .ok_or(ParseError::TokenIsNone)?
            .token_type
            != TokenType::EOF
        {
            let stmt = self.parse_statement();

            if let Ok(stmt) = stmt {
                program.statements.push(stmt);
            }

            self.next_token();
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<StatementEnum, ParseError> {
        let stmt = match self
            .cur_token
            .as_ref()
            .ok_or(ParseError::TokenIsNone)?
            .token_type
        {
            TokenType::Let => StatementEnum::LetStatement(self.parse_let_statement()?),
            TokenType::Return => StatementEnum::ReturnStatement(self.parse_return_statement()?),
            _ => StatementEnum::ExpressionStatement(self.parse_expression_statement()?),
        };

        Ok(stmt)
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        // 第一个一定是一个标识符
        if !self.expect_peek(TokenType::Identifier) {
            return Err(ParseError::ParseLetStatementError);
        }

        let name = Identifier {
            token: self.cur_token.clone().ok_or(ParseError::TokenIsNone)?,
            value: self
                .cur_token
                .as_ref()
                .ok_or(ParseError::TokenIsNone)?
                .literal
                .clone(),
        };

        // 第二个一定是 =
        if !self.expect_peek(TokenType::Assign) {
            return Err(ParseError::ParseLetStatementError);
        }

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
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

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
            token: self.cur_token.clone().ok_or(ParseError::TokenIsNone)?,
            expression: Some(self.parse_expression(Precedence::Lowest)?),
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
        let prefix = self.prefix_parse_fns.get(
            &self
                .cur_token
                .as_ref()
                .ok_or(ParseError::TokenIsNone)?
                .token_type,
        );

        match prefix {
            None => {
                self.no_prefix_parse_fn_error(
                    self.cur_token
                        .as_ref()
                        .ok_or(ParseError::TokenIsNone)?
                        .token_type,
                );
                Err(ParseError::ParseExpressionError)
            }
            Some(prefix) => {
                let mut left_exp = prefix(self)?;
                while !self.peek_token_is(TokenType::Semicolon)
                    && precedence < self.peek_precedence()
                {
                    let infix = self.infix_parse_fns.get(
                        &self
                            .peek_token
                            .as_ref()
                            .ok_or(ParseError::TokenIsNone)?
                            .token_type,
                    );

                    match infix.cloned() {
                        None => return Ok(left_exp),
                        Some(infix) => {
                            self.next_token();
                            left_exp = infix(self, Some(Box::new(left_exp)))?;
                        }
                    }
                }

                Ok(left_exp)
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        self.next_token();

        let right = Some(Box::new(self.parse_expression(Precedence::Prefix)?));

        Ok(ExpressionEnum::PrefixExpression(PrefixExpression {
            operator: token.literal.clone(),
            token,
            right,
        }))
    }

    fn parse_infix_expression(
        &mut self,
        left: Option<Box<ExpressionEnum>>,
    ) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        let precedence = self.cur_precedence();

        self.next_token();

        let right = Some(Box::new(self.parse_expression(precedence)?));

        Ok(ExpressionEnum::InfixExpression(InfixExpression {
            operator: token.literal.clone(),
            token,
            left,
            right,
        }))
    }

    fn parse_indentifier(&mut self) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        Ok(ExpressionEnum::Identifier(Identifier {
            value: token.literal.clone(),
            token,
        }))
    }

    fn parse_integer_literal(&mut self) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        match token.literal.parse::<i64>() {
            Ok(value) => Ok(ExpressionEnum::IntegerLiteral(IntegerLiteral {
                value,
                token,
            })),
            Err(_) => {
                self.errors
                    .push(format!("could not parse {} as integer", token.literal));
                Err(ParseError::ParseIntegerLiteralError)
            }
        }
    }

    fn parse_string_literal(&mut self) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        Ok(ExpressionEnum::StringLiteral(StringLiteral {
            value: token.literal.clone(),
            token,
        }))
    }

    fn parse_boolean(&mut self) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        Ok(ExpressionEnum::Boolean(Boolean {
            value: self.cur_token_is(TokenType::True),
            token,
        }))
    }

    fn parse_grouped_expression(&mut self) -> Result<ExpressionEnum, ParseError> {
        // skip (
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(TokenType::RParen) {
            return Err(ParseError::ParseGroupedExpressionError);
        }

        exp
    }

    /// 解析 if 表达式
    ///
    /// if (<condition>) <consequence> else <alternative>
    fn parse_if_expression(&mut self) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        if !self.expect_peek(TokenType::LParen) {
            return Err(ParseError::ParseIfExpressionError);
        }

        self.next_token();

        let condition = Some(Box::new(self.parse_expression(Precedence::Lowest)?));

        if !self.expect_peek(TokenType::RParen) {
            return Err(ParseError::ParseIfExpressionError);
        }

        if !self.expect_peek(TokenType::LBrace) {
            return Err(ParseError::ParseIfExpressionError);
        }

        let consequence = self.parse_block_statement().ok();
        let mut alternative = None;
        if self.peek_token_is(TokenType::Else) {
            self.next_token();

            if !self.expect_peek(TokenType::LBrace) {
                return Err(ParseError::ParseIfExpressionError);
            }

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
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

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
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        if !self.expect_peek(TokenType::LParen) {
            return Err(ParseError::ParseFunctionLiteralError);
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(TokenType::LBrace) {
            return Err(ParseError::ParseFunctionLiteralError);
        }

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
            token: self.cur_token.clone().ok_or(ParseError::TokenIsNone)?,
            value: self
                .cur_token
                .as_ref()
                .ok_or(ParseError::TokenIsNone)?
                .literal
                .clone(),
        };

        identifiers.push(ident);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token(); // 当前 -> COMMA
            self.next_token(); // COMMA -> 下一个参数

            let ident = Identifier {
                token: self.cur_token.clone().ok_or(ParseError::TokenIsNone)?,
                value: self
                    .cur_token
                    .as_ref()
                    .ok_or(ParseError::TokenIsNone)?
                    .literal
                    .clone(),
            };

            identifiers.push(ident);
        }

        if !self.expect_peek(TokenType::RParen) {
            return Err(ParseError::ParseFunctionLiteralError);
        }

        Ok(identifiers)
    }

    /// 解析函数调用
    ///
    /// <expression>(<param1>, <param2>, ...)
    fn parse_call_expression(
        &mut self,
        function: Option<Box<ExpressionEnum>>,
    ) -> Result<ExpressionEnum, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        let arguments = self.parse_call_arguments()?;

        Ok(ExpressionEnum::CallExpression(CallExpression {
            token,
            function: function.ok_or(ParseError::TokenIsNone)?,
            arguments,
        }))
    }

    // 解析表达式列表
    // <param1>, <param2>, ...)
    fn parse_call_arguments(&mut self) -> Result<Vec<ExpressionEnum>, ParseError> {
        let mut args = vec![];

        // 空参数
        if self.peek_token_is(TokenType::RParen) {
            self.next_token();
            return Ok(args);
        }

        self.next_token();

        let arg = self.parse_expression(Precedence::Lowest);

        if let Ok(arg) = arg {
            args.push(arg);
        }

        while self.peek_token_is(TokenType::Comma) {
            self.next_token(); // 当前 -> COMMA
            self.next_token(); // COMMA -> 下一个参数

            let arg = self.parse_expression(Precedence::Lowest);

            if let Ok(arg) = arg {
                args.push(arg);
            }
        }

        if !self.expect_peek(TokenType::RParen) {
            return Err(ParseError::ParseCallArgumentsError);
        }

        Ok(args)
    }

    /// 检查下一个 token 是否是期望的类型（会消耗一个 token）
    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    /// 检查当前 token 是否是期望的类型
    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token
            .as_ref()
            .map_or(false, |x| x.token_type == t)
    }

    /// 检查下一个 token 是否是期望的类型
    fn cur_token_is(&self, t: TokenType) -> bool {
        self.cur_token.as_ref().map_or(false, |x| x.token_type == t)
    }

    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
    }

    fn no_prefix_parse_fn_error(&mut self, t: TokenType) {
        self.errors
            .push(format!("no prefix parse function for {:?} found", t));
    }

    fn peek_precedence(&self) -> Precedence {
        self.peek_token.as_ref().map_or(Precedence::Lowest, |x| {
            PRECEDENCES
                .get(&x.token_type)
                .map_or(Precedence::Lowest, |x| *x)
        })
    }

    fn cur_precedence(&self) -> Precedence {
        self.cur_token.as_ref().map_or(Precedence::Lowest, |x| {
            PRECEDENCES
                .get(&x.token_type)
                .map_or(Precedence::Lowest, |x| *x)
        })
    }
}

#[cfg(test)]
mod test {

    use core::panic;

    use super::Parser;
    use crate::{
        ast::{ExpressionEnum, Node, StatementEnum},
        lexer::Lexer,
    };

    enum Value {
        Integer(i64),
        Boolean(bool),
        Text(String),
    }

    fn check_parse_errors(p: &Parser) {
        let errors = p.errors();
        if errors.len() == 0 {
            return;
        }
        eprintln!("parser has {} errors", errors.len());

        for msg in errors {
            eprintln!("parser error: {}", msg);
        }

        panic!();
    }

    #[test]
    fn test_let_statement() {
        let tests = [
            ("let x = 5;", "x", Value::Integer(5)),
            ("let y = true;", "y", Value::Boolean(true)),
            ("let foobar = y;", "foobar", Value::Text("y".to_string())),
        ];

        for tt in tests {
            let (input, expected_indent, expected_value) = tt;
            let l = Lexer::new(input.to_string());
            let mut p = Parser::new(l);

            let program = p.parse_program();
            check_parse_errors(&p);

            assert!(program.is_ok(), "parse_program() returned Error");

            let program = program.unwrap();

            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain {} statements. got={}",
                1,
                program.statements.len()
            );

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
            ("return foobar;", Value::Text("foobar".to_string())),
        ];

        for tt in tests {
            let (input, expected_value) = tt;
            let l = Lexer::new(input.to_string());
            let mut p = Parser::new(l);

            let program = p.parse_program();
            check_parse_errors(&p);

            assert!(program.is_ok(), "parse_program() returned Error");

            let program = program.unwrap();

            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain {} statements. got={}",
                1,
                program.statements.len()
            );

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

        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parse_errors(&p);

        assert!(program.is_ok(), "parse_program() returned Error");

        let program = program.unwrap();

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain {} statements. got={}",
            1,
            program.statements.len()
        );

        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = stmt.expression.as_ref().unwrap();
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

        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parse_errors(&p);

        assert!(program.is_ok(), "parse_program() returned Error");

        let program = program.unwrap();

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain {} statements. got={}",
            1,
            program.statements.len()
        );

        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = stmt.expression.as_ref().unwrap();
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
            let l = Lexer::new(input.to_string());
            let mut p = Parser::new(l);

            let program = p.parse_program();
            check_parse_errors(&p);

            assert!(program.is_ok(), "parse_program() returned Error");

            let program = program.unwrap();

            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain {} statements. got={}",
                1,
                program.statements.len()
            );

            let stmt = &program.statements[0];

            if let StatementEnum::ExpressionStatement(stmt) = stmt {
                let exp = stmt.expression.as_ref().unwrap();
                if let ExpressionEnum::PrefixExpression(exp) = exp {
                    assert_eq!(
                        exp.operator, operator,
                        "exp.operator is not '{}', got={}",
                        operator, exp.operator
                    );

                    assert!(_test_literal_expression(
                        exp.right.as_ref().unwrap().as_ref(),
                        value
                    ));
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
            let l = Lexer::new(input.to_string());
            let mut p = Parser::new(l);

            let program = p.parse_program();
            check_parse_errors(&p);

            assert!(program.is_ok(), "parse_program() returned Error");

            let program = program.unwrap();

            assert_eq!(
                program.statements.len(),
                1,
                "program.statements does not contain {} statements. got={}",
                1,
                program.statements.len()
            );

            let stmt = &program.statements[0];

            if let StatementEnum::ExpressionStatement(exp_stmt) = stmt {
                assert!(_test_infix_expression(
                    exp_stmt.expression.as_ref().unwrap(),
                    left_value,
                    operator,
                    right_value,
                ));
            } else {
            }
            // let exp_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>();

            // assert!(
            //     exp_stmt.is_some(),
            //     "stmt is not ExpressionStatement, got={:?}",
            //     stmt
            // );
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
        ];

        for tt in tests {
            let (input, expected) = tt;
            let l = Lexer::new(input.to_string());
            let mut p = Parser::new(l);

            let program = p.parse_program();
            check_parse_errors(&p);

            assert!(program.is_ok(), "parse_program() returned Error");

            let program = program.unwrap();

            let actual = program.to_string();

            assert_eq!(actual, expected, "expected={}, got={}", expected, actual);
        }
    }

    #[test]
    fn test_boolean_expression() {
        let input = "true;";

        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parse_errors(&p);

        assert!(program.is_ok(), "parse_program() returned Error");

        let program = program.unwrap();

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain {} statements. got={}",
            1,
            program.statements.len()
        );

        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = stmt.expression.as_ref().unwrap();
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

        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parse_errors(&p);

        assert!(program.is_ok(), "parse_program() returned Error");

        let program = program.unwrap();

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain {} statements. got={}",
            1,
            program.statements.len()
        );

        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = stmt.expression.as_ref().unwrap();
            if let ExpressionEnum::IfExpression(if_exp) = exp {
                assert!(_test_infix_expression(
                    if_exp.condition.as_ref().unwrap().as_ref(),
                    Value::Text("x".to_string()),
                    "<",
                    Value::Text("y".to_string()),
                ));

                assert_eq!(
                    if_exp.consequence.as_ref().unwrap().statements.len(),
                    1,
                    "consequence is not 1 statements, got={}",
                    if_exp.consequence.as_ref().unwrap().statements.len()
                );

                let consequence = &if_exp.consequence.as_ref().unwrap().statements[0];

                if let StatementEnum::ExpressionStatement(consequence) = consequence {
                    let consequence_exp = consequence.expression.as_ref().unwrap();
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

        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parse_errors(&p);

        assert!(program.is_ok(), "parse_program() returned Error");

        let program = program.unwrap();

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain {} statements. got={}",
            1,
            program.statements.len()
        );

        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = stmt.expression.as_ref().unwrap();
            if let ExpressionEnum::IfExpression(if_exp) = exp {
                assert!(_test_infix_expression(
                    if_exp.condition.as_ref().unwrap().as_ref(),
                    Value::Text("x".to_string()),
                    "<",
                    Value::Text("y".to_string()),
                ));

                assert_eq!(
                    if_exp.consequence.as_ref().unwrap().statements.len(),
                    1,
                    "consequence is not 1 statements, got={}",
                    if_exp.consequence.as_ref().unwrap().statements.len()
                );

                let consequence = &if_exp.consequence.as_ref().unwrap().statements[0];

                if let StatementEnum::ExpressionStatement(consequence) = consequence {
                    let consequence_exp = consequence.expression.as_ref().unwrap();
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
                    let alternative_exp = alternative.expression.as_ref().unwrap();
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

        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parse_errors(&p);

        assert!(program.is_ok(), "parse_program() returned Error");

        let program = program.unwrap();

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain {} statements. got={}",
            1,
            program.statements.len()
        );

        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = stmt.expression.as_ref().unwrap();
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
                    let body_exp = body_stmt.expression.as_ref().unwrap();
                    assert!(_test_infix_expression(
                        body_exp,
                        Value::Text("x".to_string()),
                        "+",
                        Value::Text("y".to_string()),
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
            let l = Lexer::new(input.to_string());
            let mut p = Parser::new(l);

            let program = p.parse_program();
            check_parse_errors(&p);

            assert!(program.is_ok(), "parse_program() returned Error");

            let program = program.unwrap();

            let stmt = &program.statements[0];

            if let StatementEnum::ExpressionStatement(stmt) = stmt {
                let exp = stmt.expression.as_ref().unwrap();
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

        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parse_errors(&p);

        assert!(program.is_ok(), "parse_program() returned Error");

        let program = program.unwrap();

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain {} statements. got={}",
            1,
            program.statements.len()
        );

        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = stmt.expression.as_ref().unwrap();
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
            assert!(_test_literal_expression(
                op_exp.left.as_ref().unwrap().as_ref(),
                left
            ));

            assert_eq!(
                op_exp.operator, operator,
                "exp.operator is not '{}', got={}",
                operator, op_exp.operator
            );

            assert!(_test_literal_expression(
                op_exp.right.as_ref().unwrap().as_ref(),
                right
            ));

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
            Value::Text(v) => _test_identifier(exp, &v),
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
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parse_errors(&p);

        assert!(program.is_ok(), "parse_program() returned Error");

        let program = program.unwrap();
        let stmt = &program.statements[0];

        if let StatementEnum::ExpressionStatement(stmt) = stmt {
            let exp = stmt.expression.as_ref().unwrap();
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
}
