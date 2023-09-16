use std::collections::HashMap;

use lazy_static::lazy_static;

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
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunction(X)
}

lazy_static! {
    static ref PRECEDENCES: HashMap<TokenType, Precedence> = {
        let mut map = HashMap::new();
        map.insert(TokenType::EQ, Precedence::EQUALS);
        map.insert(TokenType::NOTEQ, Precedence::EQUALS);
        map.insert(TokenType::LT, Precedence::LESSGREATER);
        map.insert(TokenType::GT, Precedence::LESSGREATER);
        map.insert(TokenType::PLUS, Precedence::SUM);
        map.insert(TokenType::MINUS, Precedence::SUM);
        map.insert(TokenType::SLASH, Precedence::PRODUCT);
        map.insert(TokenType::ASTERISK, Precedence::PRODUCT);
        map.insert(TokenType::LPAREN, Precedence::CALL);
        map
    };
}

use crate::ast::{
    BlockStatement, Boolean, CallExpression, Expression, ExpressionStatement, Identifier,
    InfixExpression, IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement,
    Statement,
};

type PrefixParseFn = fn(&mut Parser) -> Result<Box<dyn Expression>, ParseError>;
type InfixParseFn =
    fn(&mut Parser, Option<Box<dyn Expression>>) -> Result<Box<dyn Expression>, ParseError>;

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

        p.register_prefix(TokenType::IDENT, Self::parse_indentifier);
        p.register_prefix(TokenType::INT, Self::parse_integer_literal);
        p.register_prefix(TokenType::TRUE, Self::parse_boolean);
        p.register_prefix(TokenType::FALSE, Self::parse_boolean);
        // 遇到 ! 和 - 时，当作整个前缀表达式 <prefix><expression>
        p.register_prefix(TokenType::BANG, Self::parse_prefix_expression);
        p.register_prefix(TokenType::MINUS, Self::parse_prefix_expression);
        // 遇到 ( 时，启用分组
        p.register_prefix(TokenType::LPAREN, Self::parse_grouped_expression);
        // 遇到 <if> 时，调用 parseIfExpression
        p.register_prefix(TokenType::IF, Self::parse_if_expression);

        // 遇到 <function> 时，调用 parseFunctionLiteral
        p.register_prefix(TokenType::FUNCTION, Self::parse_function_literal);

        // 遇到 ( 时，也有可能时函数调用
        // 例如：add(1, 2 * 3, 4 + 5)，此时左括号 ( 为中缀表达式
        // 程序是如何区分 prefix 的 ( 和 infix 的 ( 的呢？
        // 如果是函数调用，( 的左边一定是一个 prefix （如 identifier）
        // 如果是普通的分组，( 的左边一定是一个 infix（如 operator） 或空
        // 因为前面是 infix，所以此时左括号成为了 prefix
        p.register_infix(TokenType::LPAREN, Self::parse_call_expression);

        // + - * / == != > < 时，当作中缀表达式 <expression><infix><expression>
        p.register_infix(TokenType::PLUS, Self::parse_infix_expression);
        p.register_infix(TokenType::MINUS, Self::parse_infix_expression);
        p.register_infix(TokenType::SLASH, Self::parse_infix_expression);
        p.register_infix(TokenType::ASTERISK, Self::parse_infix_expression);
        p.register_infix(TokenType::EQ, Self::parse_infix_expression);
        p.register_infix(TokenType::NOTEQ, Self::parse_infix_expression);
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

    fn parse_statement(&mut self) -> Result<Box<dyn Statement>, ParseError> {
        let stmt: Box<dyn Statement> = match self
            .cur_token
            .as_ref()
            .ok_or(ParseError::TokenIsNone)?
            .token_type
        {
            TokenType::LET => Box::new(self.parse_let_statement()?) as Box<dyn Statement>,
            TokenType::RETURN => Box::new(self.parse_return_statement()?) as Box<dyn Statement>,
            _ => Box::new(self.parse_expression_statement()?) as Box<dyn Statement>,
        };

        Ok(stmt)
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        // 第一个一定是一个标识符
        if !self.expect_peek(TokenType::IDENT) {
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
        if !self.expect_peek(TokenType::ASSIGN) {
            return Err(ParseError::ParseLetStatementError);
        }

        // skip =
        self.next_token();

        let value = Some(self.parse_expression(Precedence::LOWEST)?);

        if self.peek_token_is(TokenType::SEMICOLON) {
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

        let return_value = Some(self.parse_expression(Precedence::LOWEST)?);

        if self.peek_token_is(TokenType::SEMICOLON) {
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
            expression: Some(self.parse_expression(Precedence::LOWEST)?),
        };

        // 表达式结尾可以没有分号（用于 REPL，如敲下 5 + 5 并回车）
        // 但是如果有分号，就要跳过分号
        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(stmt)
    }

    /// 解析表达式
    ///
    /// `expression`
    fn parse_expression(
        &mut self,
        precedence: Precedence,
    ) -> Result<Box<dyn Expression>, ParseError> {
        let prefix = self.prefix_parse_fns.get(
            &self
                .cur_token
                .as_ref()
                .ok_or(ParseError::TokenIsNone)?
                .token_type,
        );

        match prefix {
            None => {
                self.no_prefix_parse_fn_error(self.cur_token.as_ref().unwrap().token_type);
                Err(ParseError::ParseExpressionError)
            }
            Some(prefix) => {
                let mut left_exp = prefix(self)?;
                while !self.peek_token_is(TokenType::SEMICOLON)
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
                            left_exp = infix(self, Some(left_exp))?;
                        }
                    }
                }

                Ok(left_exp)
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Box<dyn Expression>, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        self.next_token();

        let right = Some(self.parse_expression(Precedence::PREFIX)?);

        Ok(Box::new(PrefixExpression {
            operator: token.literal.clone(),
            token,
            right,
        }))
    }

    fn parse_infix_expression(
        &mut self,
        left: Option<Box<dyn Expression>>,
    ) -> Result<Box<dyn Expression>, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        let precedence = self.cur_precedence();

        self.next_token();

        let right = Some(self.parse_expression(precedence)?);

        Ok(Box::new(InfixExpression {
            operator: token.literal.clone(),
            token,
            left,
            right,
        }))
    }

    fn parse_indentifier(&mut self) -> Result<Box<dyn Expression>, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        Ok(Box::new(Identifier {
            value: token.literal.clone(),
            token,
        }))
    }

    fn parse_integer_literal(&mut self) -> Result<Box<dyn Expression>, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        match token.literal.parse::<i64>() {
            Ok(value) => Ok(Box::new(IntegerLiteral { value, token })),
            Err(_) => {
                self.errors
                    .push(format!("could not parse {} as integer", token.literal));
                Err(ParseError::ParseIntegerLiteralError)
            }
        }
    }

    fn parse_boolean(&mut self) -> Result<Box<dyn Expression>, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        Ok(Box::new(Boolean {
            value: self.cur_token_is(TokenType::TRUE),
            token,
        }))
    }

    fn parse_grouped_expression(&mut self) -> Result<Box<dyn Expression>, ParseError> {
        // skip (
        self.next_token();

        let exp = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(TokenType::RPAREN) {
            return Err(ParseError::ParseGroupedExpressionError);
        }

        exp
    }

    /// 解析 if 表达式
    ///
    /// if (<condition>) <consequence> else <alternative>
    fn parse_if_expression(&mut self) -> Result<Box<dyn Expression>, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        if !self.expect_peek(TokenType::LPAREN) {
            return Err(ParseError::ParseIfExpressionError);
        }

        self.next_token();

        let condition = Some(self.parse_expression(Precedence::LOWEST)?);

        if !self.expect_peek(TokenType::RPAREN) {
            return Err(ParseError::ParseIfExpressionError);
        }

        if !self.expect_peek(TokenType::LBRACE) {
            return Err(ParseError::ParseIfExpressionError);
        }

        let consequence = self.parse_block_statement().ok();
        let mut alternative = None;
        if self.peek_token_is(TokenType::ELSE) {
            self.next_token();

            if !self.expect_peek(TokenType::LBRACE) {
                return Err(ParseError::ParseIfExpressionError);
            }

            alternative = self.parse_block_statement().ok();
        }

        Ok(Box::new(crate::ast::IfExpression {
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

        while !self.cur_token_is(TokenType::RBRACE) && !self.cur_token_is(TokenType::EOF) {
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
    fn parse_function_literal(&mut self) -> Result<Box<dyn Expression>, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        if !self.expect_peek(TokenType::LPAREN) {
            return Err(ParseError::ParseFunctionLiteralError);
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(TokenType::LBRACE) {
            return Err(ParseError::ParseFunctionLiteralError);
        }

        let body = self.parse_block_statement()?;

        Ok(Box::new(crate::ast::FunctionLiteral {
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
        if self.peek_token_is(TokenType::RPAREN) {
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

        while self.peek_token_is(TokenType::COMMA) {
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

        if !self.expect_peek(TokenType::RPAREN) {
            return Err(ParseError::ParseFunctionLiteralError);
        }

        Ok(identifiers)
    }

    /// 解析函数调用
    ///
    /// <expression>(<param1>, <param2>, ...)
    fn parse_call_expression(
        &mut self,
        function: Option<Box<dyn Expression>>,
    ) -> Result<Box<dyn Expression>, ParseError> {
        let token = self.cur_token.clone().ok_or(ParseError::TokenIsNone)?;

        let arguments = self.parse_call_arguments()?;

        Ok(Box::new(CallExpression {
            token,
            function: function.unwrap(),
            arguments,
        }))
    }

    // 解析表达式列表
    // <param1>, <param2>, ...)
    fn parse_call_arguments(&mut self) -> Result<Vec<Box<dyn Expression>>, ParseError> {
        let mut args = vec![];

        // 空参数
        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Ok(args);
        }

        self.next_token();

        let arg = self.parse_expression(Precedence::LOWEST);

        if let Ok(arg) = arg {
            args.push(arg);
        }

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token(); // 当前 -> COMMA
            self.next_token(); // COMMA -> 下一个参数

            let arg = self.parse_expression(Precedence::LOWEST);

            if let Ok(arg) = arg {
                args.push(arg);
            }
        }

        if !self.expect_peek(TokenType::RPAREN) {
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
        self.peek_token.as_ref().map_or(Precedence::LOWEST, |x| {
            PRECEDENCES
                .get(&x.token_type)
                .map_or(Precedence::LOWEST, |x| *x)
        })
    }

    fn cur_precedence(&self) -> Precedence {
        self.cur_token.as_ref().map_or(Precedence::LOWEST, |x| {
            PRECEDENCES
                .get(&x.token_type)
                .map_or(Precedence::LOWEST, |x| *x)
        })
    }
}

#[cfg(test)]
mod test {

    use super::Parser;
    use crate::{
        ast::{
            Expression, ExpressionStatement, Identifier, IntegerLiteral, LetStatement, Node,
            Statement,
        },
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

            let stmt = program.statements[0].as_ref();

            assert!(_test_let_statement(stmt, expected_indent));

            let exp = stmt
                .as_any()
                .downcast_ref::<LetStatement>()
                .as_ref()
                .unwrap()
                .value
                .as_ref()
                .unwrap()
                .as_ref();

            assert!(_test_literal_expression(exp, expected_value));
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

            let stmt = program.statements[0].as_ref();

            assert_eq!(
                stmt.token_literal(),
                "return",
                "stmt.token_literal not 'return', got={}",
                stmt.token_literal()
            );

            let exp = stmt
                .as_any()
                .downcast_ref::<crate::ast::ReturnStatement>()
                .as_ref()
                .unwrap()
                .return_value
                .as_ref()
                .unwrap()
                .as_ref();

            assert!(_test_literal_expression(exp, expected_value));
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

        let stmt = program.statements[0].as_ref();

        let exp_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>();

        assert!(
            exp_stmt.is_some(),
            "stmt is not ExpressionStatement, got={:?}",
            stmt
        );

        let ident = exp_stmt
            .unwrap()
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<Identifier>();

        assert!(ident.is_some(), "exp not Identifier. got={:?}", ident);

        assert_eq!(
            ident.unwrap().value,
            "foobar",
            "ident.value not 'foobar', got = {}",
            ident.unwrap().value
        );

        assert_eq!(
            ident.unwrap().token_literal(),
            "foobar",
            "ident.token_literal not 'foobar', got = {}",
            ident.unwrap().token_literal()
        );
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

        let stmt = program.statements[0].as_ref();

        let exp_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>();

        assert!(
            exp_stmt.is_some(),
            "stmt is not ExpressionStatement, got={:?}",
            stmt
        );

        let literal = exp_stmt
            .unwrap()
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<IntegerLiteral>();

        assert!(
            literal.is_some(),
            "exp not IntegerLiteral. got={:?}",
            literal
        );

        assert_eq!(
            literal.unwrap().value,
            5,
            "literal.value not '5', got = {}",
            literal.unwrap().value
        );

        assert_eq!(
            literal.unwrap().token_literal(),
            "5",
            "literal.token_literal not '5', got = {}",
            literal.unwrap().token_literal()
        );
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

            let stmt = program.statements[0].as_ref();

            let exp_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>();

            assert!(
                exp_stmt.is_some(),
                "stmt is not ExpressionStatement, got={:?}",
                stmt
            );

            let exp = exp_stmt
                .unwrap()
                .expression
                .as_ref()
                .unwrap()
                .as_any()
                .downcast_ref::<crate::ast::PrefixExpression>();

            assert!(exp.is_some(), "exp is not PrefixExpression, got={:?}", exp);

            assert_eq!(
                exp.unwrap().operator,
                operator,
                "exp.operator is not '{}', got={}",
                operator,
                exp.unwrap().operator
            );

            assert!(_test_literal_expression(
                exp.unwrap().right.as_ref().unwrap().as_ref(),
                value
            ));
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

            let stmt = program.statements[0].as_ref();

            let exp_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>();

            assert!(
                exp_stmt.is_some(),
                "stmt is not ExpressionStatement, got={:?}",
                stmt
            );

            assert!(_test_infix_expression(
                exp_stmt
                    .as_ref()
                    .unwrap()
                    .expression
                    .as_ref()
                    .unwrap()
                    .as_ref(),
                left_value,
                operator,
                right_value,
            ));
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

        let stmt = program.statements[0].as_ref();

        let exp_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>();

        assert!(
            exp_stmt.is_some(),
            "stmt is not ExpressionStatement, got={:?}",
            stmt
        );

        let boolean = exp_stmt
            .unwrap()
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<crate::ast::Boolean>();

        assert!(boolean.is_some(), "exp not Boolean. got={:?}", boolean);

        assert_eq!(
            boolean.unwrap().value,
            true,
            "boolean.value not 'true', got = {}",
            boolean.unwrap().value
        );

        assert_eq!(
            boolean.unwrap().token_literal(),
            "true",
            "boolean.token_literal not 'true', got = {}",
            boolean.unwrap().token_literal()
        );
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

        let stmt = program.statements[0].as_ref();

        let exp_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>();

        assert!(
            exp_stmt.is_some(),
            "stmt is not ExpressionStatement, got={:?}",
            stmt
        );

        let exp = exp_stmt
            .unwrap()
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<crate::ast::IfExpression>();

        assert!(exp.is_some(), "exp is not IfExpression, got={:?}", exp);

        assert!(_test_infix_expression(
            exp.unwrap().condition.as_ref().unwrap().as_ref(),
            Value::Text("x".to_string()),
            "<",
            Value::Text("y".to_string()),
        ));

        assert_eq!(
            exp.unwrap().consequence.as_ref().unwrap().statements.len(),
            1,
            "consequence is not 1 statements, got={}",
            exp.unwrap().consequence.as_ref().unwrap().statements.len()
        );

        let consequence = exp.unwrap().consequence.as_ref().unwrap().statements[0].as_ref();

        let consequence = consequence.as_any().downcast_ref::<ExpressionStatement>();

        assert!(consequence.is_some(), "Statements[0] is not Identifier");

        assert!(_test_identifier(
            consequence
                .as_ref()
                .unwrap()
                .expression
                .as_ref()
                .unwrap()
                .as_ref(),
            "x".to_string()
        ));

        assert!(
            exp.unwrap().alternative.is_none(),
            "exp.alternative.statements was not None, got={:?}",
            exp.unwrap().alternative
        );

        assert!(exp.unwrap().alternative.is_none());
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

        let stmt = program.statements[0].as_ref();

        let exp_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>();

        assert!(
            exp_stmt.is_some(),
            "stmt is not ExpressionStatement, got={:?}",
            stmt
        );

        let exp = exp_stmt
            .unwrap()
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<crate::ast::IfExpression>();

        assert!(exp.is_some(), "exp is not IfExpression, got={:?}", exp);

        assert!(_test_infix_expression(
            exp.unwrap().condition.as_ref().unwrap().as_ref(),
            Value::Text("x".to_string()),
            "<",
            Value::Text("y".to_string()),
        ));

        assert_eq!(
            exp.unwrap().consequence.as_ref().unwrap().statements.len(),
            1,
            "consequence is not 1 statements, got={}",
            exp.unwrap().consequence.as_ref().unwrap().statements.len()
        );

        let consequence = exp.unwrap().consequence.as_ref().unwrap().statements[0].as_ref();

        let consequence = consequence.as_any().downcast_ref::<ExpressionStatement>();

        assert!(consequence.is_some(), "Statements[0] is not Identifier");

        assert!(_test_identifier(
            consequence
                .as_ref()
                .unwrap()
                .expression
                .as_ref()
                .unwrap()
                .as_ref(),
            "x".to_string()
        ));

        assert_eq!(
            exp.unwrap().alternative.as_ref().unwrap().statements.len(),
            1,
            "alternative is not 1 statements, got={}",
            exp.unwrap().alternative.as_ref().unwrap().statements.len()
        );

        let alternative = exp.unwrap().alternative.as_ref().unwrap().statements[0].as_ref();

        let alternative = alternative.as_any().downcast_ref::<ExpressionStatement>();

        assert!(alternative.is_some(), "Statements[0] is not Identifier");

        assert!(_test_identifier(
            alternative
                .as_ref()
                .unwrap()
                .expression
                .as_ref()
                .unwrap()
                .as_ref(),
            "y".to_string()
        ));
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

        let stmt = program.statements[0].as_ref();

        let exp_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>();

        assert!(
            exp_stmt.is_some(),
            "stmt is not ExpressionStatement, got={:?}",
            stmt
        );

        let function = exp_stmt
            .unwrap()
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<crate::ast::FunctionLiteral>();

        assert!(
            function.is_some(),
            "exp is not FunctionLiteral, got={:?}",
            function
        );

        assert_eq!(
            function.unwrap().parameters.len(),
            2,
            "function literal parameters wrong. want 2, got={}",
            function.unwrap().parameters.len()
        );

        assert_eq!(
            function.unwrap().parameters[0].value,
            "x",
            "parameter is not 'x', got={}",
            function.unwrap().parameters[0].value
        );

        assert_eq!(
            function.unwrap().parameters[1].value,
            "y",
            "parameter is not 'y', got={}",
            function.unwrap().parameters[1].value
        );

        assert_eq!(
            function.unwrap().body.statements.len(),
            1,
            "function.body.statements has not 1 statements. got={}",
            function.unwrap().body.statements.len()
        );

        let body_stmt = function.unwrap().body.statements[0].as_ref();

        let body_stmt = body_stmt.as_any().downcast_ref::<ExpressionStatement>();

        assert!(
            body_stmt.is_some(),
            "function body stmt is not ExpressionStatement"
        );

        assert!(_test_infix_expression(
            body_stmt
                .as_ref()
                .unwrap()
                .expression
                .as_ref()
                .unwrap()
                .as_ref(),
            Value::Text("x".to_string()),
            "+",
            Value::Text("y".to_string()),
        ));
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

            let stmt = program.statements[0].as_ref();

            let exp_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>();

            assert!(
                exp_stmt.is_some(),
                "stmt is not ExpressionStatement, got={:?}",
                stmt
            );

            let function = exp_stmt
                .unwrap()
                .expression
                .as_ref()
                .unwrap()
                .as_any()
                .downcast_ref::<crate::ast::FunctionLiteral>();

            assert!(
                function.is_some(),
                "exp is not FunctionLiteral, got={:?}",
                function
            );

            assert_eq!(
                function.unwrap().parameters.len(),
                expected_params.len(),
                "length parameters wrong. want {}, got={}",
                expected_params.len(),
                function.unwrap().parameters.len()
            );

            for (i, ident) in expected_params.iter().enumerate() {
                assert_eq!(
                    function.unwrap().parameters[i].value,
                    ident.to_string(),
                    "parameter is not {}, got={}",
                    ident,
                    function.unwrap().parameters[i].value
                );
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

        let stmt = program.statements[0].as_ref();

        let exp_stmt = stmt.as_any().downcast_ref::<ExpressionStatement>();

        assert!(
            exp_stmt.is_some(),
            "stmt is not ExpressionStatement, got={:?}",
            stmt
        );

        let exp = exp_stmt
            .unwrap()
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<crate::ast::CallExpression>();

        assert!(exp.is_some(), "exp is not CallExpression, got={:?}", exp);

        assert!(_test_identifier(
            exp.unwrap().function.as_ref(),
            "add".to_string()
        ));

        assert_eq!(
            exp.unwrap().arguments.len(),
            3,
            "wrong length of arguments, got={}",
            exp.unwrap().arguments.len()
        );

        assert!(_test_literal_expression(
            exp.unwrap().arguments[0].as_ref(),
            Value::Integer(1)
        ));

        assert!(_test_infix_expression(
            exp.unwrap().arguments[1].as_ref(),
            Value::Integer(2),
            "*",
            Value::Integer(3)
        ));

        assert!(_test_infix_expression(
            exp.unwrap().arguments[2].as_ref(),
            Value::Integer(4),
            "+",
            Value::Integer(5)
        ));
    }

    fn _test_infix_expression(
        exp: &dyn Expression,
        left: Value,
        operator: &str,
        right: Value,
    ) -> bool {
        let op_exp = exp.as_any().downcast_ref::<crate::ast::InfixExpression>();

        assert!(
            op_exp.is_some(),
            "exp is not InfixExpression, got={:?}",
            exp
        );

        assert!(_test_literal_expression(
            op_exp.unwrap().left.as_ref().unwrap().as_ref(),
            left
        ));

        assert_eq!(
            op_exp.unwrap().operator,
            operator,
            "exp.operator is not '{}', got={}",
            operator,
            op_exp.unwrap().operator
        );

        assert!(_test_literal_expression(
            op_exp.unwrap().right.as_ref().unwrap().as_ref(),
            right
        ));

        true
    }

    fn _test_let_statement(s: &dyn Statement, expected_name: &str) -> bool {
        let s = s.as_any().downcast_ref::<LetStatement>();

        assert!(s.is_some(), "s is not LetStatement, got={:?}", s);
        let s = s.unwrap();
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
    }

    fn _test_literal_expression(exp: &dyn Expression, expected: Value) -> bool {
        match expected {
            Value::Boolean(v) => _test_bool_literal(exp, v),
            Value::Integer(v) => _test_integer_literal(exp, v),
            Value::Text(v) => _test_identifier(exp, v),
        }
    }

    fn _test_identifier(exp: &dyn Expression, expected: String) -> bool {
        let ident = exp.as_any().downcast_ref::<Identifier>();

        assert!(ident.is_some(), "exp not Identifier. got={:?}", exp);

        assert_eq!(
            ident.unwrap().value,
            expected,
            "ident.value not {}. got={}",
            expected,
            ident.unwrap().value
        );

        assert_eq!(
            ident.unwrap().token_literal(),
            expected,
            "ident.token_literal not {}. got={}",
            expected,
            ident.unwrap().token_literal()
        );
        true
    }

    fn _test_bool_literal(exp: &dyn Expression, expected: bool) -> bool {
        let bl = exp.as_any().downcast_ref::<crate::ast::Boolean>();
        assert!(bl.is_some(), "exp not Boolean. got={:?}", exp);

        assert_eq!(
            bl.unwrap().value,
            expected,
            "bl.value not {}. got={}",
            expected,
            bl.unwrap().value
        );

        assert_eq!(
            bl.unwrap().token_literal(),
            format!("{}", expected),
            "bl.token_literal not {}. got={}",
            expected,
            bl.unwrap().token_literal()
        );

        true
    }

    fn _test_integer_literal(exp: &dyn Expression, expected: i64) -> bool {
        let il = exp.as_any().downcast_ref::<crate::ast::IntegerLiteral>();

        assert!(il.is_some(), "exp not IntegerLiteral. got={:?}", exp);

        assert_eq!(
            il.unwrap().value,
            expected,
            "il.value not {}. got={}",
            expected,
            il.unwrap().value
        );

        assert_eq!(
            il.unwrap().token_literal(),
            format!("{}", expected),
            "il.token_literal not {}. got={}",
            expected,
            il.unwrap().token_literal()
        );

        true
    }
}
