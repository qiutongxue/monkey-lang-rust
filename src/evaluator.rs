use crate::{
    ast::{
        BlockStatement, ExpressionEnum, Identifier, IfExpression, NodeEnum, Program, StatementEnum,
    },
    object::{Environment, Object},
};

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

#[derive(Debug)]
pub enum EvalError {
    UnknownOperator(String), // -BOOLEAN, BOOLEAN + BOOLEAN
    TypeMismatch(String),    // BOOLEAN + INTEGER
    UnexpectedError,
    MissingExpression,          // 缺少表达式
    IdentifierNotFound(String), // 未找到标识符
}

impl From<EvalError> for Object {
    fn from(error: EvalError) -> Self {
        match error {
            EvalError::UnknownOperator(msg) => Object::Error(format!("unknown operator: {}", msg)),
            EvalError::TypeMismatch(msg) => Object::Error(format!("type mismatch: {}", msg)),
            EvalError::IdentifierNotFound(ident) => {
                Object::Error(format!("identifier not found: {}", ident))
            }
            EvalError::UnexpectedError => Object::Error("unexpected error".to_string()),
            EvalError::MissingExpression => Object::Error("missing expression".to_string()),
        }
    }
}

pub(crate) fn eval(node: NodeEnum, env: &mut Environment) -> Result<Object, EvalError> {
    match node {
        NodeEnum::Program(p) => eval_programe(&p, env),
        NodeEnum::StatementEnum(s) => eval_statement(&s, env),
        NodeEnum::ExpressionEnum(e) => eval_expression(&e, env),
    }
}

fn eval_statement(stmt: &StatementEnum, env: &mut Environment) -> Result<Object, EvalError> {
    match stmt {
        StatementEnum::ExpressionStatement(s) => eval_expression(
            s.expression.as_ref().ok_or(EvalError::MissingExpression)?,
            env,
        ),
        StatementEnum::ReturnStatement(s) => {
            let value = s
                .return_value
                .as_ref()
                .map(|rv| eval_expression(rv, env))
                .transpose()?
                .unwrap_or(NULL);
            Ok(Object::ReturnValue(Box::new(value)))
        }
        StatementEnum::LetStatement(s) => {
            let value = s
                .value
                .as_ref()
                .map(|v| eval_expression(v, env))
                .transpose()?
                .unwrap_or(NULL);
            env.set(s.name.value.to_owned(), value.clone());
            Ok(value)
        }
    }
}

fn eval_expression(exp: &ExpressionEnum, env: &mut Environment) -> Result<Object, EvalError> {
    match exp {
        ExpressionEnum::IntegerLiteral(il) => Ok(Object::Integer(il.value)),
        ExpressionEnum::Boolean(b) => Ok(native_bool_to_boolean_object(b.value)),
        ExpressionEnum::PrefixExpression(pe) => {
            let right =
                eval_expression(pe.right.as_ref().ok_or(EvalError::MissingExpression)?, env)?;
            eval_prefix_expression(pe.operator.as_str(), right)
        }
        ExpressionEnum::InfixExpression(ie) => {
            let left = eval_expression(ie.left.as_ref().ok_or(EvalError::MissingExpression)?, env)?;
            let right =
                eval_expression(ie.right.as_ref().ok_or(EvalError::MissingExpression)?, env)?;
            eval_infix_expression(ie.operator.as_str(), left, right)
        }
        ExpressionEnum::IfExpression(ie) => eval_if_expression(ie, env),
        ExpressionEnum::Identifier(ident) => eval_identifier(ident, env),
        _ => Ok(NULL),
    }
}

fn eval_programe(program: &Program, env: &mut Environment) -> Result<Object, EvalError> {
    let mut result = NULL;
    for stmt in &program.statements {
        result = eval_statement(stmt, env)?;
        if let Object::ReturnValue(rv) = result {
            return Ok(*rv);
        }
    }
    Ok(result)
}

fn eval_block_statement(
    block: &BlockStatement,
    env: &mut Environment,
) -> Result<Object, EvalError> {
    let mut result = NULL;
    for stmt in &block.statements {
        result = eval_statement(stmt, env)?;
        // 这里不解构 rv，因为可能会有嵌套 return 的情况
        // 即遇到第一个return 之后，后续的语句都不执行
        if matches!(result, Object::ReturnValue(_)) {
            return Ok(result);
        }
    }
    return Ok(result);
}

fn eval_identifier(ident: &Identifier, env: &mut Environment) -> Result<Object, EvalError> {
    env.get(&ident.value)
        .cloned()
        .ok_or(EvalError::IdentifierNotFound(ident.value.to_string()))
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Result<Object, EvalError> {
    match (&left, &right) {
        (Object::Integer(left_value), Object::Integer(right_value)) => {
            eval_integer_infix_expression(operator, *left_value, *right_value)
        }
        (Object::Boolean(left_value), Object::Boolean(right_value)) => match operator {
            "==" => Ok(native_bool_to_boolean_object(left_value == right_value)),
            "!=" => Ok(native_bool_to_boolean_object(left_value != right_value)),
            _ => Err(EvalError::UnknownOperator(format!(
                "BOOLEAN {} BOOLEAN",
                operator,
            ))),
        },
        _ => {
            return Err(EvalError::TypeMismatch(format!(
                "{} {} {}",
                left.get_type(),
                operator,
                right.get_type()
            )))
        }
    }
}

fn eval_integer_infix_expression(
    operator: &str,
    left_value: i64,
    right_value: i64,
) -> Result<Object, EvalError> {
    match operator {
        "+" => Ok(Object::Integer(left_value + right_value)),
        "-" => Ok(Object::Integer(left_value - right_value)),
        "*" => Ok(Object::Integer(left_value * right_value)),
        "/" => Ok(Object::Integer(left_value / right_value)),
        "<" => Ok(native_bool_to_boolean_object(left_value < right_value)),
        ">" => Ok(native_bool_to_boolean_object(left_value > right_value)),
        "==" => Ok(native_bool_to_boolean_object(left_value == right_value)),
        "!=" => Ok(native_bool_to_boolean_object(left_value != right_value)),
        _ => Err(EvalError::UnknownOperator(format!(
            "INTEGER {} INTEGER",
            operator,
        ))),
    }
}

fn eval_prefix_expression(operator: &str, right: Object) -> Result<Object, EvalError> {
    match operator {
        "!" => Ok(eval_bang_operator_expression(right)),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Err(EvalError::UnknownOperator(format!(
            "{}{}",
            operator,
            right.get_type()
        ))),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    native_bool_to_boolean_object(!right.is_truthy())
}

fn eval_minus_prefix_operator_expression(right: Object) -> Result<Object, EvalError> {
    if let Object::Integer(i) = right {
        Ok(Object::Integer(-i))
    } else {
        Err(EvalError::UnknownOperator(format!("-{}", right.get_type())))
    }
}

fn eval_if_expression(ie: &IfExpression, env: &mut Environment) -> Result<Object, EvalError> {
    let condition = eval_expression(
        ie.condition.as_ref().ok_or(EvalError::UnexpectedError)?,
        env,
    )?;

    if condition.is_truthy() {
        eval_block_statement(
            ie.consequence.as_ref().ok_or(EvalError::UnexpectedError)?,
            env,
        )
    } else if ie.alternative.is_some() {
        eval_block_statement(
            ie.alternative.as_ref().ok_or(EvalError::UnexpectedError)?,
            env,
        )
    } else {
        Ok(NULL)
    }
}

fn native_bool_to_boolean_object(input: bool) -> Object {
    if input {
        TRUE
    } else {
        FALSE
    }
}

#[cfg(test)]
mod test {
    use crate::{
        ast::NodeEnum,
        lexer::Lexer,
        object::{Environment, Object},
        parser::Parser,
    };

    use super::eval;

    fn test_eval(input: &str) -> Object {
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        let mut env = Environment::new();

        return eval(NodeEnum::Program(program), &mut env).unwrap_or_else(|e| e.into());
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(&evaluated, expected);
        }
    }

    fn test_integer_object(obj: &Object, expected: i64) {
        if let Object::Integer(value) = obj {
            assert_eq!(
                *value, expected,
                "object has wrong value. got={}, want={}",
                value, expected
            );
        } else {
            panic!("object is not Integer");
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = [
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true!= false", true),
            ("false!= true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(&evaluated, expected);
        }
    }

    fn test_boolean_object(obj: &Object, expected: bool) {
        if let Object::Boolean(value) = obj {
            assert_eq!(
                *value, expected,
                "object has wrong value. got={}, want={}",
                value, expected
            );
        } else {
            panic!("object is not Boolean");
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(&evaluated, expected);
        }
    }

    #[derive(Debug)]
    enum Value {
        Integer(i64),
        // Boolean(bool),
        Null,
    }

    impl From<i64> for Value {
        fn from(i: i64) -> Self {
            Value::Integer(i)
        }
    }

    #[test]
    fn test_if_expression() {
        let tests = [
            ("if (true) { 10 }", 10.into()),
            ("if (false) { 10 }", Value::Null),
            ("if (1) { 10 }", 10.into()),
            ("if (1 < 2) { 10 }", 10.into()),
            ("if (1 > 2) { 10 }", Value::Null),
            ("if (1 > 2) { 10 } else { 20 }", 20.into()),
            ("if (1 < 2) { 10 } else { 20 }", 10.into()),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            if let Value::Integer(i) = expected {
                test_integer_object(&evaluated, i);
            } else {
                test_null_object(&evaluated);
            }
        }
    }

    fn test_null_object(obj: &Object) {
        assert!(matches!(obj, Object::Null), "object is not Null")
    }

    #[test]
    fn test_return_statements() {
        let tests = [
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            ("if (10 > 1) { if (10 > 1) { return 10; } return 1; }", 10),
            (
                "if (10 > 1) { if (10 > 1) { return 10; } return 1; } else { return 20; }",
                10,
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(&evaluated, expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = [
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            if let Object::Error(msg) = evaluated {
                assert_eq!(
                    msg, expected,
                    "wrong error message. got={}, want={}",
                    msg, expected
                );
            } else {
                panic!("object is not Error");
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = [
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(&evaluated, expected);
        }
    }
}
