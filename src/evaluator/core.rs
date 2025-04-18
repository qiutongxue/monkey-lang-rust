use std::collections::HashMap;

use crate::{
    ast::{BlockStatement, ExpressionEnum, Identifier, IfExpression, Program, StatementEnum},
    object::{Environment, Function, Object, RcEnvironment, NULL},
};

use super::builtin::BUILTINS;

#[derive(Debug)]
pub enum EvalError {
    UnknownOperator(String), // -BOOLEAN, BOOLEAN + BOOLEAN
    TypeMismatch(String),    // BOOLEAN + INTEGER
    UnexpectedError,
    IdentifierNotFound(String), // 未找到标识符
    NotAFunction(String),       // 不是函数
    IndexError(String),         // 数组索引越界
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
            EvalError::NotAFunction(msg) => Object::Error(format!("not a function: {}", msg)),
            EvalError::IndexError(msg) => Object::Error(format!("index error: {}", msg)),
        }
    }
}

pub fn eval(program: &Program, env: RcEnvironment) -> Object {
    eval_programe(program, env).unwrap_or_else(|error| error.into())
}

fn eval_statement(stmt: &StatementEnum, env: RcEnvironment) -> Result<Object, EvalError> {
    match stmt {
        StatementEnum::ExpressionStatement(s) => eval_expression(&s.expression, env),
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
                .map(|v| eval_expression(v, env.clone()))
                .transpose()?
                .unwrap_or(NULL);
            env.borrow_mut().set(s.name.value.to_owned(), value);
            Ok(NULL)
        }
    }
}

fn eval_expression(exp: &ExpressionEnum, env: RcEnvironment) -> Result<Object, EvalError> {
    match exp {
        ExpressionEnum::IntegerLiteral(il) => Ok(Object::from(il.value)),
        ExpressionEnum::Boolean(b) => Ok(Object::from(b.value)),
        ExpressionEnum::PrefixExpression(pe) => {
            let right = eval_expression(pe.right.as_ref(), env)?;
            eval_prefix_expression(pe.operator.as_str(), right)
        }
        ExpressionEnum::InfixExpression(ie) => {
            let left = eval_expression(ie.left.as_ref(), env.clone())?;
            let right = eval_expression(ie.right.as_ref(), env)?;
            eval_infix_expression(ie.operator.as_str(), left, right)
        }
        ExpressionEnum::IfExpression(ie) => eval_if_expression(ie, env),
        ExpressionEnum::Identifier(ident) => eval_identifier(ident, env),
        ExpressionEnum::FunctionLiteral(func) => {
            let parameters = func.parameters.clone();
            let body = func.body.clone();
            Ok(Object::Function(Function {
                parameters,
                body,
                env,
            }))
        }
        ExpressionEnum::CallExpression(ce) => {
            let function = eval_expression(&ce.function, env.clone())?;
            let args = eval_expressions(&ce.arguments, env)?;
            apply_function(&function, args)
        }
        ExpressionEnum::BlockStatement(bs) => eval_block_statement(bs, env),
        ExpressionEnum::StringLiteral(sl) => Ok(Object::from(sl.value.as_str())),
        ExpressionEnum::ArrayLiteral(al) => {
            let elements = eval_expressions(&al.elements, env)?;
            Ok(Object::Array(elements))
        }
        ExpressionEnum::IndexExpression(ie) => {
            let left = eval_expression(ie.left.as_ref(), env.clone())?;
            let index = eval_expression(ie.index.as_ref(), env)?;
            match (&left, &index) {
                (Object::Array(arr), Object::Integer(idx)) => {
                    if *idx < 0 || *idx as usize >= arr.len() {
                        Ok(NULL)
                    } else {
                        Ok(arr[*idx as usize].clone())
                    }
                }
                (Object::Dict(dict), key) => {
                    ensure_key_type_is_valid(key)?;
                    if let Some(value) = dict.get(key) {
                        Ok(value.clone())
                    } else {
                        Ok(NULL)
                    }
                }
                _ => Err(EvalError::TypeMismatch(format!(
                    "index operator not supported, left={}, index={}",
                    left.get_type(),
                    index.get_type()
                ))),
            }
        }
        ExpressionEnum::DictLiteral(dict_literal) => {
            let mut dict = HashMap::with_capacity(dict_literal.pairs.len());
            for (key, value) in dict_literal.pairs.iter() {
                let key = eval_expression(key, env.clone())?;
                ensure_key_type_is_valid(&key)?;
                let value = eval_expression(value, env.clone())?;
                dict.insert(key, value);
            }
            Ok(Object::Dict(dict))
        }
    }
}

fn ensure_key_type_is_valid(key: &Object) -> Result<(), EvalError> {
    match key {
        Object::String(_) | Object::Integer(_) | Object::Boolean(_) => Ok(()),
        _ => Err(EvalError::TypeMismatch(format!(
            "dict key must be string, integer or boolean, got {}",
            key.get_type()
        ))),
    }
}
fn eval_expressions(exps: &[ExpressionEnum], env: RcEnvironment) -> Result<Vec<Object>, EvalError> {
    let mut result = Vec::with_capacity(exps.len());
    for exp in exps {
        let evaluated = eval_expression(exp, env.clone())?;
        result.push(evaluated);
    }
    Ok(result)
}

fn apply_function(func: &Object, args: Vec<Object>) -> Result<Object, EvalError> {
    match func {
        Object::Function(f) => {
            let new_env = extend_function_env(f, args);
            let evaluated = eval_block_statement(&f.body, new_env)?.unwrap_return_value();
            Ok(evaluated)
        }
        Object::Builtin(b) => {
            let result = b(args);
            Ok(result)
        }
        _ => Err(EvalError::NotAFunction(func.get_type().to_string())),
    }
}

/// 为函数创建新环境
fn extend_function_env(f: &Function, args: Vec<Object>) -> RcEnvironment {
    let mut new_env = Environment::new_enclosed(f.env.clone());
    for (param, arg) in f.parameters.iter().zip(args) {
        new_env.set(param.value.to_string(), arg);
    }
    new_env.to_rc()
}

fn eval_programe(program: &Program, env: RcEnvironment) -> Result<Object, EvalError> {
    let mut result = NULL;
    for stmt in &program.statements {
        result = eval_statement(stmt, env.clone())?;
        if let Object::ReturnValue(rv) = result {
            return Ok(*rv);
        }
    }
    Ok(result)
}

fn eval_block_statement(block: &BlockStatement, env: RcEnvironment) -> Result<Object, EvalError> {
    let mut result = NULL;
    for stmt in &block.statements {
        result = eval_statement(stmt, env.clone())?;
        // 这里不解构 rv，因为可能会有嵌套 return 的情况
        // 即遇到第一个return 之后，后续的语句都不执行
        if matches!(result, Object::ReturnValue(_)) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_identifier(ident: &Identifier, env: RcEnvironment) -> Result<Object, EvalError> {
    if let Some(obj) = env.borrow().get(&ident.value) {
        Ok(obj)
    } else if let Some(builtin) = BUILTINS.get(ident.value.as_str()) {
        Ok(Object::Builtin(*builtin))
    } else {
        Err(EvalError::IdentifierNotFound(ident.value.to_string()))
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Result<Object, EvalError> {
    match (&left, &right) {
        (Object::Integer(left_value), Object::Integer(right_value)) => {
            eval_integer_infix_expression(operator, *left_value, *right_value)
        }
        (Object::Boolean(left_value), Object::Boolean(right_value)) => match operator {
            "==" => Ok(Object::from(left_value == right_value)),
            "!=" => Ok(Object::from(left_value != right_value)),
            _ => Err(EvalError::UnknownOperator(format!(
                "BOOLEAN {} BOOLEAN",
                operator,
            ))),
        },
        (Object::String(left_value), Object::String(right_value)) => match operator {
            "+" => Ok(Object::from(left_value.to_owned() + right_value)),
            _ => Err(EvalError::UnknownOperator(format!(
                "STRING {} STRING",
                operator,
            ))),
        },
        _ => Err(EvalError::TypeMismatch(format!(
            "{} {} {}",
            left.get_type(),
            operator,
            right.get_type()
        ))),
    }
}

fn eval_integer_infix_expression(
    operator: &str,
    left_value: i64,
    right_value: i64,
) -> Result<Object, EvalError> {
    match operator {
        "+" => Ok(Object::from(left_value + right_value)),
        "-" => Ok(Object::from(left_value - right_value)),
        "*" => Ok(Object::from(left_value * right_value)),
        "/" => Ok(Object::from(left_value / right_value)),
        "<" => Ok(Object::from(left_value < right_value)),
        ">" => Ok(Object::from(left_value > right_value)),
        "==" => Ok(Object::from(left_value == right_value)),
        "!=" => Ok(Object::from(left_value != right_value)),
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
    Object::from(!right.is_truthy())
}

fn eval_minus_prefix_operator_expression(right: Object) -> Result<Object, EvalError> {
    if let Object::Integer(i) = right {
        Ok(Object::from(-i))
    } else {
        Err(EvalError::UnknownOperator(format!("-{}", right.get_type())))
    }
}

fn eval_if_expression(ie: &IfExpression, env: RcEnvironment) -> Result<Object, EvalError> {
    let condition = eval_expression(ie.condition.as_ref(), env.clone())?;

    if condition.is_truthy() {
        eval_block_statement(&ie.consequence, env.clone())
    } else if ie.alternative.is_some() {
        eval_block_statement(
            ie.alternative.as_ref().ok_or(EvalError::UnexpectedError)?,
            env,
        )
    } else {
        Ok(NULL)
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::{
        lexer::Lexer,
        object::{Environment, Object, FALSE, TRUE},
        parser::Parser,
        test_utils::{
            object::{test_boolean_object, test_integer_object, test_null_object, test_object},
            Value,
        },
    };

    use super::eval;

    fn test_eval(input: &str) -> Object {
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        let env = Environment::new().to_rc();

        return eval(&program, env);
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
            (r#""Hello" - "World""#, "unknown operator: STRING - STRING"),
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

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; }";
        let evaluated = test_eval(input);
        if let Object::Function(func) = evaluated {
            assert_eq!(
                func.parameters.len(),
                1,
                "function has wrong parameters. Parameters: {}",
                func.parameters.len()
            );

            assert_eq!(
                func.parameters[0].to_string(),
                "x",
                "function has wrong parameter names. Parameters: {:?}",
                func.parameters
            );

            let expected_body = "(x + 2)";

            assert_eq!(
                func.body.to_string(),
                expected_body,
                "function has wrong body. Body: {}",
                func.body.to_string()
            );
        } else {
            panic!("object is not Function");
        }
    }

    #[test]
    fn test_function_application() {
        let tests = [
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(&evaluated, expected);
        }
    }

    #[test]
    fn test_closures() {
        let input = r#"
            let x = 10;
            let y = 20;
            let newAdder = fn(x) { 
                fn(y) { x + y } 
            }; 
            let addTwo = newAdder(2); 
            addTwo(3);
            "#;
        let evaluated = test_eval(input);
        test_integer_object(&evaluated, 5);
    }

    #[test]
    fn test_string_literal() {
        let input = r#""hello world""#;
        let evaluated = test_eval(input);
        if let Object::String(s) = evaluated {
            assert_eq!(
                s, "hello world",
                "string has wrong value. got={}, want={}",
                s, "hello world"
            );
        } else {
            panic!("object is not String");
        }
    }

    #[test]
    fn test_string_concatenation() {
        let tests = [
            (r#""hello" + " " + "world""#, "hello world"),
            (r#""hello" + " " + "world" + "!""#, "hello world!"),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            if let Object::String(s) = evaluated {
                assert_eq!(
                    s, expected,
                    "string has wrong value. got={}, want={}",
                    s, expected
                );
            } else {
                panic!("object is not String, got={:?}", evaluated);
            }
        }
    }

    #[test]
    fn test_builtin_functions() {
        let tests = [
            ("len(\"\")", Value::Integer(0)),
            ("len(\"four\")", Value::Integer(4)),
            ("len(\"hello world\")", Value::Integer(11)),
            (
                "len(123)",
                "argument to `len` not supported, got INTEGER".into(),
            ),
            (
                "len(\"one\", \"two\")",
                "wrong number of arguments. got=2, want=1".into(),
            ),
            ("len([1, 2, 3])", 3.into()),
            ("len([])", 0.into()),
            ("first([1, 2, 3])", 1.into()),
            ("first([])", Value::Null),
            (
                "first(123)",
                "argument to `first` must be ARRAY, got INTEGER".into(),
            ),
            ("last([1, 2, 3])", 3.into()),
            ("last([])", Value::Null),
            (
                "last(123)",
                "argument to `last` must be ARRAY, got INTEGER".into(),
            ),
            ("rest([1, 2, 3])", vec![2, 3].into()),
            ("rest([])", Value::Null),
            ("push([1, 2], 3)", vec![1, 2, 3].into()),
            ("push([], 1)", vec![1].into()),
            // ("puts(123)", "123"),
            // ("puts(true)", "true"),
            // ("puts(null)", "null"),
            // ("puts([1, 2, 3])", "[1, 2, 3]"),
            // ("puts([])", "[]"),
            // ("puts(\"hello\")", "hello"),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match expected {
                Value::String(s) => match evaluated {
                    Object::Error(msg) => {
                        assert_eq!(msg, s, "wrong error message. got={}, want={}", msg, s)
                    }
                    _ => panic!("object is not Error, got={:?}", evaluated),
                },
                value => test_object(&evaluated, &value),
            }
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let evaluated = test_eval(input);
        if let Object::Array(arr) = evaluated {
            assert_eq!(
                arr.len(),
                3,
                "array has wrong length. got={}, want={}",
                arr.len(),
                3
            );
            test_integer_object(&arr[0], 1);
            test_integer_object(&arr[1], 4);
            test_integer_object(&arr[2], 6);
        } else {
            panic!("object is not Array");
        }
    }

    #[test]
    fn test_array_index_expressions() {
        let tests = [
            ("[1, 2, 3][0]", 1.into()),
            ("[1, 2, 3][1]", 2.into()),
            ("[1, 2, 3][2]", 3.into()),
            ("let i = 0; [1][i];", 1.into()),
            ("[1, 2, 3][1 + 1];", 3.into()),
            ("let myArray = [1, 2, 3]; myArray[2];", 3.into()),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                6.into(),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                2.into(),
            ),
            ("[1, 2, 3][3]", Value::Null),
            ("[1, 2, 3][-1]", Value::Null),
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

    #[test]
    fn test_dict_literals() {
        let input = r#"
        let two = "two";
        {
            "one": 10 - 9,
            two: 1 + 1,
            "thr" + "ee": 6 / 2,
            4: 4,
            true: 5,
            false: 6
        }
        "#;
        let evaluated = test_eval(input);
        if let Object::Dict(dict) = evaluated {
            let expected = HashMap::from([
                (Object::String("one".to_string()), 1),
                (Object::String("two".to_string()), 2),
                (Object::String("three".to_string()), 3),
                (Object::Integer(4), 4),
                (TRUE, 5),
                (FALSE, 6),
            ]);
            assert_eq!(dict.len(), expected.len());
            for (key, value) in expected {
                let obj = dict.get(&key);
                if let Some(obj) = obj {
                    test_integer_object(obj, value);
                } else {
                    panic!("key not found in dictionary, key={:?}", key);
                }
            }
        } else {
            panic!("object is not Dictionary, got={:?}", evaluated);
        }
    }

    #[test]
    fn test_dict_index_expressions() {
        let tests = [
            (r#"{ "one": 1, "two": 2, "three": 3 }["one"]"#, 1.into()),
            (r#"{ "one": 1, "two": 2, "three": 3 }["two"]"#, 2.into()),
            (r#"{ "one": 1, "two": 2, "three": 3 }["three"]"#, 3.into()),
            (r#"{ 1: 1, 2: 2, 3: 3 }[1]"#, 1.into()),
            (r#"{ 1: 1, 2: 2, 3: 3 }[2]"#, 2.into()),
            (r#"{ 1: 1, 2: 2, 3: 3 }[3]"#, 3.into()),
            (r#"{ true: 1, false: 2 }[true]"#, 1.into()),
            (r#"{ true: 1, false: 2 }[false]"#, 2.into()),
            (r#"{ "one": 1, "two": 2, "three": 3 }["four"]"#, Value::Null),
            (r#"{ "one": 1, "two": 2, "three": 3 }[0]"#, Value::Null),
            (r#"{ "one": 1, "two": 2, "three": 3 }[-1]"#, Value::Null),
            (r#"let key = "foo"; { key: 5 }[key]"#, 5.into()),
            (r#"{}["foo"]"#, Value::Null),
            (r#"let d = { "foo": 5 }; d["foo"]"#, 5.into()),
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
}
