use crate::{
    ast::{ExpressionEnum, NodeEnum, StatementEnum},
    object::Object,
};

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

pub(crate) fn eval(node: NodeEnum) -> Option<Object> {
    match node {
        NodeEnum::Program(p) => eval_statements(&p.statements),
        NodeEnum::StatementEnum(s) => eval_statement(&s),
        NodeEnum::ExpressionEnum(e) => eval_expression(&e),
    }
}

fn eval_statement(stmt: &StatementEnum) -> Option<Object> {
    match stmt {
        StatementEnum::ExpressionStatement(s) => eval_expression(s.expression.as_ref()?),
        _ => None,
    }
}

fn eval_expression(exp: &ExpressionEnum) -> Option<Object> {
    match exp {
        ExpressionEnum::IntegerLiteral(il) => Some(Object::Integer(il.value)),
        ExpressionEnum::Boolean(b) => Some(native_bool_to_boolean_object(b.value)),
        ExpressionEnum::PrefixExpression(pe) => {
            let right = eval_expression(pe.right.as_ref()?)?;
            eval_prefix_expression(pe.operator.as_str(), right)
        }
        ExpressionEnum::InfixExpression(ie) => {
            let left = eval_expression(ie.left.as_ref()?)?;
            let right = eval_expression(ie.right.as_ref()?)?;
            eval_infix_expression(ie.operator.as_str(), left, right)
        }
        _ => None,
    }
}

fn eval_statements(stmts: &[StatementEnum]) -> Option<Object> {
    let mut result = None;
    for stmt in stmts {
        result = eval_statement(stmt);
    }
    result
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Option<Object> {
    match (left, right) {
        (Object::Integer(left_value), Object::Integer(right_value)) => {
            eval_integer_infix_expression(operator, left_value, right_value)
        }
        _ => return None,
    }
}

fn eval_integer_infix_expression(
    operator: &str,
    left_value: i64,
    right_value: i64,
) -> Option<Object> {
    match operator {
        "+" => Some(Object::Integer(left_value + right_value)),
        "-" => Some(Object::Integer(left_value - right_value)),
        "*" => Some(Object::Integer(left_value * right_value)),
        "/" => Some(Object::Integer(left_value / right_value)),
        "<" => Some(native_bool_to_boolean_object(left_value < right_value)),
        ">" => Some(native_bool_to_boolean_object(left_value > right_value)),
        "==" => Some(native_bool_to_boolean_object(left_value == right_value)),
        "!=" => Some(native_bool_to_boolean_object(left_value != right_value)),
        _ => None,
    }
}

fn eval_prefix_expression(operator: &str, right: Object) -> Option<Object> {
    match operator {
        "!" => Some(eval_bang_operator_expression(right)),
        "-" => Some(eval_minus_prefix_operator_expression(right)),
        _ => Some(NULL),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(b) => native_bool_to_boolean_object(!b),
        Object::Integer(i) => native_bool_to_boolean_object(i == 0),
        Object::Null => TRUE,
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    if let Object::Integer(i) = right {
        Object::Integer(-i)
    } else {
        NULL
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
    use crate::{ast::NodeEnum, lexer::Lexer, object::Object, parser::Parser};

    use super::eval;

    fn test_eval(input: &str) -> Object {
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();

        // println!("{program:?}");

        return eval(NodeEnum::Program(program)).unwrap();
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
}
