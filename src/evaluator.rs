use crate::{
    ast::{Expression, ExpressionStatement, IntegerLiteral, Node, Program, Statement},
    object::{Boolean, Integer, Null, Object},
};

fn eval(node: Box<dyn Node>) -> Box<dyn Object> {
    let any = node.as_any();
    // statements
    if let Some(p) = any.downcast_ref::<Program>() {
        return eval_statements(&p.statements);
    }
    if let Some(es) = any.downcast_ref::<ExpressionStatement>() {
        return eval_expression(es.expression.as_ref().unwrap());
    }

    // expressions
    if let Some(il) = any.downcast_ref::<IntegerLiteral>() {
        return Box::new(Integer { value: il.value }) as Box<dyn Object>;
    }
    return Box::new(Boolean { value: true }) as Box<dyn Object>;

    // panic!("unimplemented");
}

fn eval_statement(stmt: &Box<dyn Statement>) -> Box<dyn Object> {
    let any = stmt.as_any();
    // statements
    if let Some(p) = any.downcast_ref::<Program>() {
        return eval_statements(&p.statements);
    }
    if let Some(es) = any.downcast_ref::<ExpressionStatement>() {
        return eval_expression(es.expression.as_ref().unwrap());
    }
    todo!()
}

fn eval_expression(exp: &Box<dyn Expression>) -> Box<dyn Object> {
    let any = exp.as_any();
    if let Some(il) = any.downcast_ref::<IntegerLiteral>() {
        return Box::new(Integer { value: il.value }) as Box<dyn Object>;
    }
    return Box::new(Boolean { value: true }) as Box<dyn Object>;
}

fn eval_statements(stmts: &[Box<dyn Statement>]) -> Box<dyn Object> {
    let mut result = Box::new(Null) as Box<dyn Object>;
    for stmt in stmts {
        result = eval_statement(stmt);
    }
    result
}

#[cfg(test)]
mod test {
    use crate::{
        lexer::Lexer,
        object::{Integer, Object},
        parser::Parser,
    };

    use super::eval;

    #[test]
    fn test_eval_integer_expression() {
        let tests = [("5", 5), ("10", 10)];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(evaluated.as_ref(), expected);
        }
    }

    fn test_eval(input: &str) -> Box<dyn Object> {
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();

        println!("{program:?}");

        return eval(Box::new(program));
    }

    fn test_integer_object(obj: &dyn Object, expected: i64) {
        let result = obj.as_any().downcast_ref::<Integer>();
        assert!(result.is_some(), "object is not Integer");
        assert_eq!(
            result.as_ref().unwrap().value,
            expected,
            "object has wrong value. got={}, want={}",
            result.as_ref().unwrap().value,
            expected
        );
    }
}
