use crate::{
    ast::{IntegerLiteral, Node},
    object::{Integer, Object},
};
use lazy_static::lazy_static;
use std::any::TypeId;

fn eval(node: impl Node) -> impl Object {
    let any = node.as_any();
    if let Some(il) = any.downcast_ref::<IntegerLiteral>() {
        return Integer { value: il.value };
    }

    panic!("unimplemented");
}

#[cfg(test)]
mod test {
    use crate::{lexer::Lexer, object::Object, parser::Parser};

    use super::eval;

    #[test]
    fn test_eval_integer_expression() {
        let tests = [("5", 5), ("10", 10)];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    fn test_eval(input: &str) -> impl Object {
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();

        return eval(program);
    }

    fn test_integer_object(obj: impl Object, expected: i64) {
        // if let Some(result) = obj.
    }
}
