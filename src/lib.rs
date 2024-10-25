pub mod ast;
pub mod code;
pub mod compiler;
pub mod evaluator;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod repl;
pub mod token;
pub mod vm;

pub trait AsAny {
    fn as_any(&self) -> &dyn std::any::Any;
}

#[macro_export]
macro_rules! impl_as_any {
    ($t:ty) => {
        impl AsAny for $t {
            fn as_any(&self) -> &dyn std::any::Any {
                self
            }
        }
    };
    ($($t:ty),* $(,)?) => {
        $(
            impl_as_any!($t);
        )*
    };
}

#[macro_export]
macro_rules! impl_node_for_enum {
    ($enum_name:ident {$($variant:ident),* }) => {
        impl std::fmt::Display for $enum_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        $enum_name::$variant(v) => write!(f, "{}", v),
                    )*
                }
            }
        }

        impl Node for $enum_name {
            fn token_literal(&self) -> String {
                match self {
                    $(
                        $enum_name::$variant(v) => v.token_literal(),
                    )*
                }
            }
        }

    };
}

#[cfg(test)]
mod test_utils {

    #[derive(Debug)]
    pub enum Value {
        Integer(i64),
        Boolean(bool),
        Null,
        String(String),
        Array(Vec<Value>),
    }

    impl From<i64> for Value {
        fn from(i: i64) -> Self {
            Value::Integer(i)
        }
    }

    impl From<&str> for Value {
        fn from(s: &str) -> Self {
            Value::String(s.to_owned())
        }
    }

    impl From<Vec<i64>> for Value {
        fn from(arr: Vec<i64>) -> Self {
            Self::Array(arr.into_iter().map(|i| i.into()).collect())
        }
    }

    pub fn parse_program(input: &str) -> crate::ast::Program {
        let l = crate::lexer::Lexer::new(input.to_string());
        let mut p = crate::parser::Parser::new(l);
        p.parse_program().expect("parse program error")
    }

    pub mod object {
        use crate::object::Object;

        use super::Value;

        pub fn test_boolean_object(obj: &Object, expected: bool) {
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

        pub fn test_integer_object(obj: &Object, expected: i64) {
            if let Object::Integer(value) = obj {
                assert_eq!(
                    *value, expected,
                    "object has wrong value. got={}, want={}",
                    value, expected
                );
            } else {
                panic!("object is not Integer, got={obj:?}");
            }
        }

        fn test_array_object(obj: &Object, expected: &[Value]) {
            if let Object::Array(arr) = obj {
                assert_eq!(
                    arr.len(),
                    expected.len(),
                    "array has wrong length. got={}, want={}",
                    arr.len(),
                    expected.len()
                );
                for (i, obj) in arr.iter().enumerate() {
                    test_object(obj, &expected[i]);
                }
            } else {
                panic!("object is not Array. got={}", obj.get_type());
            }
        }

        pub fn test_null_object(obj: &Object) {
            assert!(matches!(obj, Object::Null), "object is not Null")
        }

        pub fn test_object(obj: &Object, expected: &Value) {
            match expected {
                Value::Integer(i) => test_integer_object(obj, *i),
                Value::String(s) => match obj {
                    Object::String(obj_s) => {
                        assert_eq!(
                            obj_s, s,
                            "string has wrong value. got={}, want={}",
                            obj_s, s
                        )
                    }
                    _ => panic!("object is not String, got={:?}", obj),
                },
                Value::Null => test_null_object(obj),
                Value::Array(arr) => test_array_object(obj, arr),
                Value::Boolean(b) => test_boolean_object(obj, *b),
            }
        }
    }
}
