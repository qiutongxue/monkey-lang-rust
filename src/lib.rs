pub mod ast;
pub mod evaluator;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod repl;
pub mod token;

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
