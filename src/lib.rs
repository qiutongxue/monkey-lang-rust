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
