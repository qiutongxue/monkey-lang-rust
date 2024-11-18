use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::ast::{BlockStatement, Identifier};

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
pub const NULL: Object = Object::Null;

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    Function(Function),
    String(String),
    Builtin(fn(Vec<Object>) -> Object),
    Array(Vec<Object>),
    Dict(HashMap<Object, Object>),
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Integer(a), Object::Integer(b)) => a == b,
            (Object::Boolean(a), Object::Boolean(b)) => a == b,
            (Object::String(a), Object::String(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Object {}

impl std::hash::Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Object::Integer(v) => v.hash(state),
            Object::Boolean(v) => v.hash(state),
            Object::Array(vec) => vec.hash(state),
            Object::String(s) => s.hash(state),
            _ => unreachable!(), // Object::ReturnValue(v) => v.hash(state),
                                 // Object::Builtin(_) => "builtin function".hash(state),
                                 // Object::Error(v) => v.hash(state),
                                 // Object::Null => "null".hash(state),
                                 // Object::Function(_) => std::mem::discriminant(self).hash(state),
        }
    }
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(v) => v.to_string(),
            Object::String(s) => format!("\"{}\"", s.to_owned()),
            Object::Boolean(v) => v.to_string(),
            Object::Null => "null".to_string(),
            Object::ReturnValue(v) => v.inspect(),
            Object::Error(v) => format!("ERROR: {v}"),
            Object::Function(func) => {
                let mut params = Vec::with_capacity(func.parameters.len());
                for param in &func.parameters {
                    params.push(param.value.clone());
                }

                format!("fn({}) {{\n{} \n}}", params.join(", "), func.body)
            }
            Object::Builtin(_) => "builtin function".to_string(),
            Object::Array(vec) => {
                let mut elements = Vec::with_capacity(vec.len());
                for element in vec {
                    elements.push(element.inspect());
                }

                format!("[{}]", elements.join(", "))
            }
            Object::Dict(dict) => {
                let mut pairs = Vec::with_capacity(dict.len());
                for (key, value) in dict {
                    pairs.push(format!("{}: {}", key.inspect(), value.inspect()));
                }

                format!("{{ {} }}", pairs.join(", "))
            }
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Integer(v) => *v != 0,
            Object::Boolean(v) => *v,
            Object::Null => false,
            Object::ReturnValue(v) => v.is_truthy(),
            Object::Error(_) => unreachable!(),
            Object::Function(_) => true,
            Object::String(s) => !s.is_empty(),
            Object::Builtin(_) => true,
            Object::Array(_) => true,
            Object::Dict(_) => true,
        }
    }

    pub fn get_type(&self) -> &'static str {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::Null => "NULL",
            Object::ReturnValue(_) => "RETURN_VALUE",
            Object::Error(_) => "ERROR",
            Object::Function(_) => "FUNCTION",
            Object::String(_) => "STRING",
            Object::Builtin(_) => "BUILTIN",
            Object::Array(_) => "ARRAY",
            Object::Dict(_) => "DICTIONARY",
        }
    }

    pub fn unwrap_return_value(self) -> Object {
        match self {
            Object::ReturnValue(v) => *v,
            obj => obj,
        }
    }
}

impl From<i64> for Object {
    fn from(value: i64) -> Self {
        Object::Integer(value)
    }
}

impl From<bool> for Object {
    fn from(value: bool) -> Self {
        if value {
            TRUE
        } else {
            FALSE
        }
    }
}

impl From<String> for Object {
    fn from(value: String) -> Self {
        Object::String(value)
    }
}

impl From<&str> for Object {
    fn from(value: &str) -> Self {
        Object::String(value.to_string())
    }
}

pub type RcEnvironment = Rc<RefCell<Environment>>;

#[derive(Debug)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<RcEnvironment>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: RcEnvironment) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        if let Some(result) = self.store.get(name) {
            return Some(result.clone());
        }
        if let Some(outer) = &self.outer {
            return outer.borrow().get(name);
        }
        None
    }

    pub fn to_rc(self) -> RcEnvironment {
        Rc::new(RefCell::new(self))
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: RcEnvironment,
}

#[cfg(test)]
mod tests {
    use std::hash::{DefaultHasher, Hash, Hasher};

    use super::Object;

    #[test]
    fn test_string_hash() {
        let hello1 = Object::String("hello".to_string());
        let hello2 = Object::String("hello".to_string());
        let diff1 = Object::String("My name is johnny".to_string());
        let diff2 = Object::String("My name is johnny".to_string());

        assert!(compare_hash(&hello1, &hello2));
        assert!(!compare_hash(&hello1, &diff1));
        assert!(compare_hash(&diff1, &diff2));
    }

    fn compare_hash<T: Hash>(a: &T, b: &T) -> bool {
        let mut hasher1 = DefaultHasher::new();
        let mut hasher2 = DefaultHasher::new();
        a.hash(&mut hasher1);
        b.hash(&mut hasher2);

        hasher1.finish() == hasher2.finish()
    }
}
