use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(v) => v.to_string(),
            Object::Boolean(v) => v.to_string(),
            Object::Null => "null".to_string(),
            Object::ReturnValue(v) => v.inspect(),
            Object::Error(v) => format!("ERROR: {v}"),
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Integer(v) => *v != 0,
            Object::Boolean(v) => *v,
            Object::Null => false,
            Object::ReturnValue(v) => v.is_truthy(),
            Object::Error(_) => unreachable!(),
        }
    }

    pub fn get_type(&self) -> &'static str {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::Null => "NULL",
            Object::ReturnValue(_) => "RETURN_VALUE",
            Object::Error(_) => "ERROR",
        }
    }
}

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        self.store.get(name)
    }
}
