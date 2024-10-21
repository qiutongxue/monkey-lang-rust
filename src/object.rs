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
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(v) => v.to_string(),
            Object::String(s) => s.to_owned(),
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
        }
    }

    pub fn unwrap_return_value(self) -> Object {
        match self {
            Object::ReturnValue(v) => *v,
            obj => obj,
        }
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
