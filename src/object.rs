pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}

impl Object {
    pub fn object_type(&self) -> ObjectType {
        match self {
            Object::Integer(_) => ObjectType::INTEGER,
            Object::Boolean(_) => ObjectType::BOOLEAN,
            Object::Null => ObjectType::NULL,
        }
    }
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(v) => v.to_string(),
            Object::Boolean(v) => v.to_string(),
            Object::Null => "null".to_string(),
        }
    }
}

pub(crate) enum ObjectType {
    INTEGER,
    BOOLEAN,
    NULL,
}
