use crate::{impl_as_any, AsAny};

pub(crate) trait Object: AsAny {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

pub(crate) enum ObjectType {
    INTEGER,
    BOOLEAN,
    NULL,
}

pub(crate) struct Integer {
    pub value: i64,
}

impl Object for Integer {
    fn object_type(&self) -> ObjectType {
        ObjectType::INTEGER
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

struct Boolean {
    value: bool,
}

impl Object for Boolean {
    fn object_type(&self) -> ObjectType {
        ObjectType::BOOLEAN
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

struct Null;

impl Object for Null {
    fn object_type(&self) -> ObjectType {
        ObjectType::NULL
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }
}

impl_as_any!(Integer, Boolean, Null);
