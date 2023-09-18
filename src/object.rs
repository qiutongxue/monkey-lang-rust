use crate::{impl_as_any, AsAny};

impl_as_any!(Integer, Boolean, Null);

pub(crate) trait Object: AsAny {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

pub(crate) enum ObjectType {
    INTEGER,
    BOOLEAN,
    NULL,
}

#[derive(Debug)]
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

#[derive(Debug)]

pub(crate) struct Boolean {
    pub value: bool,
}

impl Object for Boolean {
    fn object_type(&self) -> ObjectType {
        ObjectType::BOOLEAN
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug)]

pub(crate) struct Null;

impl Object for Null {
    fn object_type(&self) -> ObjectType {
        ObjectType::NULL
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }
}
