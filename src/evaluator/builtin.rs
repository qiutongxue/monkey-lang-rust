use std::{collections::HashMap, sync::LazyLock};

use crate::object::Object;

type BuiltinMap = HashMap<&'static str, fn(Vec<Object>) -> Object>;

pub static BUILTINS: LazyLock<BuiltinMap> = LazyLock::new(|| {
    let mut m: BuiltinMap = HashMap::new();
    m.insert("len", len);
    m
});

fn len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }
    match &args[0] {
        Object::String(s) => Object::Integer(s.len() as i64),
        obj => Object::Error(format!(
            "argument to `len` not supported, got {}",
            obj.get_type()
        )),
    }
}
