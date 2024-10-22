use std::{collections::HashMap, sync::LazyLock};

use crate::object::{Object, NULL};

type BuiltinFn = fn(Vec<Object>) -> Object;
type BuiltinMap = HashMap<&'static str, BuiltinFn>;

pub static BUILTINS: LazyLock<BuiltinMap> = LazyLock::new(|| {
    let mut m: BuiltinMap = HashMap::new();
    m.insert("len", len);
    m.insert("first", first);
    m.insert("last", last);
    m.insert("rest", rest);
    m.insert("push", push);
    m
});

fn len(args: Vec<Object>) -> Object {
    if let Err(e) = ensure_arguments_length(&args, 1) {
        return e;
    }
    match &args[0] {
        Object::String(s) => Object::Integer(s.len() as i64),
        Object::Array(arr) => Object::Integer(arr.len() as i64),
        obj => Object::Error(format!(
            "argument to `len` not supported, got {}",
            obj.get_type()
        )),
    }
}

fn ensure_arguments_length(args: &[Object], want: usize) -> Result<(), Object> {
    if args.len() != want {
        return Err(Object::Error(format!(
            "wrong number of arguments. got={}, want={}",
            args.len(),
            want
        )));
    }
    Ok(())
}

fn first(args: Vec<Object>) -> Object {
    if let Err(e) = ensure_arguments_length(&args, 1) {
        return e;
    }
    match &args[0] {
        Object::Array(arr) => match arr.first() {
            Some(obj) => obj.clone(),
            None => NULL,
        },
        obj => Object::Error(format!(
            "argument to `first` must be ARRAY, got {}",
            obj.get_type()
        )),
    }
}

fn last(args: Vec<Object>) -> Object {
    if let Err(e) = ensure_arguments_length(&args, 1) {
        return e;
    }
    match &args[0] {
        Object::Array(arr) => match arr.last() {
            Some(obj) => obj.clone(),
            None => NULL,
        },
        obj => Object::Error(format!(
            "argument to `last` must be ARRAY, got {}",
            obj.get_type()
        )),
    }
}

fn rest(args: Vec<Object>) -> Object {
    if let Err(e) = ensure_arguments_length(&args, 1) {
        return e;
    }
    match &args[0] {
        Object::Array(arr) => match arr.split_first() {
            Some((_, rest)) => Object::Array(rest.to_vec()),
            None => NULL,
        },
        obj => Object::Error(format!(
            "argument to `rest` must be ARRAY, got {}",
            obj.get_type()
        )),
    }
}

fn push(args: Vec<Object>) -> Object {
    if let Err(e) = ensure_arguments_length(&args, 2) {
        return e;
    }
    match (&args[0], &args[1]) {
        (Object::Array(arr), obj) => {
            let mut new_arr = arr.clone();
            new_arr.push(obj.clone());
            Object::Array(new_arr)
        }
        (obj, _) => Object::Error(format!(
            "argument to `push` must be ARRAY, got {}",
            obj.get_type()
        )),
    }
}
