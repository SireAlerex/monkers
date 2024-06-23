use super::object::{error, null, Object};

pub type BuiltinFunction = fn(Vec<Object>) -> Object;

pub struct BuiltinFunctions;

macro_rules! check_arg_count {
    ($args: expr, $expected: expr, $wanted: literal) => {
        if $args.len() != $expected {
            return error!(
                "wrong number of arguments. got={}, want={}",
                $args.len(),
                $wanted
            );
        }
    };
}

macro_rules! add_builtins {
    ( $chosen_question: expr, $( $question_num: expr, $question_mod: expr ), * ) => {
        match $chosen_question {
          $($question_num => Some(Object::Builtin($question_mod)),)*
          _ => None
        }
    };
}

impl BuiltinFunctions {
    pub fn get(name: &str) -> Option<Object> {
        add_builtins!(name, "len", len, "first", first, "last", last, "rest", rest, "push", push)
    }
}

fn len(args: Vec<Object>) -> Object {
    check_arg_count!(args, 1, "1");

    let size = match args.first().unwrap() {
        Object::String(s) => s.len(),
        Object::Array(array) => array.len(),
        obj => {
            return error!("argument to 'len' not supported, got {}", obj.get_type());
        }
    };

    match TryInto::<i64>::try_into(size) {
        Ok(x) => Object::Integer(x),
        Err(err) => error!("size can't be a 64bit integer ({err})"),
    }
}

fn first(args: Vec<Object>) -> Object {
    check_arg_count!(args, 1, "1");

    match args.first().unwrap() {
        Object::Array(array) => match array.first() {
            Some(obj) => obj.clone(),
            None => null!(),
        },
        obj => error!("argument to 'first' must be ARRAY, got {}", obj.get_type()),
    }
}

fn last(args: Vec<Object>) -> Object {
    check_arg_count!(args, 1, "1");

    match args.first().unwrap() {
        Object::Array(array) => match array.last() {
            Some(obj) => obj.clone(),
            None => null!(),
        },
        obj => error!("argument to 'last' must be ARRAY, got {}", obj.get_type()),
    }
}

fn rest(args: Vec<Object>) -> Object {
    check_arg_count!(args, 1, "1");

    match args.first().unwrap() {
        Object::Array(array) => {
            if !array.is_empty() {
                let mut new_array = array.clone();
                new_array.remove(0);
                Object::Array(new_array)
            } else {
                null!()
            }
        }
        obj => error!("argument to 'rest' must be ARRAY, got {}", obj.get_type()),
    }
}

fn push(args: Vec<Object>) -> Object {
    check_arg_count!(args, 2, "2");

    match args.first().unwrap() {
        Object::Array(array) => {
            let mut new_array = array.clone();
            new_array.push(args[1].clone());
            Object::Array(new_array)
        }
        obj => error!("argument to 'push' must be ARRAY, got {}", obj.get_type()),
    }
}
