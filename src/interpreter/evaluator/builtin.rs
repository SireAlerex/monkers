use super::object::{error, Object};

pub type BuiltinFunction = fn(Vec<Object>) -> Object;

pub struct BuiltinFunctions;

impl BuiltinFunctions {
    pub fn get(name: &str) -> Option<Object> {
        match name {
            "len" => Some(Object::Builtin(len)),
            _ => None
        }
    }
}

fn len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return error!("wrong number of arguments. got={}, want=1", args.len());
    }

    match args.first().unwrap() {
        Object::String(s) => match TryInto::<i64>::try_into(s.len()) {
            Ok(x) => Object::Integer(x),
            Err(err) => error!("size can't be a 64bit integer ({err})"),
        },
        obj => error!("argument to 'len' not supported, got {}", obj.get_type())
    }
}
