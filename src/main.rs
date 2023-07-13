use std::collections::HashMap;
use std::fs;
use std::env;
use std::rc::Rc;

type Memory<'a> = HashMap<&'a String, Value<'a>>;

struct Scope<'a> {
    memory: Memory<'a>,
    pub parent: Option<&'a Scope<'a>>
}
impl<'a> Scope<'a> {
    fn new(parent: Option<&'a Scope<'a>>) -> Scope<'a> {
        Scope {
            memory: HashMap::new(),
            parent
        }
    }
    fn insert(&mut self, name: &'a String, value: Value<'a>) {
        self.memory.insert(name, value);
    }
    fn get(&self, name: &String) -> Option<Value> {
        match self.memory.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None
            }
        }
    }
    fn new_child(&mut self) -> Scope {
        Scope::new(Some(self))
    }
}

#[derive(Clone, Copy)]
enum Number {
    Int(isize),
    Float(f64),
    NaN,
    Infinity
}

#[derive(Clone, Copy)]
struct Function<'a> {
    name: &'a String,
    args: &'a Vec<&'a String>,
    body: &'a Vec<Expression<'a>>
}

#[derive(Clone)]
enum Value<'a> {
    Number(Number),
    String(String),
    Bool(bool),
    Null,
    None,
    Undefined,
    Function(Rc<Function<'a>>),
    Object(Rc<HashMap<String, Value<'a>>>),
    Array(Rc<Vec<Value<'a>>>)
}

#[derive(Clone)]
enum Expression<'a> {
    // variable name, value
    Assignement(&'a String, Value<'a>),
    // function, args
    FunctionCall(Rc<Function<'a>>, Vec<Value<'a>>),
    // variable name
    Variable(String),
    // value
    Value(Value<'a>),
    None
}
impl<'a> Expression<'a> {
    fn run(&self, scope: &'a mut Scope<'a>) -> Value<'a> {
        match self {
            Expression::Assignement(name, value) => {
                scope.insert(name, value.clone());
                Value::None
            },
            Expression::FunctionCall(function, args) => {
                let mut fn_scope = scope.new_child();
                let function = *function.clone();
                for (arg, value) in function.args.iter().zip(args) {
                    fn_scope.insert(arg, value.clone());
                }
                let mut result = Value::None;

                for expression in function.body {
                    let refe = &mut fn_scope;
                    expression.run(refe);
                }
                result
            }
        }
    }
}

fn main() {
    let args = env::args().collect::<Vec<String>>();
    let filename = &args[1];

    let contents = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let lines = contents.split(';');

    let mut memory: HashMap<String, Value> = HashMap::new();


}
