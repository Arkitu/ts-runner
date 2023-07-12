use std::collections::HashMap;
use std::fs;
use std::env;

type Memory = HashMap<String, Value>;

enum Number {
    Int(isize),
    Float(f64),
    NaN,
    Infinity
}

struct Function {
    name: String,
    args: Vec<String>,
    body: Vec<Expression>
}

enum Value {
    Number(Number),
    String(String),
    Bool(bool),
    Null,
    None,
    Undefined,
    Function(Function),
    Object(HashMap<String, Value>),
    Array(Vec<Value>)
}

enum Expression {
    // variable name, value
    Assignement(String, Value),
    // function, args
    FunctionCall(Function, Vec<Value>),
    // variable name
    Variable(String),
    // value
    Value(Value)
}
impl Expression {
    fn run(&self, memory: &mut Memory) -> Value {
        match self {
            Expression::Assignement(name, value) => {
                memory.insert(name.clone(), value.clone());
                Value::None
            },
            Expression::FunctionCall(function, args) => {
                let mut memory: HashMap<String, Value> = HashMap::new();
                for (i, arg) in args.iter().enumerate() {
                    memory.insert(function.args[i].clone(), arg.clone());
                }
                for expression in function.body.iter() {
                    expression.run(&mut memory);
                }
                Value::None
            },
            Expression::Variable(name) => {
                match memory.get(name) {
                    Some(value) => value.clone(),
                    None => Value::Undefined
                }
            },
            Expression::Value(value) => value.clone()
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
