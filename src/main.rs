use std::collections::HashMap;
use std::fs;
use std::env;
use std::sync::Arc;
use tokio::sync::RwLock;
use async_recursion::async_recursion;

type Memory<'a> = HashMap<Arc<String>, Arc<RwLock<Value<'a>>>>;

struct Scope<'a> {
    memory: Memory<'a>,
    pub parent: Option<Arc<RwLock<&'a mut Scope<'a>>>>
}
impl<'a> Scope<'a> {
    fn new(parent: Option<Arc<RwLock<&'a mut Scope<'a>>>>) -> Scope<'a> {
        Scope {
            memory: HashMap::new(),
            parent
        }
    }
    fn insert(&mut self, name: Arc<String>, value: Value<'a>) {
        self.memory.insert(name, Arc::new(RwLock::new(value)));
    }
    #[async_recursion]
    async fn get(&self, name: Arc<String>) -> Option<Arc<RwLock<Value<'a>>>> {
        match self.memory.get(&name) {
            Some(value) => Some(value.clone()),
            None => {
                match &self.parent {
                    Some(parent) => parent.read().await.get(name).await,
                    None => None
                }
            }
        }
    }
    fn new_child(&'a mut self) -> Scope<'a> {
        Scope::new(Some(Arc::new(RwLock::new(self))))
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
    args: &'a Vec<Arc<String>>,
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
    Function(Arc<Function<'a>>),
    Object(Arc<HashMap<String, Value<'a>>>),
    Array(Arc<Vec<Value<'a>>>)
}

#[derive(Clone)]
enum Expression<'a> {
    // variable name, value
    Assignement(Arc<String>, Value<'a>),
    // function, args
    FunctionCall(Arc<Function<'a>>, Vec<Value<'a>>),
    // variable name
    Variable(String),
    // value
    Value(Value<'a>),
    None
}
impl<'a> Expression<'a> {
    fn run(&self, scope: &'a mut Scope<'a>) -> (Value<'a>, &'a mut Scope<'a>) {
        match self {
            Expression::Assignement(name, value) => {
                scope.insert(name.clone(), value.clone());
                (Value::None, scope)
            },
            Expression::FunctionCall(function, args) => {
                let mut fn_scope = scope.new_child();
                let function = *function.clone();
                for (arg, value) in function.args.iter().zip(args) {
                    fn_scope.insert(arg.clone(), value.clone());
                }
                let mut result = Value::None;

                let mut refe = &mut fn_scope;

                for expression in function.body {
                    refe = expression.run(refe).1
                }
                (result, scope)
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
