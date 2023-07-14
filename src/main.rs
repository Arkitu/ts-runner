use std::collections::HashMap;
use std::fs;
use std::env;
use std::sync::Arc;
use tokio::sync::RwLock;
use async_recursion::async_recursion;

type Memory<'a> = HashMap<String, Value<'a>>;

#[derive(Clone)]
struct Scope<'a> {
    memory: Memory<'a>,
    pub parent: Option<Arc<RwLock<Scope<'a>>>>
}
impl<'a> Scope<'a> {
    fn new(parent: Option<Arc<RwLock<Scope<'a>>>>) -> Scope<'a> {
        Scope {
            memory: HashMap::new(),
            parent
        }
    }
    fn insert(&mut self, name: String, value: Value<'a>) {
        self.memory.insert(name, value);
    }
    #[async_recursion]
    async fn get(&self, name: String) -> Option<Value<'a>> {
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
    Assignement(String, Value<'a>),
    // function, args
    FunctionCall(Arc<Function<'a>>, Vec<Value<'a>>),
    // variable name
    Variable(String),
    // value
    Value(Value<'a>),
    None
}
impl<'a> Expression<'a> {
    #[async_recursion]
    async fn run(&self, scope: Arc<RwLock<Scope<'a>>>) -> Value<'a> {
        match self {
            Expression::Assignement(name, value) => {
                scope.write().await.insert(name.clone(), value.clone());
                Value::None
            },
            Expression::FunctionCall(function, args) => {
                let mut fn_scope = Scope::new(Some(scope));
                let function = *function.clone();
                for (arg, value) in function.args.iter().zip(args) {
                    fn_scope.insert(arg.to_string(), value.clone());
                }
                let mut result = Value::None;
                let fn_scope = Arc::new(RwLock::new(fn_scope));
                for expression in function.body {
                    result = expression.run(fn_scope.clone()).await;
                }
                result
            },
            Expression::Variable(name) => {
                match scope.read().await.get(name.clone()).await {
                    Some(value) => value,
                    None => Value::Undefined
                }
            },
            Expression::Value(value) => value.clone(),
            Expression::None => Value::None
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

    let mut root_scope = Scope::new(None);
}
