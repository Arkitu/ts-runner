use std::collections::HashMap;
use std::fs;
use std::env;
use std::sync::Arc;
use tokio::sync::RwLock;
use async_recursion::async_recursion;
use fixedstr::str256;

type VariableName<'a> = str256;
type Memory<'a> = HashMap<VariableName<'a>, Value<'a>>;

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
    fn insert(&mut self, name: VariableName, value: Value<'a>) {
        self.memory.insert(name, value);
    }
    #[async_recursion]
    async fn get(&self, name: VariableName) -> Option<Value<'a>> {
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
    args: &'a Vec<VariableName<'a>>,
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
    Object(Arc<Memory<'a>>),
    Array(Arc<Vec<Value<'a>>>)
}

#[derive(Clone)]
enum Expression<'a> {
    // variable name, value
    Assignement(VariableName<'a>, Arc<Expression<'a>>),
    // function, args
    FunctionCall(Arc<Function<'a>>, Vec<Expression<'a>>),
    // variable name
    Variable(VariableName<'a>),
    // value
    Value(Value<'a>),
    None
}
impl<'a> Expression<'a> {
    #[async_recursion]
    async fn run(&self, scope: Arc<RwLock<Scope<'a>>>) -> Value<'a> {
        match self {
            Expression::Assignement(name, exp) => {
                scope.write().await.insert(name.clone(), exp.run(scope.clone()).await.clone());
                Value::None
            },
            Expression::FunctionCall(function, args) => {
                let mut fn_scope = Scope::new(Some(scope.clone()));
                let function = *function.clone();
                for (arg, exp) in function.args.iter().zip(args) {
                    fn_scope.insert(*arg, exp.run(scope.clone()).await.clone());
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

fn parse_expression(exp: &str) -> Expression<'static> {
    let exp = exp.trim();

    if exp.len() == 0 {
        return Expression::None
    }

    let mut tokens: Vec<&str> = exp.split(' ').collect();

    if tokens.contains(&"=") {
        if tokens[0] == "var" || tokens[0] == "let" || tokens[0] == "const" {
            tokens.remove(0);
        }

        let var_name = tokens[0];

        let value = &tokens[tokens.iter().position(|&t| t == "=").unwrap()+1..];
        let value = parse_expression(&value.join(" "));

        Expression::Assignement(var_name.into(), Arc::new(value))
    } else {
        Expression::None
    }
}

#[tokio::main]
async fn main() {
    let args = env::args().collect::<Vec<String>>();
    let filename = &args[1];

    let contents = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let expressions = contents.split(';');

    let mut root_scope = Scope::new(None);

    for exp in expressions {
        parse_expression(exp);
    }
}
