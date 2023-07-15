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

#[derive(Clone, Debug)]
enum Number {
    Int(isize),
    Float(f64),
    NaN,
    Infinity
}

#[derive(Clone, Copy, Debug)]
struct Function<'a> {
    args: &'a Vec<VariableName<'a>>,
    body: &'a Vec<Expression<'a>>
}

#[derive(Clone, Debug)]
enum Value<'a> {
    Number(Number),
    String(String),
    Bool(bool),
    Null,
    None,
    Undefined,
    Function(Arc<Function<'a>>),
    Object(Arc<Memory<'a>>),
    Array(Arc<Vec<Value<'a>>>),
    SpreadArray(Arc<Vec<Value<'a>>>)
}

#[derive(Clone, Debug)]
enum Expression<'a> {
    ExpressionList(Vec<Expression<'a>>),
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
            Expression::ExpressionList(expressions) => {
                let mut result = Value::None;
                for expression in expressions {
                    result = expression.run(scope.clone()).await;
                }
                result
            },
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

/// Extract strings. ex: console.log("Hello world", "again!") -> console.log("0", "1")
fn parse_strings(old_exp: &mut String, values: &mut Vec<Value>) {
    let mut new_exp = old_exp.clone();
    let mut last_string_start: (bool, usize, char) = (false, 0, 'a');
    let mut removed_chars: isize = 0;

    for (i, l) in old_exp.chars().enumerate() {
        if last_string_start.0 {
            if l != last_string_start.2 {
                continue
            }
            let str_value = &old_exp[(last_string_start.1+1) as usize..i];
            values.push(Value::String(str_value.to_string()));
            new_exp.replace_range((last_string_start.1 as isize-removed_chars) as usize..(i as isize+1-removed_chars) as usize, &("\"".to_string() + &(values.len()-1).to_string() + "\""));
            last_string_start = (false, 0, 'a');
            removed_chars += str_value.len() as isize - 1;
        } else if l == '"' || l == '\'' || l == '`' {
            last_string_start = (true, i, l)
        }
    }

    *old_exp = new_exp;
}

fn parse_symbol<'a>(old_exp: &mut String, values: &mut Vec<Value<'a>>, symbol: &str, value: Value<'a>) {
    let mut new_exp = String::new();

    let list = old_exp.split(symbol);
    let length = list.clone().count();
    for (i, x) in list.enumerate() {
        new_exp.push_str(x);

        if i == length-1 {
            break
        }

        let char_before = x.chars().last().unwrap_or(symbol.chars().last().unwrap());
        if char_before.is_alphanumeric() || char_before == '_' {
            new_exp.push_str(symbol);
            continue
        }

        values.push(value.clone());
        new_exp.push_str(&("\"".to_string() + &(values.len()-1).to_string() + "\""));
    }

    *old_exp = new_exp;
}

fn parse_numbers(old_exp: &mut String, values: &mut Vec<Value>) {
    let mut new_exp = old_exp.clone();
    let mut last_number_start = (false, 0);
    let mut removed_chars: isize = 0;

    for (i, l) in old_exp.chars().enumerate() {
        if last_number_start.0 {
            if l.is_numeric() || l == '.' {
                continue
            }
            let str_value = &old_exp[last_number_start.1..i];
            if str_value.contains('.') {
                values.push(Value::Number(Number::Float(str_value.parse().unwrap())));
            } else {
                values.push(Value::Number(Number::Int(str_value.parse().unwrap())));
            }
            new_exp.replace_range((last_number_start.1 as isize-removed_chars) as usize..(i as isize-removed_chars) as usize, &("\"".to_string() + &(values.len()-1).to_string() + "\""));
            last_number_start = (false, 0);
            removed_chars += str_value.len() as isize - 3;
        } else if l.is_numeric() {
            let last_char = old_exp.chars().nth(i-1).unwrap_or(' ');

            if last_char != ' ' && last_char != '(' && last_char != '[' && last_char != '{' && last_char != ',' && last_char != ':' && last_char != '=' && last_char != '+' && last_char != '-' && last_char != '*' && last_char != '/' && last_char != '%' && last_char != '^' && last_char != '&' && last_char != '|' && last_char != '!' && last_char != '<' && last_char != '>' && last_char != '?' && last_char != '.' {
                continue
            }

            last_number_start = (true, i)
        }
    }

    *old_exp = new_exp;
}

fn parse_values(exp: &mut String, values: &mut Vec<Value>) {
    parse_strings(exp, values);
    parse_numbers(exp, values);
    parse_symbol(exp, values, "true", Value::Bool(true));
    parse_symbol(exp, values, "True", Value::Bool(true));
    parse_symbol(exp, values, "false", Value::Bool(false));
    parse_symbol(exp, values, "False", Value::Bool(false));
    parse_symbol(exp, values, "null", Value::Null);
    parse_symbol(exp, values, "undefined", Value::Undefined);
    parse_symbol(exp, values, "NaN", Value::Number(Number::NaN));
    parse_symbol(exp, values, "None", Value::None);
    parse_symbol(exp, values, "Infinity", Value::Number(Number::Infinity));
}

fn parse_expression(exp: &str, values: &mut Vec<Value>) -> Expression<'static> {
    let mut exp = exp.trim().to_string();

    if exp.len() == 0 {
        return Expression::None
    }

    if exp.contains(';') {
        let mut exps = Vec::new();
        for exp in exp.split(';') {
            exps.push(parse_expression(exp, values));
        }
        return Expression::ExpressionList(exps)
    }

    Expression::None

    // let mut tokens: Vec<&str> = exp.split(' ').collect();

    // if tokens.contains(&"=") {
    //     if tokens[0] == "var" || tokens[0] == "let" || tokens[0] == "const" {
    //         tokens.remove(0);
    //     }

    //     let var_name = tokens[0];

    //     let value = &tokens[tokens.iter().position(|&t| t == "=").unwrap()+1..];
    //     let value = parse_expression(&value.join(" "));

    //     Expression::Assignement(var_name.into(), Arc::new(value))
    // } else {
    //     Expression::None
    // }
}

fn parse_code(exp: &str) -> (Expression, Vec<Value>) {
    let mut values = Vec::new();
    let mut exp = exp.trim().to_string();
    parse_values(&mut exp, &mut values);
    let exp = parse_expression(&exp, &mut values);
    (exp, values)
}

#[tokio::main]
async fn main() {
    let args = env::args().collect::<Vec<String>>();
    let filename = &args[1];

    let content = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let mut root_scope = Scope::new(None);

    let (root_exp, values) = parse_code(&content);

    println!("{:?}", root_exp);
    println!("{:#?}", values);
}
