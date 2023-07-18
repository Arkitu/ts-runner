use std::collections::HashMap;
use std::fs;
use std::env;
use std::ops::Range;
use std::sync::Arc;
use id_vec::Id;
use tokio::sync::RwLock;
use async_recursion::async_recursion;
use fixedstr::str256;
use id_vec::IdVec;

type VariableName<'a> = str256;
type Memory<'a> = HashMap<VariableName<'a>, Value<'a>>;
type Values<'a> = IdVec<Value<'a>>;

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
    Array(Arc<Vec<Value<'a>>>)
}

#[derive(Clone, Debug)]
enum Expression<'a> {
    ExpressionList(Vec<Expression<'a>>),
    Scope(Box<Expression<'a>>),
    // variable name, value
    Assignement(VariableName<'a>, Box<Expression<'a>>),
    // function, args
    FunctionCall(Box<Function<'a>>, Vec<Expression<'a>>),
    // variable name
    Variable(VariableName<'a>),
    // value
    Value(Value<'a>),
    Return(Arc<Expression<'a>>),
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
            Expression::Scope(exp) => {
                let child_scope = Scope::new(Some(scope.clone()));
                exp.run(Arc::new(RwLock::new(child_scope))).await
            }
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
                    if let Expression::Return(exp) = expression {
                        result = exp.run(fn_scope.clone()).await;
                        break
                    }
                    expression.run(fn_scope.clone()).await;
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
            Expression::Return(exp) => {
                exp.run(scope.clone()).await
            },
            Expression::None => Value::None
        }
    }
}

fn get_val_from_id<'a>(values: &Values<'a>, id: &str) -> Option<Value<'a>> {
    match id[1..id.len()-1].parse::<usize>() {
        Ok(id) => {
            match values.get(Id::from_index(id)) {
                Some(value) => Some(value.clone()),
                None => None
            }
        },
        Err(_) => None
    }
}

fn parse_value<'a>(values: &mut Values<'a>, val: String) -> Value<'a> {
    let val = val.trim();
    if val == "null" || val == "Null" {
        Value::Null
    } else if val == "none" || val == "None" {
        Value::None
    } else if val == "undefined" || val == "Undefined" {
        Value::Undefined
    } else if val == "nan" || val == "NaN" {
        Value::Number(Number::NaN)
    } else if val == "infinity" || val == "Infinity" {
        Value::Number(Number::Infinity)
    } else if val.parse::<isize>().is_ok() {
        Value::Number(Number::Int(val.parse::<isize>().unwrap()))
    } else if val.parse::<f64>().is_ok() {
        Value::Number(Number::Float(val.parse::<f64>().unwrap()))
    } else if val == "true" || val == "True" {
        Value::Bool(true)
    } else if val == "false" || val == "False" {
        Value::Bool(false)
    } else if (val.starts_with("\"") && val.ends_with("\"")) || (val.starts_with("'") && val.ends_with("'")) || (val.starts_with("`") && val.ends_with("`")) {
        Value::String(val[1..val.len()-1].to_string())
    } else if val.starts_with("[") && val.ends_with("]") {
        let mut arr = Vec::new();
        for value in val[1..val.len()-1].split(",") {
            if value.len() == 0 {
                continue;
            }
            arr.push(get_val_from_id(values, value.trim()).expect("Invalid value"));
        }
        Value::Array(Arc::new(arr))
    } else if val.starts_with('{') && val.ends_with('}') {
        let mut obj = Memory::new();
        for value in val[1..val.len()-1].split(",") {
            if value.len() == 0 {
                continue;
            }
            let mut value = value.split(":");
            let key = value.next().unwrap().trim();
            let value = value.next().unwrap().trim();
            obj.insert(key.into(), get_val_from_id(values, value).expect("Invalid value"));
        }
        Value::Object(Arc::new(obj))
    } else {
        panic!("Invalid value: {}", val)
    }
}

/// Returns the number of characters removed
fn parse_value_in_context(ctx: &mut String, values: &mut Values, range: Range<usize>) -> isize {
    let str_val = ctx[range.clone()].to_string();
    let mut removed_chars: isize = str_val.len() as isize;
    let val = parse_value(values, str_val);
    let id = values.insert(val.clone()).index_value().to_string();
    ctx.replace_range(range, &("\"".to_string() + &id + "\""));
    removed_chars -= id.len() as isize + 2;
    removed_chars
}

#[derive(Debug)]
enum ParsedValue {
    String(char),
    Number,
    Array,
    Object,
    // length
    Symbol(usize),
    None
}

fn iter_char(values: &mut Values, exp: &str, i: usize, l: char, new_exp: &mut String, current_vals: &mut Vec<(ParsedValue, usize, isize)>, removed_chars: &mut isize) {
    let current_val = current_vals.last().unwrap_or(&(ParsedValue::None, 0, 0));

    match current_val.0 {
        ParsedValue::String(c) => {
            if l != c {
                return;
            }
            let range = (current_val.1 as isize-current_val.2) as usize..(i as isize+1-removed_chars.to_owned()) as usize;
            *removed_chars += parse_value_in_context(new_exp, values, range);
            current_vals.pop();
            return
        },
        ParsedValue::Number => {
            if l.is_numeric() || l == '.' {
                return
            }
            let range = (current_val.1 as isize-current_val.2) as usize..(i as isize-removed_chars.to_owned()) as usize;
            *removed_chars += parse_value_in_context(new_exp, values, range);
            current_vals.pop();
            iter_char(values, exp, i, l, new_exp, current_vals, removed_chars);
        },
        ParsedValue::Array => {
            if l == ']' {
                let range = (current_val.1 as isize-current_val.2) as usize..(i as isize+1-removed_chars.to_owned()) as usize;
                *removed_chars += parse_value_in_context(new_exp, values, range);
                current_vals.pop();
                return
            }
        },
        ParsedValue::Object => {
            if l == '}' {
                let range = (current_val.1 as isize-current_val.2) as usize..(i as isize+1-removed_chars.to_owned()) as usize;
                *removed_chars += parse_value_in_context(new_exp, values, range);
                current_vals.pop();
                return
            }
        },
        ParsedValue::Symbol(len) => {
            if i-current_val.1 == len {
                current_vals.pop();
            }
            return
        }
        ParsedValue::None => {}
    }

    let last_char = if i == 0 {
        ' '
    } else {
        exp.chars().nth(i-1).unwrap_or(' ')
    };

    if l == '"' || l == '\'' || l == '`' {
        current_vals.push((ParsedValue::String(l), i, removed_chars.to_owned()));
    } else if l.is_numeric() {
        current_vals.push((ParsedValue::Number, i, removed_chars.to_owned()));
    } else if last_char == ' ' || last_char == ',' || last_char == ':' || last_char == '=' || last_char == '|' || last_char == '&' || last_char == '!' || last_char == '?' || last_char == '(' || last_char == '[' {
        if l == '[' {
            current_vals.push((ParsedValue::Array, i, removed_chars.to_owned()));
        } else if l == '{' {
            current_vals.push((ParsedValue::Object, i, removed_chars.to_owned()));
        } else if exp[i..exp.len()].starts_with("true") || exp[i..exp.len()].starts_with("True") || exp[i..exp.len()].starts_with("false") || exp[i..exp.len()].starts_with("False") || exp[i..exp.len()].starts_with("null") || exp[i..exp.len()].starts_with("Null") || exp[i..exp.len()].starts_with("undefined") || exp[i..exp.len()].starts_with("Undefined") || exp[i..exp.len()].starts_with("nan") || exp[i..exp.len()].starts_with("NaN") || exp[i..exp.len()].starts_with("Infinity") || exp[i..exp.len()].starts_with("infinity") || exp[i..exp.len()].starts_with("none") || exp[i..exp.len()].starts_with("None") {
            let len = if exp[i..exp.len()].starts_with("true") || exp[i..exp.len()].starts_with("True") || exp[i..exp.len()].starts_with("null") || exp[i..exp.len()].starts_with("Null") {
                4
            } else if exp[i..exp.len()].starts_with("false") || exp[i..exp.len()].starts_with("False") {
                5
            } else if exp[i..exp.len()].starts_with("undefined") || exp[i..exp.len()].starts_with("Undefined") {
                9
            } else if exp[i..exp.len()].starts_with("nan") || exp[i..exp.len()].starts_with("NaN") {
                3
            } else if exp[i..exp.len()].starts_with("Infinity") || exp[i..exp.len()].starts_with("infinity") {
                8
            } else if exp[i..exp.len()].starts_with("none") || exp[i..exp.len()].starts_with("None") {
                4
            } else {
                0
            };
            let after_char = exp.chars().nth(i+len).unwrap_or(' ');
            if after_char == ' ' || after_char == ',' || after_char == '=' || after_char == '|' || after_char == '&' || after_char == '!' || after_char == '?' || after_char == ')' || after_char == ']' || after_char == '}' {
                let rel_i = ((i as isize)-removed_chars.to_owned()) as usize;
                let range = rel_i..(rel_i+len);
                *removed_chars += parse_value_in_context(new_exp, values, range);
                current_vals.push((ParsedValue::Symbol(len), i, removed_chars.to_owned()));
            }
        }
    }
}

fn parse_values(exp: &mut String, values: &mut Values) {
    let mut new_exp = exp.clone();
    // (value_type, start_index, removed_chars_at_start)
    let mut current_vals: Vec<(ParsedValue, usize, isize)> = Vec::new();
    let mut removed_chars: isize = 0;

    for (i, l) in exp.chars().enumerate() {
        iter_char(values, exp, i, l, &mut new_exp, &mut current_vals, &mut removed_chars)
    }

    *exp = new_exp;
}

fn standardize_code(exp: &str) -> String {
    let mut exp = exp.trim().to_string();

    if exp.contains(';') {
        let mut exps = Vec::new();
        for exp in exp.split(';') {
            exps.push(standardize_code(exp));
        }
        return exps.join(";")
    }

    exp = exp.replace('\n', "");

    if exp.contains(',') {
        let mut exps = Vec::new();
        for exp in exp.split(',') {
            exps.push(exp.trim());
        }
        exp = exps.join(",");
    }

    if exp.starts_with("var") {
        exp = exp[3..exp.len()].trim().to_string();
    } else if exp.starts_with("let") {
        exp = exp[3..exp.len()].trim().to_string();
    } else if exp.starts_with("const") {
        exp = exp[5..exp.len()].trim().to_string();
    }

    exp
}

fn parse_expression<'a>(exp: &str, values: &mut Values<'a>) -> Expression<'a> {
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
    
    if exp.starts_with("return") {
        let exp = exp.replace("return", "");
        return Expression::Return(Arc::new(parse_expression(exp.trim(), values)))
    }

    if exp.starts_with('"') && exp.ends_with('"') {
        return Expression::Value(get_val_from_id(&values, exp).expect("Invalid value"));
    }
    
    // let mut tokens: Vec<&str> = exp.split(' ').collect();

    // if tokens.contains(&"=") {
    //     if tokens[0] == "var" || tokens[0] == "let" || tokens[0] == "const" {
    //         tokens.remove(0);
    //     }

    //     let var_name = tokens[0];

    //     let value = &tokens[tokens.iter().position(|&t| t == "=").unwrap()+1..];
    //     let value = parse_expression(&value.join(" "));

    //     return Expression::Assignement(var_name.into(), Arc::new(value))
    // }

    Expression::None
}

fn parse_code(exp: &str) -> (Expression, Values) {
    let mut values: Values = IdVec::new();
    let mut exp = exp.trim().to_string();
    parse_values(&mut exp, &mut values);
    println!("{}", exp);
    let exp = &standardize_code(&exp);
    println!("{}", exp);
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
    println!("{:#?}", values.into_iter().collect::<Vec<Value>>());
}
