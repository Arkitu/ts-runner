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

/// Extract strings. ex: console.log("Hello world", "again!") -> console.log("0", "1")
fn parse_strings(old_exp: &mut String, values: &mut Values) {
    let mut new_exp = old_exp.clone();
    let mut last_string_start: (bool, usize, char) = (false, 0, 'a');
    let mut removed_chars: isize = 0;

    for (i, l) in old_exp.chars().enumerate() {
        if last_string_start.0 {
            if l != last_string_start.2 {
                continue
            }
            let range = (last_string_start.1 as isize-removed_chars) as usize..(i as isize+1-removed_chars) as usize;
            removed_chars += parse_value_in_context(&mut new_exp, values, range);
            last_string_start = (false, 0, 'a');
        } else if l == '"' || l == '\'' || l == '`' {
            last_string_start = (true, i, l)
        }
    }

    *old_exp = new_exp;
}

fn parse_symbol<'a>(old_exp: &mut String, values: &mut Values<'a>, symbol: &str, value: Value<'a>) {
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

        let id = values.insert(value.clone()).index_value().to_string();
        new_exp.push_str(&("\"".to_string() + &id + "\""));
    }

    *old_exp = new_exp;
}

fn parse_numbers(old_exp: &mut String, values: &mut Values) {
    let mut new_exp = old_exp.clone();
    let mut last_number_start = (false, 0);
    let mut removed_chars: isize = 0;

    for (i, l) in old_exp.chars().enumerate() {
        if last_number_start.0 {
            if l.is_numeric() || l == '.' {
                continue
            }
            let range = (last_number_start.1 as isize-removed_chars) as usize..(i as isize-removed_chars) as usize;
            removed_chars += parse_value_in_context(&mut new_exp, values, range);
            last_number_start = (false, 0);
        } else if l.is_numeric() {
            let last_char = old_exp.chars().nth(i-1).unwrap_or(' ');

            if last_char != ' ' && last_char != '(' && last_char != '[' && last_char != ',' && last_char != '=' && last_char != '+' && last_char != '-' && last_char != '*' && last_char != '/' && last_char != '%' && last_char != '^' && last_char != '&' && last_char != '|' && last_char != '!' && last_char != '<' && last_char != '>' && last_char != '?' {
                continue
            }

            last_number_start = (true, i)
        }
    }

    *old_exp = new_exp;
}

fn parse_arrays(old_exp: &mut String, values: &mut Values) {
    let mut new_exp = old_exp.clone();
    let mut last_arr_start: (bool, usize) = (false, 0);
    let mut removed_chars: isize = 0;

    for (i, l) in old_exp.chars().enumerate() {
        if last_arr_start.0 {
            if l != ']' {
                continue
            }
            let range = (last_arr_start.1 as isize-removed_chars) as usize..(i as isize+1-removed_chars) as usize;
            removed_chars += parse_value_in_context(&mut new_exp, values, range.clone());
            last_arr_start = (false, 0);
        } else if l == '[' {
            let last_char = old_exp.chars().nth(i-1).unwrap_or(' ');

            if last_char != ' ' && last_char != ',' && last_char != '=' && last_char != '|' && last_char != '&' && last_char != '!' && last_char != '<' && last_char != '>' && last_char != '?' && last_char != '(' && last_char != '[' {
                continue
            }

            last_arr_start = (true, i)
        }
    }

    *old_exp = new_exp;
}

fn parse_object(old_exp: &mut String, values: &mut Values) {
    let mut new_exp = old_exp.clone();
    let mut last_arr_start = (false, 0);
    let mut removed_chars: isize = 0;

    for (i, l) in old_exp.chars().enumerate() {
        if last_arr_start.0 {
            if l != '}' {
                continue
            }
            let str_value = &old_exp[(last_arr_start.1+1)..i];
            
            let mut obj: HashMap<VariableName, Value<'_>> = HashMap::new();
            for pair in str_value.split(',') {
                if pair == "" {
                    continue
                }
                let (key, val) = pair.split_at(pair.find(':').unwrap());
                let key = key.trim().to_string();
                let mut val = val.trim().to_string();
                val.pop();
                val.remove(0);
                let val = val.parse::<usize>().unwrap();
                let val = Id::from_index(val);
                obj.insert(key.into(), values.get(val).unwrap().clone());
                values.remove(val);
            }

            let id = values.insert(Value::Object(Arc::new(obj))).index_value().to_string();

            new_exp.replace_range((last_arr_start.1 as isize-removed_chars) as usize..(i as isize+1-removed_chars) as usize, &("\"".to_string() + &id + "\""));
            last_arr_start = (false, 0);
            removed_chars += str_value.len() as isize - 1;
        } else if l == '{' {
            let last_char = old_exp.chars().nth(i-1).unwrap_or(' ');

            if last_char != ' ' && last_char != ',' && last_char != '=' && last_char != '|' && last_char != '&' && last_char != '!' && last_char != '<' && last_char != '>' && last_char != '?' && last_char != '(' && last_char != '[' {
                continue
            }

            last_arr_start = (true, i)
        }
    }

    *old_exp = new_exp;
}

#[derive(Debug)]
enum ParseValue {
    String(char),
    Number,
    Array,
    Object,
    // length
    Symbol(usize),
    None
}

fn iter_char(values: &mut Values, exp: &str, i: usize, l: char, new_exp: &mut String, current_vals: &mut Vec<(ParseValue, usize, isize)>, removed_chars: &mut isize) {
    let current_val = current_vals.last().unwrap_or(&(ParseValue::None, 0, 0));

    match current_val.0 {
        ParseValue::String(c) => {
            if l != c {
                return;
            }
            let range = (current_val.1 as isize-current_val.2) as usize..(i as isize+1-removed_chars.to_owned()) as usize;
            *removed_chars += parse_value_in_context(new_exp, values, range);
            current_vals.pop();
            return
        },
        ParseValue::Number => {
            if l.is_numeric() || l == '.' {
                return
            }
            let range = (current_val.1 as isize-current_val.2) as usize..(i as isize-removed_chars.to_owned()) as usize;
            *removed_chars += parse_value_in_context(new_exp, values, range);
            current_vals.pop();
            iter_char(values, exp, i, l, new_exp, current_vals, removed_chars);
        },
        ParseValue::Array => {
            if l == ']' {
                let range = (current_val.1 as isize-current_val.2) as usize..(i as isize+1-removed_chars.to_owned()) as usize;
                *removed_chars += parse_value_in_context(new_exp, values, range);
                current_vals.pop();
                return
            }
        },
        ParseValue::Object => {
            if l == '}' {
                let range = (current_val.1 as isize-current_val.2) as usize..(i as isize+1-removed_chars.to_owned()) as usize;
                *removed_chars += parse_value_in_context(new_exp, values, range);
                current_vals.pop();
                return
            }
        },
        ParseValue::Symbol(len) => {
            if i-current_val.1 == len {
                current_vals.pop();
            }
            return
        }
        ParseValue::None => {}
    }

    let last_char = if i == 0 {
        ' '
    } else {
        exp.chars().nth(i-1).unwrap_or(' ')
    };

    if l == '"' || l == '\'' || l == '`' {
        current_vals.push((ParseValue::String(l), i, removed_chars.to_owned()));
    } else if l.is_numeric() {
        current_vals.push((ParseValue::Number, i, removed_chars.to_owned()));
    } else if last_char == ' ' || last_char == ',' || last_char == ':' || last_char == '=' || last_char == '|' || last_char == '&' || last_char == '!' || last_char == '?' || last_char == '(' || last_char == '[' {
        if l == '[' {
            current_vals.push((ParseValue::Array, i, removed_chars.to_owned()));
        } else if l == '{' {
            current_vals.push((ParseValue::Object, i, removed_chars.to_owned()));
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
                current_vals.push((ParseValue::Symbol(len), i, removed_chars.to_owned()));
            }
        }
    }
}

fn parse_values(exp: &mut String, values: &mut Values) {
    let mut new_exp = exp.clone();
    // (value_type, start_index, removed_chars_at_start)
    let mut current_vals: Vec<(ParseValue, usize, isize)> = Vec::new();
    let mut removed_chars: isize = 0;

    for (i, l) in exp.chars().enumerate() {
        iter_char(values, exp, i, l, &mut new_exp, &mut current_vals, &mut removed_chars)
    }
    // parse_symbol(exp, values, "true", Value::Bool(true));
    // parse_symbol(exp, values, "True", Value::Bool(true));
    // parse_symbol(exp, values, "false", Value::Bool(false));
    // parse_symbol(exp, values, "False", Value::Bool(false));
    // parse_symbol(exp, values, "null", Value::Null);
    // parse_symbol(exp, values, "undefined", Value::Undefined);
    // parse_symbol(exp, values, "NaN", Value::Number(Number::NaN));
    // parse_symbol(exp, values, "None", Value::None);
    // parse_symbol(exp, values, "Infinity", Value::Number(Number::Infinity));

    // parse_strings(exp, values);
    // parse_numbers(exp, values);
    
    // parse_arrays(exp, values);
    // parse_object(exp, values);
}

fn parse_expression(exp: &str, values: &mut Values) -> Expression<'static> {
    let exp = exp.trim().to_string();

    if exp.len() == 0 {
        return Expression::None
    }

    if exp.contains(';') {
        let mut exps = Vec::new();
        for exp in exp.split(';') {
            exps.push(parse_expression(exp, values));
        }
        return Expression::ExpressionList(exps)
    } else if exp.starts_with("return") {
        let exp = exp.replace("return", "");
        return Expression::Return(Arc::new(parse_expression(&exp, values)))
    }
    
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
    Expression::None
}

fn parse_code(exp: &str) -> (Expression, Values) {
    let mut values: Values = IdVec::new();
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
    println!("{:#?}", values.into_iter().collect::<Vec<Value>>());
}
