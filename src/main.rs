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

type ParsedExpressions<'a> = IdVec<Expression<'a>>;

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
impl Into<bool> for Value<'_> {
    fn into(self) -> bool {
        match self {
            Value::Bool(b) => b,
            Value::Number(Number::Int(i)) => i != 0,
            Value::Number(Number::Float(f)) => f != 0.0,
            Value::Number(Number::NaN) => false,
            Value::Number(Number::Infinity) => true,
            Value::String(s) => !s.is_empty(),
            Value::Null => false,
            Value::None => false,
            Value::Undefined => false,
            Value::Function(_) => true,
            Value::Object(o) => !o.is_empty(),
            Value::Array(a) => a.len() != 0
        }
    }
}
impl<'a> Into<Expression<'a>> for Value<'a> {
    fn into(self) -> Expression<'a> {
        Expression::Value(self)
    }
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
    // condition, if true, else
    If(Box<Expression<'a>>, Box<Expression<'a>>, Option<Box<Expression<'a>>>),
    // value
    Value(Value<'a>),
    Return(Arc<Expression<'a>>),
    ArrayBuilder(Vec<Expression<'a>>),
    ObjectBuilder(HashMap<VariableName<'a>, Expression<'a>>),
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
            Expression::If(condition, if_exp, else_exp) => {
                if condition.run(scope.clone()).await.into() {
                    if_exp.run(scope.clone()).await
                } else {
                    match else_exp {
                        Some(exp) => exp.run(scope.clone()).await,
                        None => Value::None
                    }
                }
            }
            Expression::Value(value) => value.clone(),
            Expression::Return(exp) => {
                exp.run(scope.clone()).await
            },
            Expression::ArrayBuilder(expressions) => {
                let mut result = Vec::new();
                for expression in expressions {
                    result.push(expression.run(scope.clone()).await);
                }
                Value::Array(Arc::new(result))
            },
            Expression::ObjectBuilder(expressions) => {
                let mut result = HashMap::new();
                for (name, expression) in expressions {
                    result.insert(name.clone(), expression.run(scope.clone()).await);
                }
                Value::Object(Arc::new(result))
            },
            Expression::None => Value::None
        }
    }
}

fn get_exp_from_id<'a>(values: &ParsedExpressions<'a>, id: &str) -> Option<Expression<'a>> {
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

fn parse_value<'a>(values: &mut ParsedExpressions<'a>, val: String) -> Result<Value<'a>, ()> {
    let val = val.trim();
    Ok(if val == "null" || val == "Null" {
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
            let val = get_exp_from_id(values, value.trim()).expect("Invalid value");
            if let Expression::Value(v) = val {
                arr.push(v);
            } else {
                return Err(());
            }
        }
        Value::Array(Arc::new(arr))
    } else if val.starts_with('{') && val.ends_with('}') {
        let mut obj = Memory::new();
        for value in val[1..val.len()-1].split(",") {
            if value.len() == 0 {
                continue;
            }
            let mut value = value.split(":");
            let key = value.next().ok_or(())?.trim();
            let value = value.next().ok_or(())?.trim();
            let val = get_exp_from_id(values, value).expect("Invalid value");
            if let Expression::Value(v) = val {
                obj.insert(key.into(), v);
            } else {
                return Err(());
            }
        }
        Value::Object(Arc::new(obj))
    } else {
        return Err(());
    })
}

/// Returns the number of characters removed
fn parse_value_in_context(ctx: &mut String, values: &mut ParsedExpressions, range: Range<usize>) -> Result<isize, ()> {
    let str_val = ctx[range.clone()].to_string();
    let mut removed_chars: isize = str_val.len() as isize;
    let val = parse_value(values, str_val)?;
    let id = values.insert(val.clone().into()).index_value().to_string();
    ctx.replace_range(range, &("\"".to_string() + &id + "\""));
    removed_chars -= id.len() as isize + 2;
    Ok(removed_chars)
}

fn parse_expression_in_context<'a>(ctx: &'a mut String, values: &'a mut ParsedExpressions<'a>, range: Range<usize>) -> Result<isize, ()> {
    let str_val = ctx[range.clone()].to_string();
    let mut removed_chars: isize = str_val.len() as isize;
    let val = parse_code_with_values(&str_val, values);
    let id = values.insert(val.clone()).index_value().to_string();
    ctx.replace_range(range, &("\"".to_string() + &id + "\""));
    removed_chars -= id.len() as isize + 2;
    Ok(removed_chars)
}

#[derive(Debug)]
enum ParsedValue {
    // delimiter
    String(char),
    Number,
    Array,
    // length
    Symbol(usize),
    Scope,
    ScopeOrObject,
    None
}

fn iter_char(values: &mut ParsedExpressions, exp: &str, i: usize, l: char, new_exp: &mut String, current_vals: &mut Vec<(ParsedValue, usize, isize)>, removed_chars: &mut isize) {
    let current_val = current_vals.last().unwrap_or(&(ParsedValue::None, 0, 0));

    match current_val.0 {
        ParsedValue::String(c) => {
            if l != c {
                return;
            }
            let range = (current_val.1 as isize-current_val.2) as usize..(i as isize+1-removed_chars.to_owned()) as usize;
            *removed_chars += parse_value_in_context(new_exp, values, range).expect("Invalid value");
            current_vals.pop();
            return
        },
        ParsedValue::Number => {
            if l.is_numeric() || l == '.' {
                return
            }
            let range = (current_val.1 as isize-current_val.2) as usize..(i as isize-removed_chars.to_owned()) as usize;
            *removed_chars += parse_value_in_context(new_exp, values, range).expect("Invalid value");
            current_vals.pop();
            iter_char(values, exp, i, l, new_exp, current_vals, removed_chars);
        },
        ParsedValue::Array => {
            if l == ']' {
                let range = (current_val.1 as isize-current_val.2) as usize..(i as isize+1-removed_chars.to_owned()) as usize;
                *removed_chars += parse_value_in_context(new_exp, values, range).expect("Invalid value");
                current_vals.pop();
                return
            }
        },
        ParsedValue::ScopeOrObject => {
            if l == '}' {
                let range = (current_val.1 as isize-current_val.2) as usize..(i as isize+1-removed_chars.to_owned()) as usize;

                if let Ok(i) = parse_value_in_context(new_exp, values, range.clone()) {
                    *removed_chars += i
                } else {
                    // scope
                    *removed_chars += parse_expression_in_context(new_exp, values, range).expect("Invalid value");
                }
                current_vals.pop();
                return
            }
        },
        ParsedValue::Scope => {
            if l == '}' {
                let range = (current_val.1 as isize-current_val.2) as usize..(i as isize+1-removed_chars.to_owned()) as usize;
                *removed_chars += parse_expression_in_context(new_exp, values, range).expect("Invalid value");
                current_vals.pop();
                return
            }
        }
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
    } else if l == '{' {
        // scope
        if last_char == ' ' || last_char == ',' || last_char == ':' || last_char == '=' || last_char == '|' || last_char == '&' || last_char == '!' || last_char == '?' || last_char == '(' || last_char == '[' {
            current_vals.push((ParsedValue::ScopeOrObject, i, removed_chars.to_owned()));
        } else {
            current_vals.push((ParsedValue::Scope, i, removed_chars.to_owned()));
        }
    } else if last_char == ' ' || last_char == ',' || last_char == ':' || last_char == '=' || last_char == '|' || last_char == '&' || last_char == '!' || last_char == '?' || last_char == '(' || last_char == '[' {
        if l == '[' {
            current_vals.push((ParsedValue::Array, i, removed_chars.to_owned()));
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
                *removed_chars += parse_value_in_context(new_exp, values, range).expect("Invalid value");
                current_vals.push((ParsedValue::Symbol(len), i, removed_chars.to_owned()));
            }
        }
    }
}

fn parse_values(exp: &mut String, values: &mut ParsedExpressions) {
    let mut new_exp = exp.clone();
    // (value_type, start_index, removed_chars_at_start)
    let mut current_vals: Vec<(ParsedValue, usize, isize)> = Vec::new();
    let mut removed_chars: isize = 0;

    for (i, l) in exp.chars().enumerate() {
        iter_char(values, exp, i, l, &mut new_exp, &mut current_vals, &mut removed_chars)
    }

    *exp = new_exp;
}

const NO_SPACE_TOKENS: [&str; 47] = [
    "+", "-", "*", "/", "%", "^",
    "(", ")",
    "[", "]",
    "{", "}",
    ":", ",", ";",
    "=", "==", "===", "!=", "!==", ">", "<", ">=", "<=", "&&", "||", "!", "?", ".", "..", "...", "=>", "=>>", "<<", ">>", ">>>", "|", "&", "~", "++", "--", "+=", "-=", "*=", "/=", "%=", "^=",
];

const SPACE_TOKEN: [&str; 75] = [
    "else", "if", "for", "while", "do", "switch", "case", "break", "continue", "return", "try", "catch", "finally", "throw", "new", "in", "of", "as", "is", "typeof", "void", "delete", "instanceof", "yield", "await", "async", "function", "class", "extends", "interface", "implements", "package", "import", "export", "from", "const", "let", "var", "with", "debugger", "default", "this", "super", "static", "private", "protected", "public", "enum", "declare", "module", "namespace", "require", "global", "declare", "type", "any", "number", "boolean", "string", "symbol", "object", "undefined", "null", "true", "false", "NaN", "Infinity", "none", "undefined", "null", "true", "false", "NaN", "Infinity", "none"
];

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

    for token in NO_SPACE_TOKENS.iter() {
        if exp.contains(token) {
            let mut exps = Vec::new();
            for exp in exp.split(token) {
                exps.push(exp.trim());
            }
            exp = exps.join(token);
        }
    }

    for token in SPACE_TOKEN.iter() {
        if exp.contains(token) {
            let mut new_exp = String::new();
            let mut last_e = String::new();
            let mut last_was_token = false;

            for e in exp.split(token) {
                if e.len() == 0 {continue}

                let e = e.to_string();

                if last_e.chars().last().unwrap_or(' ').is_alphanumeric() || e.chars().nth(0).unwrap().is_alphanumeric() {
                    if last_was_token {
                        new_exp.push_str(" ");
                    }
                    new_exp.push_str(&e);
                    new_exp.push_str(token);
                    last_was_token = false;
                } else {
                    new_exp = new_exp.trim_end().to_string() + " " + token + " " + e.trim_start();
                    last_was_token = true;
                }

                last_e = e;
            }

            exp = new_exp;
        }
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

fn parse_expression<'a>(exp: &str, values: &mut ParsedExpressions<'a>) -> Expression<'a> {
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

    if let Some(v) = get_exp_from_id(&values, exp) {
        return v
    }

    if exp.starts_with('"') && exp.ends_with('"') {
        return get_exp_from_id(&values, exp).expect("Invalid value");
    }

    println!("exp avant: {}", exp);
    if exp.starts_with('(') && exp.ends_with(')') {
        let exp = exp[1..exp.len()-1].trim();
        println!("exp: {}", exp);
        return parse_expression(exp, values)
    }
    
    if exp.starts_with("return") {
        let exp = exp.replace("return", "");
        return Expression::Return(Arc::new(parse_expression(exp.trim(), values)))
    }

    if exp.starts_with("if") {
        // format: if condition { body } else { body }
        let mut exp = exp[2..exp.len()].trim().to_string();
        let mut layers = 0;
        let mut condition = String::new();

        for l in exp.chars() {
            if l == '(' || l == '{' || l == '[' {
                layers += 1;
            } else if l == ')' || l == '}' || l == ']' {
                layers -= 1;
            }

            if layers == 0 && l == '{' && condition.len() > 0 {
                break;
            } else {
                condition.push(l);
            }
        }

        exp = exp[condition.to_string().len()..exp.len()].trim().to_string();
        let mut body = String::new();
        layers = 0;

        for l in exp.chars() {
            if l == '(' || l == '{' || l == '[' {
                layers += 1;
            } else if l == ')' || l == '}' || l == ']' {
                layers -= 1;
            }

            if layers == 0 {
                body.push(l);
                break;
            } else {
                body.push(l);
            }
        }

        let condition = parse_expression(condition.trim(), values);

        println!("condition: {:?}", condition);
    }

    Expression::None
}

fn parse_code_with_values<'a>(exp: &str, values: &'a mut ParsedExpressions<'a>) -> Expression<'a> {
    let mut exp = exp.trim().to_string();
    parse_values(&mut exp, values);
    println!("{}", exp);
    let exp = &standardize_code(&exp);
    println!("{}", exp);
    let exp = parse_expression(&exp, values);
    exp
}

fn parse_code<'a>(exp: &str) -> (Expression<'a>, ParsedExpressions<'a>) {
    let mut values: ParsedExpressions = IdVec::new();
    let exp = parse_code_with_values(exp, &mut values);
    (exp, values)
}

#[tokio::main]
async fn main() {
    let args = env::args().collect::<Vec<String>>();
    let filename = &args[1];

    let content = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    let mut root_scope = Scope::new(None);

    let (root_exp, exps) = parse_code(&content);

    println!("{:?}", root_exp);
    println!("{:#?}", exps.into_iter().collect::<Vec<Expression>>());
}
