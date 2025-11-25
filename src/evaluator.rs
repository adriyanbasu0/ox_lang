use std::fmt;
use crate::ast::{BlockStatement, Expression, Program, Statement};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

pub const NULL: Object = Object::Null;
pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Return(Box<Object>),
    Function(Function),
    Null,
    Error(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub parameters: Vec<crate::ast::Identifier>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Null => false,
            Object::Boolean(b) => *b,
            _ => true,
        }
    }

    pub fn type_str(&self) -> &str {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::String(_) => "STRING",
            Object::Return(_) => "RETURN_VALUE",
            Object::Function(_) => "FUNCTION",
            Object::Null => "NULL",
            Object::Error(_) => "ERROR",
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::String(value) => write!(f, "{}", value),
            Object::Return(value) => write!(f, "{}", value),
            Object::Function(func) => {
                let params: Vec<String> = func.parameters.iter().map(|p| p.value.clone()).collect();
                write!(f, "fn({}) {{ ... }}", params.join(", "))
            },
            Object::Null => write!(f, "null"),
            Object::Error(value) => write!(f, "ERROR: {}", value),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed_environment(outer: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(obj) => Some(obj.clone()),
            None => self.outer.as_ref().and_then(|e| e.borrow().get(name)),
        }
    }

    pub fn set(&mut self, name: String, val: Object) -> Object {
        self.store.insert(name, val.clone());
        val
    }
}

pub fn eval(program: Program, env: &mut Rc<RefCell<Environment>>) -> Object {
    let mut result = NULL;
    for statement in program.statements {
        result = eval_statement(statement, env.clone());
        if let Object::Return(value) = result {
            return *value;
        }
        if let Object::Error(_) = result {
            return result;
        }
    }
    result
}

fn eval_statement(statement: Statement, env: Rc<RefCell<Environment>>) -> Object {
    match statement {
        Statement::Expression(expr) => eval_expression(expr.expression, env),
        Statement::Return(ret_stmt) => {
            let val = eval_expression(ret_stmt.return_value, env);
            if is_error(&val) {
                return val;
            }
            Object::Return(Box::new(val))
        }
        Statement::Let(let_stmt) => {
            let val = eval_expression(let_stmt.value, env.clone());
            if is_error(&val) {
                return val;
            }
            env.borrow_mut().set(let_stmt.name.value, val)
        }
        Statement::Print(print_stmt) => eval_print_statement(print_stmt.expression, env),
    }
}

fn eval_print_statement(expression: Expression, env: Rc<RefCell<Environment>>) -> Object {
    let value = eval_expression(expression, env);
    if is_error(&value) {
        return value;
    }
    println!("{}", value);
    NULL
}

fn eval_expression(expression: Expression, env: Rc<RefCell<Environment>>) -> Object {
    match expression {
        Expression::IntLiteral(int) => Object::Integer(int.value),
        Expression::Boolean(boolean) => native_bool_to_boolean_object(boolean.value),
        Expression::StringLiteral(s) => Object::String(s.value),
        Expression::Prefix(prefix) => {
            let right = eval_expression(*prefix.right, env);
            if is_error(&right) {
                return right;
            }
            eval_prefix_expression(prefix.operator, right)
        }
        Expression::Infix(infix) => {
            let left = eval_expression(*infix.left, env.clone());
            if is_error(&left) {
                return left;
            }
            let right = eval_expression(*infix.right, env);
            if is_error(&right) {
                return right;
            }
            eval_infix_expression(infix.operator, left, right)
        }
        Expression::If(if_exp) => {
            let condition = eval_expression(*if_exp.condition, env.clone());
            if is_error(&condition) {
                return condition;
            }
            if condition.is_truthy() {
                eval_block_statement(if_exp.consequence, env)
            } else if let Some(alt) = if_exp.alternative {
                eval_block_statement(alt, env)
            } else {
                NULL
            }
        }
        Expression::Identifier(ident) => eval_identifier(ident, env),
        Expression::Function(func) => Object::Function(Function {
            parameters: func.parameters,
            body: func.body,
            env: env, // Capture the current Rc<RefCell<Environment>> directly
        }),
        Expression::Call(call_exp) => {
            let function = eval_expression(*call_exp.function, env.clone());
            if is_error(&function) {
                return function;
            }

            let args = eval_expressions(call_exp.arguments, env);
            if args.len() == 1 && is_error(&args[0]) {
                return args[0].clone();
            }

            apply_function(function, args)
        }
    }
}

fn eval_block_statement(block: BlockStatement, env: Rc<RefCell<Environment>>) -> Object {
    let mut result = NULL;
    for statement in block.statements {
        result = eval_statement(statement, env.clone());
        if let Object::Return(_) | Object::Error(_) = result {
            return result;
        }
    }
    result
}

fn eval_prefix_expression(operator: crate::token::Token, right: Object) -> Object {
    match operator {
        crate::token::Token::Bang => eval_bang_operator_expression(right),
        crate::token::Token::Minus => eval_minus_operator_expression(right),
        _ => Object::Error(format!("Unknown operator: {:?}{:?}", operator, right)),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => TRUE,
        _ => FALSE,
    }
}

fn eval_minus_operator_expression(right: Object) -> Object {
    if let Object::Integer(value) = right {
        Object::Integer(-value)
    } else {
        Object::Error(format!("Unknown operator: -{:?}", right))
    }
}

fn eval_infix_expression(operator: crate::token::Token, left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(l_val), Object::Integer(r_val)) => {
            eval_integer_infix_expression(operator, l_val, r_val)
        }
        (Object::Boolean(l_val), Object::Boolean(r_val)) => {
            eval_boolean_infix_expression(operator, l_val, r_val)
        }
        (Object::String(l_val), Object::String(r_val)) => {
            if operator == crate::token::Token::Plus {
                Object::String(format!("{}{}", l_val, r_val))
            } else {
                Object::Error(format!("Unknown operator: {:?} {:?} {:?}", l_val, operator, r_val))
            }
        }
        (l, r) if l.type_str() != r.type_str() => {
            Object::Error(format!(
                "Type mismatch: {:?} {:?} {:?}",
                l.type_str(),
                operator,
                r.type_str()
            ))
        }
        (l, r) => Object::Error(format!("Unknown operator: {:?} {:?} {:?}", l, operator, r)),
    }
}

fn eval_integer_infix_expression(
    operator: crate::token::Token,
    left: i64,
    right: i64,
) -> Object {
    match operator {
        crate::token::Token::Plus => Object::Integer(left + right),
        crate::token::Token::Minus => Object::Integer(left - right),
        crate::token::Token::Asterisk => Object::Integer(left * right),
        crate::token::Token::Slash => Object::Integer(left / right),
        crate::token::Token::Lt => native_bool_to_boolean_object(left < right),
        crate::token::Token::Gt => native_bool_to_boolean_object(left > right),
        crate::token::Token::Eq => native_bool_to_boolean_object(left == right),
        crate::token::Token::NotEq => native_bool_to_boolean_object(left != right),
        _ => Object::Error(format!("Unknown operator: {:?} {:?} {:?}", left, operator, right)),
    }
}

fn eval_boolean_infix_expression(
    operator: crate::token::Token,
    left: bool,
    right: bool,
) -> Object {
    match operator {
        crate::token::Token::Eq => native_bool_to_boolean_object(left == right),
        crate::token::Token::NotEq => native_bool_to_boolean_object(left != right),
        _ => Object::Error(format!("Unknown operator: {:?} {:?} {:?}", left, operator, right)),
    }
}

fn eval_identifier(ident: crate::ast::Identifier, env: Rc<RefCell<Environment>>) -> Object {
    if let Some(val) = env.borrow().get(&ident.value) {
        return val;
    }

    // Add built-in functions here later if needed
    // For now, let's just return an error for undeclared identifiers
    Object::Error(format!("Identifier not found: {}", ident.value))
}

fn eval_expressions(expressions: Vec<Expression>, env: Rc<RefCell<Environment>>) -> Vec<Object> {
    let mut result = Vec::new();
    for exp in expressions {
        let evaluated = eval_expression(exp, env.clone());
        if is_error(&evaluated) {
            return vec![evaluated];
        }
        result.push(evaluated);
    }
    result
}

fn apply_function(func: Object, args: Vec<Object>) -> Object {
    match func {
        Object::Function(function) => {
            let extended_env = Rc::new(RefCell::new(Environment::new_enclosed_environment(function.env.clone())));
            for (param_ident, arg_obj) in function.parameters.into_iter().zip(args.into_iter()) {
                extended_env.borrow_mut().set(param_ident.value, arg_obj);
            }
            let evaluated = eval_block_statement(function.body, extended_env.clone());
            unwrap_return_value(evaluated)
        }
        _ => Object::Error(format!("Not a function: {:?}", func)),
    }
}

fn unwrap_return_value(obj: Object) -> Object {
    if let Object::Return(value) = obj {
        *value
    } else {
        obj
    }
}

fn native_bool_to_boolean_object(input: bool) -> Object {
    if input {
        TRUE
    } else {
        FALSE
    }
}

fn is_error(obj: &Object) -> bool {
    matches!(obj, Object::Error(_))
}