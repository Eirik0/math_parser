#[derive(Debug, PartialEq)]
pub enum Expr {
    Integer(i64),
    Var(String),
    Function(Function, Box<Expr>),
    Bop(Box<Expr>, Operator, Box<Expr>),
    Neg(Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Exp,
}

impl Operator {
    pub fn new(op: char) -> Self {
        match op {
            '+' => Operator::Add,
            '-' => Operator::Sub,
            '*' => Operator::Mul,
            '/' => Operator::Div,
            '^' => Operator::Exp,
            _ => panic!(format!("Unknown operator: {}", op)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Function {
    Cos,
    Sin,
    Tan,
}

impl Function {
    pub fn new(function_name: &str) -> Self {
        match function_name {
            "cos" => Function::Cos,
            "sin" => Function::Sin,
            "tan" => Function::Tan,
            _ => panic!(format!("Unknown function: {}", function_name))
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Integer(i64),
    Rational(i64, i64),
}

fn get_operator_char(o: Operator) -> char {
    match o {
        Operator::Add => '+',
        Operator::Sub => '-',
        Operator::Mul => '*',
        Operator::Div => '/',
        Operator::Exp => '^',
    }
}

pub fn expr_to_string(e: Expr) -> String {
    match e {
        Expr::Integer(i) => i.to_string(),
        Expr::Var(c) => c.to_string(),
        Expr::Function(name, box e) => format!("{:?}({})", name, expr_to_string(e)),
        Expr::Bop(box r, op, box l) => format!(
            "({}{}{})",
            expr_to_string(r),
            get_operator_char(op),
            expr_to_string(l)
        ),
        Expr::Neg(box e) => format!("(-{})", expr_to_string(e)),
    }
}
