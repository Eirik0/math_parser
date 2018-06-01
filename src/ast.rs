#[derive(Debug, PartialEq)]
pub enum Expr {
    Integer(i64),
    Bop(Box<Expr>, Operator, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

pub fn get_operator(c: char) -> Operator {
    match c {
        '+' => Operator::Add,
        '-' => Operator::Sub,
        '*' => Operator::Mul,
        '/' => Operator::Div,
        _ => panic!(format!("Unexpected operator: {}", c)),
    }
}
