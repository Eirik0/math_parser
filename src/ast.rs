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
    Exp,
}

pub fn get_operator(c: char) -> Operator {
    match c {
        '+' => Operator::Add,
        '-' => Operator::Sub,
        '*' => Operator::Mul,
        '/' => Operator::Div,
        '^' => Operator::Exp,
        _ => panic!(format!("Unexpected operator: {}", c)),
    }
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
        Expr::Bop(box r, op, box l) => format!(
            "({}{}{})",
            expr_to_string(r),
            get_operator_char(op),
            expr_to_string(l)
        ),
    }
}
