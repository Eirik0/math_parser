use ast::Operator::*;
use ast::*;

pub fn eval(e: Expr) -> Result<Type, String> {
    match e {
        Expr::Integer(i) => Ok(Type::Integer(i)),
        Expr::Bop(box lhs, op, box rhs) => {
            let lhs = eval(lhs)?;
            let rhs = eval(rhs)?;
            match op {
                Add => Ok(add(lhs, rhs)),
                Sub => Ok(subtract(lhs, rhs)),
                Mul => Ok(multiply(lhs, rhs)),
                Div => Ok(divide(lhs, rhs)),
                _ => Err("not implemented".to_string()),
                //Exp => Ok(exponentiate(lhs, rhs)),
            }
        }
        Expr::Neg(box expr) => Ok(negate(eval(expr)?)),
    }
}

pub fn add(lhs: Type, rhs: Type) -> Type {
    match (lhs, rhs) {
        (Type::Integer(i1), Type::Integer(i2)) => Type::Integer(i1 + i2),
        (Type::Integer(i), Type::Rational(n, d)) => Type::Rational(i * d + n, d),
        (Type::Rational(n, d), Type::Integer(i)) => Type::Rational(n + i * d, d),
        (Type::Rational(n1, d1), Type::Rational(n2, d2)) => {
            Type::Rational(n1 * d2 + n2 * d1, d1 * d2)
        }
    }
}

pub fn subtract(lhs: Type, rhs: Type) -> Type {
    match (lhs, rhs) {
        (Type::Integer(i1), Type::Integer(i2)) => Type::Integer(i1 - i2),
        (Type::Integer(i), Type::Rational(n, d)) => Type::Rational(i * d - n, d),
        (Type::Rational(n, d), Type::Integer(i)) => Type::Rational(n - i * d, d),
        (Type::Rational(n1, d1), Type::Rational(n2, d2)) => {
            Type::Rational(n1 * d2 - n2 * d1, d1 * d2)
        }
    }
}

pub fn multiply(lhs: Type, rhs: Type) -> Type {
    match (lhs, rhs) {
        (Type::Integer(i1), Type::Integer(i2)) => Type::Integer(i1 * i2),
        (Type::Integer(i), Type::Rational(n, d)) => Type::Rational(i * n, d),
        (Type::Rational(n, d), Type::Integer(i)) => Type::Rational(n * i, d),
        (Type::Rational(n1, d1), Type::Rational(n2, d2)) => Type::Rational(n1 * n2, d1 * d2),
    }
}

pub fn divide(lhs: Type, rhs: Type) -> Type {
    match (lhs, rhs) {
        (Type::Integer(i1), Type::Integer(i2)) => Type::Rational(i1, i2), // XXX divide by zero
        (Type::Integer(i), Type::Rational(n, d)) => Type::Rational(i * d, n),
        (Type::Rational(n, d), Type::Integer(i)) => Type::Rational(n, d * i),
        (Type::Rational(n1, d1), Type::Rational(n2, d2)) => Type::Rational(n1 * d2, d1 * n2),
    }
}

pub fn negate(expr: Type) -> Type {
    match expr {
        Type::Integer(i) => Type::Integer(-i),
        Type::Rational(n, d) => Type::Rational(-n, d),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parse::*;

    fn parse_and_eval(s: &str) -> Result<Type, String> {
        eval(parse(s)?)
    }

    #[test]
    fn one_evalss_to_itself() {
        assert_eq!(Ok(Type::Integer(1)), parse_and_eval("1"))
    }

    #[test]
    fn eval_bop_to_int() {
        assert_eq!(Ok(Type::Integer(2)), parse_and_eval("1+1"));
        assert_eq!(Ok(Type::Integer(0)), parse_and_eval("1-1"));
        assert_eq!(Ok(Type::Integer(6)), parse_and_eval("2*3"));
        //assert_eq!(Ok(Type::Integer(2)), parse_and_eval("4/2")); // XXX reduce
    }

    #[test]
    fn eval_negation() {
        assert_eq!(Ok(Type::Integer(-1)), parse_and_eval("-1"));
        assert_eq!(Ok(Type::Integer(-2)), parse_and_eval("-(1+1)"));
        assert_eq!(Ok(Type::Integer(1)), parse_and_eval("-(1+-2)"));
    }

    #[test]
    fn eval_nested() {
        assert_eq!(Ok(Type::Integer(7)), parse_and_eval("1+2*3"));
        assert_eq!(Ok(Type::Integer(19)), parse_and_eval("1*2+3*4+5"));
        assert_eq!(Ok(Type::Integer(25)), parse_and_eval("1*(2+3)*4+5"));
    }
    #[test]
    fn eval_add() {
        assert_eq!(Ok(Type::Integer(2)), parse_and_eval("1+1"));
        assert_eq!(Ok(Type::Integer(6)), parse_and_eval("1+2+3"));
        assert_eq!(Ok(Type::Rational(3, 2)), parse_and_eval("1+1/2"));
        assert_eq!(Ok(Type::Rational(3, 2)), parse_and_eval("1/2+1"));
        assert_eq!(Ok(Type::Rational(5, 6)), parse_and_eval("1/2+1/3"));
    }

    #[test]
    fn eval_subtract() {
        assert_eq!(Ok(Type::Integer(0)), parse_and_eval("1-1"));
        assert_eq!(Ok(Type::Integer(-4)), parse_and_eval("1-2-3"));
        assert_eq!(Ok(Type::Rational(1, 2)), parse_and_eval("1-1/2"));
        assert_eq!(Ok(Type::Rational(-1, 2)), parse_and_eval("1/2-1"));
        assert_eq!(Ok(Type::Rational(1, 6)), parse_and_eval("1/2-1/3"));
    }

    #[test]
    fn eval_divide() {
        //assert_eq!(Ok(Type::Integer(1)), parse_and_eval("1/1")); // XXX reduce
        assert_eq!(Ok(Type::Rational(1, 6)), parse_and_eval("1/2/3"));
        assert_eq!(Ok(Type::Rational(2, 1)), parse_and_eval("1/(1/2)"));
        assert_eq!(Ok(Type::Rational(1, 4)), parse_and_eval("(1/2)/2"));
        assert_eq!(Ok(Type::Rational(3, 2)), parse_and_eval("(1/2)/(1/3)"));
    }
}
