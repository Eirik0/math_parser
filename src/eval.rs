use ast::Expr::*;
use ast::Operator::*;
use ast::*;

pub fn reduce(e: Expr) -> Expr {
    match e {
        Integer(_) => e,
        Bop(box Integer(i1), op, box Integer(i2)) => match op {
            Add => Integer(i1 + i2),
            Sub => Integer(i1 - i2),
            Mul => Integer(i1 * i2),
            Div => Integer(i1 / i2),
            Exp if i2 > 0 => Integer(i1.pow(i2 as u32)),
            _ => panic!("Not supported"),
        },
        Bop(box Integer(i1), op, box l) => reduce(Bop(box Integer(i1), op, box reduce(l))),
        Bop(box r, op, box l) => reduce(Bop(box reduce(r), op, box l)),
        Neg(box Integer(i)) => Integer(-i),
        Neg(box e) => reduce(Neg(box reduce(e))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use parse::*;

    fn parse_and_reduce(s: &str) -> Option<Expr> {
        match parse(s) {
            Ok(e) => Some(reduce(e)),
            Err(_) => None,
        }
    }

    #[test]
    fn one_reduces_to_itself() {
        assert_eq!(Some(Integer(1)), parse_and_reduce("1"))
    }

    #[test]
    fn reduce_bop_to_int() {
        assert_eq!(Some(Integer(2)), parse_and_reduce("1+1"));
        assert_eq!(Some(Integer(0)), parse_and_reduce("1-1"));
        assert_eq!(Some(Integer(6)), parse_and_reduce("2*3"));
        assert_eq!(Some(Integer(2)), parse_and_reduce("4/2"));
    }

    #[test]
    fn reduce_negation() {
        assert_eq!(Some(Integer(-1)), parse_and_reduce("-1"));
        assert_eq!(Some(Integer(-2)), parse_and_reduce("-(1+1)"));
        assert_eq!(Some(Integer(1)), parse_and_reduce("-(1+-2)"));
    }

    #[test]
    fn reduce_nested() {
        assert_eq!(Some(Integer(7)), parse_and_reduce("1+2*3"));
        assert_eq!(Some(Integer(19)), parse_and_reduce("1*2+3*4+5"));
        assert_eq!(Some(Integer(25)), parse_and_reduce("1*(2+3)*4+5"));
    }
}
