use std::str::Chars;

use ast::Expr::*;
use ast::*;

use self::ParseResult::*;

#[derive(Debug, PartialEq)]
enum ParseResult {
    Success(Expr, Option<char>),
    Failure(String),
}

// expr := term [{"+"|"-" sum}]
// term := neg [{"*"|"/" neg}]
// neg = factor | "-" neg
// factor := exponent [{"^" exponent}]
// exponent := number | "(" expr ")"
// number := digit [{digit}]
// digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

pub fn parse(s: &str) -> Result<Expr, String> {
    match parse_expr(&mut s.chars()) {
        Success(e, None) => Ok(e),
        Success(_, Some(c)) => Err(format!("Unexpected character: {}", c)),
        Failure(m) => Err(m),
    }
}

fn parse_expr(remainder: &mut Chars) -> ParseResult {
    match parse_term(remainder) {
        Success(lhs, next) => parse_bop(lhs, next, remainder, vec!['+', '-']),
        Failure(m) => Failure(m),
    }
}


fn parse_term(remainder: &mut Chars) -> ParseResult {
    match parse_neg(remainder) {
        Success(lhs, next) => parse_bop(lhs, next, remainder, vec!['*', '/']),
        Failure(m) => Failure(m),
    }
}

fn parse_neg(remainder: &mut Chars) -> ParseResult {
    match next_non_whitespace(remainder) {
        Some('-') => match parse_neg(remainder) {
            Success(e, next) => Success(Neg(box e), next),
            Failure(m) => Failure(m),
        },
        next => parse_factor(next, remainder),
    }
}

fn parse_factor(next: Option<char>, remainder: &mut Chars) -> ParseResult {
    match parse_exponent(next, remainder) {
        Success(lhs, next) => parse_bop(lhs, next, remainder, vec!['^']),
        Failure(m) => Failure(m),
    }
}

fn parse_bop(lhs: Expr, op: Option<char>, remainder: &mut Chars, ops: Vec<char>) -> ParseResult {
    match op {
        None => Success(lhs, None),
        Some(op) if ops.contains(&op) => match parse_term(remainder) {
            Success(rhs, next) => {
                let bop = Bop(box lhs, get_operator(op), box rhs);
                parse_bop(bop, next, remainder, ops)
            }
            Failure(m) => Failure(m),
        },
        next => Success(lhs, next),
    }
}

fn parse_exponent(next: Option<char>, remainder: &mut Chars) -> ParseResult {
    match next {
        Some(c) if c.is_digit(10) => parse_number(c, remainder),
        Some('(') => parse_paren(remainder),
        c => Failure(format!("Expected exponent, but found: {:?}", c)),
    }
}

fn parse_number(start: char, remainder: &mut Chars) -> ParseResult {
    let mut i = String::from(start.to_string());
    let mut next = remainder.next();
    while next.is_some() && next.unwrap().is_digit(10) {
        i.push(next.unwrap());
        next = remainder.next();
    }
    if next.is_some() && next.unwrap().is_whitespace() {
        next = next_non_whitespace(remainder);
    }
    Success(Integer(i.parse().unwrap()), next)
}

fn parse_paren(remainder: &mut Chars) -> ParseResult {
    match parse_expr(remainder) {
        Success(expr, Some(')')) => Success(expr, next_non_whitespace(remainder)),
        Success(_, c) => Failure(format!("Expected ), but found {:?}", c)),
        Failure(m) => Failure(m),
    }
}

fn next_non_whitespace(remainder: &mut Chars) -> Option<char> {
    let mut next = remainder.next();
    while next.is_some() && next.unwrap().is_whitespace() {
        next = remainder.next();
    }
    next
}

#[cfg(test)]
mod tests {
    use super::*;

    use ast::Operator::*;

    fn i(i: i64) -> Box<Expr> {
        box Integer(i)
    }

    fn b(lhs: Box<Expr>, op: Operator, rhs: Box<Expr>) -> Box<Expr> {
        box Bop(lhs, op, rhs)
    }

    fn n(e: Box<Expr>) -> Box<Expr> {
        box Neg(e)
    }

    #[test]
    fn skip_whitespace() {
        let chars = &mut "   abc".chars();
        assert_eq!(Some('a'), next_non_whitespace(chars));
    }

    #[test]
    fn parse_integer() {
        assert_eq!(Ok(Integer(1)), parse("1"));
        assert_eq!(Ok(Integer(1)), parse(" 1"));
        assert_eq!(Ok(Integer(12)), parse("12"));
    }

    #[test]
    fn parse_add() {
        assert_eq!(Ok(Bop(i(1), Add, i(2))), parse("1+2"));
        assert_eq!(Ok(Bop(i(1), Add, i(2))), parse("  1  +  2"));
    }

    #[test]
    fn parse_add_mul() {
        assert_eq!(Ok(Bop(i(1), Add, b(i(2), Mul, i(3)))), parse("1+2*3"));
        assert_eq!(Ok(Bop(b(i(1), Mul, i(2)), Add, i(3))), parse("1*2+3"));
        assert_eq!(
            Ok(Bop(
                b(b(i(1), Mul, i(2)), Add, b(i(3), Mul, i(4))),
                Add,
                i(5)
            )),
            parse("1*2+3*4+5")
        );
    }
    #[test]
    fn parse_pow() {
        assert_eq!(
            Ok(Bop(i(1), Add, b(i(2), Mul, b(i(3), Exp, i(4))))),
            parse("1+2*3^4")
        );
    }

    #[test]
    fn parse_parentheses() {
        assert_eq!(Ok(Integer(1)), parse("(1)"));
        assert_eq!(Ok(Integer(1)), parse("((1))"));
        assert_eq!(Ok(Bop(i(1), Add, i(2))), parse("(1+2)"));
        assert_eq!(Ok(Bop(b(i(1), Add, i(2)), Mul, i(3))), parse("(1+2)*3"));
    }

    #[test]
    fn parse_negation() {
        assert_eq!(Ok(Neg(i(1))), parse("-1"));
        assert_eq!(Ok(Neg(n(i(1)))), parse("--1"));
        assert_eq!(Ok(Bop(n(i(1)), Add, i(2))), parse("-1+2"));
        assert_eq!(Ok(Bop(n(i(1)), Mul, i(2))), parse("-1*2"));
        assert_eq!(Ok(Neg(b(i(1), Exp, i(2)))), parse("-1^2"));
    }

    #[test]
    fn parse_order_correct() {
        assert_eq!("((1+2)+3)", expr_to_string(parse("1+2+3").unwrap()));
        assert_eq!("(1+(2*(3^4)))", expr_to_string(parse("1+2*3^4").unwrap()));
    }
}
