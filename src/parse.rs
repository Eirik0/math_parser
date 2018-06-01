use std::iter::Peekable;
use std::str::Chars;

use ast::Expr::*;
use ast::*;

use self::ParseResult::*;

#[derive(Debug, PartialEq)]
pub enum ParseResult {
    Success(Expr, Option<char>),
    Failure(String),
}

// expr := term [{"+"|"-" sum}]
// term := factor [{"*"|"/" factor}]
// factor := number | "(" expr ")"
// number := digit [{digit}]
// digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

pub fn parse(s: &str) -> Result<Expr, String> {
    match parse_expr(&mut s.chars().peekable()) {
        Success(e, None) => Ok(e),
        Success(_, Some(c)) => Err(format!("Unexpected character: {}", c)),
        Failure(m) => Err(m),
    }
}

fn parse_expr(remainder: &mut Peekable<Chars>) -> ParseResult {
    match parse_term(remainder) {
        Success(lhs, next) => parse_operator(lhs, next, remainder, &vec!['+', '-']),
        Failure(m) => Failure(m),
    }
}

fn parse_term(remainder: &mut Peekable<Chars>) -> ParseResult {
    match parse_factor(remainder) {
        Success(lhs, next) => parse_operator(lhs, next, remainder, &vec!['*', '/']),
        Failure(m) => Failure(m),
    }
}

fn parse_operator(
    lhs: Expr,
    op: Option<char>,
    remainder: &mut Peekable<Chars>,
    operators: &Vec<char>,
) -> ParseResult {
    match op {
        None => Success(lhs, None),
        Some(op) if operators.contains(&op) => match parse_term(remainder) {
            Success(rhs, next) => parse_operator(
                Bop(Box::new(lhs), get_operator(op), Box::new(rhs)),
                next,
                remainder,
                operators,
            ),
            Failure(m) => Failure(m),
        },
        next => Success(lhs, next),
    }
}

fn parse_factor(remainder: &mut Peekable<Chars>) -> ParseResult {
    skip_whitespace(remainder);
    match remainder.next() {
        Some(c) if c.is_digit(10) => parse_number(c, remainder),
        Some('(') => parse_paren(remainder),
        c => Failure(format!("Expected factor, but found: {:?}", c)),
    }
}

fn parse_number(start: char, remainder: &mut Peekable<Chars>) -> ParseResult {
    let mut i = String::from(start.to_string());
    while remainder.peek().is_some() && remainder.peek().unwrap().is_digit(10) {
        i.push(remainder.next().unwrap());
    }
    skip_whitespace(remainder);
    Success(Integer(i.parse().unwrap()), remainder.next())
}

fn parse_paren(remainder: &mut Peekable<Chars>) -> ParseResult {
    match parse_expr(remainder) {
        Success(expr, Some(')')) => {
            skip_whitespace(remainder);
            Success(expr, remainder.next())
        }
        Success(_, c) => Failure(format!("Expected ), but found {:?}", c)),
        Failure(m) => Failure(m),
    }
}

fn skip_whitespace(chars: &mut Peekable<Chars>) {
    while chars.peek() == Some(&' ') {
        chars.next();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use ast::Operator::*;

    fn integer(i: i64) -> Box<Expr> {
        Box::new(Integer(i))
    }

    fn bop(lhs: Box<Expr>, op: Operator, rhs: Box<Expr>) -> Box<Expr> {
        Box::new(Bop(lhs, op, rhs))
    }

    #[test]
    fn whitespace_skipped() {
        let chars = &mut "   abc".chars().peekable();
        skip_whitespace(chars);
        assert_eq!(Some('a'), chars.next())
    }

    #[test]
    fn integers_parsed() {
        assert_eq!(Ok(Integer(1)), parse("1"));
        assert_eq!(Ok(Integer(1)), parse(" 1"));
        assert_eq!(Ok(Integer(12)), parse("12"));
    }

    #[test]
    fn add_parsed() {
        assert_eq!(Ok(Bop(integer(1), Add, integer(2))), parse("1+2"));
        assert_eq!(Ok(Bop(integer(1), Add, integer(2))), parse("  1  +  2"));
    }

    #[test]
    fn add_mul_parsed() {
        assert_eq!(
            Ok(Bop(integer(1), Add, bop(integer(2), Mul, integer(3)))),
            parse("1+2*3")
        );
        assert_eq!(
            Ok(Bop(bop(integer(1), Mul, integer(2)), Add, integer(3))),
            parse("1*2+3")
        );
        assert_eq!(
            Ok(Bop(
                bop(
                    bop(integer(1), Mul, integer(2)),
                    Add,
                    bop(integer(3), Mul, integer(4))
                ),
                Add,
                integer(5)
            )),
            parse("1*2+3*4+5")
        );
    }

    #[test]
    fn parentheses_parsed() {
        assert_eq!(Ok(Integer(1)), parse("(1)"));
        assert_eq!(Ok(Integer(1)), parse("((1))"));
        assert_eq!(Ok(Bop(integer(1), Add, integer(2))), parse("(1+2)"));
        assert_eq!(
            Ok(Bop(bop(integer(1), Add, integer(2)), Mul, integer(3))),
            parse("(1+2)*3")
        );
    }
}
