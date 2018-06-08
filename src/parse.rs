use ast::{Expr, Function, Operator};
use lex;
use lex::Token;

type ParseResult = Result<(Expr, Option<Token>), String>;
type TokenIter<'a> = &'a mut Iterator<Item = Token>;

// expr := expr ("+"|"-") term | term
// term := term ("*"|"/") neg | term right_factor | neg
// neg := "-" neg | factor
// factor := exponent "^" neg | exponent
// right_factor := right_exponent "^" neg | exponent
// exponent := number | variable | function | "(" expr ")"
// right_exponent := variable | function | "(" expr ")"
// number := integer
// variable := char
// function := function_name "(" expr ")"
// integer := integer digit
// char := "a" | ... | "z" | "A" | ...
// digit := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

pub fn parse(s: &str) -> Result<Expr, String> {
    let tokens = lex::tokenize(s)?;
    let (expr, next) = parse_expr(&mut tokens.into_iter())?;
    match next {
        None => Ok(expr),
        Some(t) => Err(format!("Unexpected trailing token: {:?}", t)),
    }
}

fn parse_expr(remainder: TokenIter) -> ParseResult {
    let (lhs, next) = parse_term(remainder)?;
    parse_exprs(lhs, next, remainder)
}

fn parse_exprs(lhs: Expr, next: Option<Token>, remainder: TokenIter) -> ParseResult {
    match next {
        Some(Token::Operator(op)) if op == '+' || op == '-' => {
            let (rhs, next) = parse_term(remainder)?;
            let bop = Expr::Bop(box lhs, Operator::new(op), box rhs);
            parse_exprs(bop, next, remainder)
        }
        next => Ok((lhs, next)),
    }
}

fn parse_term(remainder: TokenIter) -> ParseResult {
    let (lhs, next) = parse_neg(remainder)?;
    parse_terms(lhs, next, remainder)
}

fn parse_terms(lhs: Expr, next: Option<Token>, remainder: TokenIter) -> ParseResult {
    match next {
        Some(Token::Operator(op)) if op == '*' || op == '/' => {
            let (rhs, next) = parse_neg(remainder)?;
            let bop = Expr::Bop(box lhs, Operator::new(op), box rhs);
            parse_terms(bop, next, remainder)
        }
        Some(Token::Variable(_)) | Some(Token::Function(_)) | Some(Token::Operator('(')) => {
            let (rhs, next) = parse_factor(next, remainder)?;
            let bop = Expr::Bop(box lhs, Operator::Mul, box rhs);
            parse_terms(bop, next, remainder)
        }
        next => Ok((lhs, next)),
    }
}

fn parse_neg(remainder: TokenIter) -> ParseResult {
    match remainder.next() {
        Some(Token::Operator('-')) => {
            let (expr, next) = parse_neg(remainder)?;
            Ok((Expr::Neg(box expr), next))
        }
        next => parse_factor(next, remainder),
    }
}

fn parse_factor(next: Option<Token>, remainder: TokenIter) -> ParseResult {
    let (lhs, next) = parse_exponent(next, remainder)?;
    match next {
        Some(Token::Operator('^')) => {
            let (rhs, next) = parse_neg(remainder)?;
            let bop = Expr::Bop(box lhs, Operator::Exp, box rhs);
            Ok((bop, next))
        }
        next => Ok((lhs, next)),
    }
}

fn parse_exponent(next: Option<Token>, remainder: TokenIter) -> ParseResult {
    match next {
        Some(Token::Number(num)) => match num.parse() {
            Ok(i) => Ok((Expr::Integer(i), remainder.next())),
            Err(_) => Err(format!("Unable to parse number: {}", num)),
        },
        Some(Token::Variable(x)) => Ok((Expr::Var(x), remainder.next())),
        Some(Token::Function(function_name)) => match remainder.next() {
            Some(Token::Operator('(')) => {
                let (expr, next) = parse_paren(remainder)?;
                Ok((Expr::Function(Function::new(&function_name), box expr), next))
            }
            t => Err(format!("Expected ), but found {:?}", t)),
        } 
        Some(Token::Operator('(')) => parse_paren(remainder),
        t => Err(format!("Expected exponent, but found: {:?}", t)),
    }
}

fn parse_paren(remainder: TokenIter) -> ParseResult {
    let (expr, next) = parse_expr(remainder)?;
    match next {
        Some(Token::Operator(')')) => Ok((expr, remainder.next())),
        t => Err(format!("Expected ), but found {:?}", t)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use ast::Expr::*;
    use ast::Operator::*;
    use ast::Function::*;
    use ast::*;

    fn i(i: i64) -> Box<Expr> {
        box Integer(i)
    }

    fn v(c: char) -> Box<Expr> {
        box Var(c.to_string())
    }

    fn b(lhs: Box<Expr>, op: Operator, rhs: Box<Expr>) -> Box<Expr> {
        box Bop(lhs, op, rhs)
    }

    fn n(e: Box<Expr>) -> Box<Expr> {
        box Neg(e)
    }

    #[test]
    fn parse_integer() {
        assert_eq!(Ok(Integer(1)), parse("1"));
        assert_eq!(Ok(Integer(1)), parse(" 1"));
        assert_eq!(Ok(Integer(12)), parse("12"));
        assert_eq!(Ok(Integer(1234567890)), parse("1234567890"));
    }

    #[test]
    fn parse_add() {
        assert_eq!(Ok(Bop(i(1), Add, i(2))), parse("1+2"));
        assert_eq!(Ok(Bop(b(i(1), Add, i(2)), Add, i(3))), parse("1+2+3"));
        assert_eq!(Ok(Bop(i(1), Add, i(2))), parse("  1  +  2"));
    }

    #[test]
    fn parse_divide() {
        assert_eq!(Ok(Bop(b(i(1), Div, i(2)), Div, i(3))), parse("1/2/3"));
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
    fn parse_exp() {
        assert_eq!(
            Ok(Bop(i(1), Add, b(i(2), Mul, b(i(3), Exp, i(4))))),
            parse("1+2*3^4")
        );
        assert_eq!(
            Ok(Neg(b(i(1), Exp, n(b(i(2), Exp, n(i(3))))))),
            parse("-1^-2^-3")
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
    fn parse_order_of_operations() {
        assert_eq!("((1+2)+3)", expr_to_string(parse("1+2+3").unwrap()));
        assert_eq!("(1^(2^3))", expr_to_string(parse("1^2^3").unwrap()));
        assert_eq!("(1+(2*(3^4)))", expr_to_string(parse("1+2*3^4").unwrap()));
    }

    #[test]
    fn parse_vars() {
        assert_eq!(Ok(Bop(v('x'), Add, v('y'))), parse("x+y"));
        assert_eq!(Ok(Bop(v('x'), Mul, v('y'))), parse("xy"));
        assert_eq!(Ok(Bop(v('x'), Mul, v('y'))), parse("x(y)"));
        assert_eq!(Ok(Bop(v('x'), Mul, v('y'))), parse("(x)(y)"));
        assert_eq!(Ok(Bop(i(3), Mul, v('x'))), parse("3x"));
        assert_eq!(Ok(Bop(b(v('x'), Mul, v('y')), Mul, v('z'))), parse("xyz"));
    }

    #[test]
    fn parse_function() {
        assert_eq!(Ok(Function(Sin, v('x'))), parse("sin(x)"));
    }
}
