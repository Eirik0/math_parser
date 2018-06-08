use std::str::Chars;

const FUNCTION_NAMES: [&str; 3] = ["cos", "sin", "tan"];

#[derive(Debug, PartialEq)]
pub enum Token {
    Number(String),
    Variable(String),
    Function(String),
    Operator(char),
}

pub fn tokenize(s: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();

    let remainder = &mut s.chars();
    let mut next = next_non_whitespace(remainder);

    while next != None {
        next = match next {
            Some(next) if next.is_digit(10) => {
                let (num, next) = next_number(next, remainder);
                tokens.push(num);
                next
            }
            Some(next) if is_valid_operator(next) => {
                tokens.push(Token::Operator(next));
                next_non_whitespace(remainder)
            }
            Some(next) if next.is_alphabetic() => {
                let (mut var_tokens, next) = next_variables(next, remainder);
                tokens.append(&mut var_tokens);
                next
            }
            Some(next) => {
                return Err(format!("Unexpected character: {}", next));
            }
            None => None,
        }
    }

    Ok(tokens)
}

fn is_valid_operator(c: char) -> bool {
    match c {
        '+' | '-' | '*' | '/' | '^' | '(' | ')' => true,
        _ => false,
    }
}

fn next_non_whitespace(remainder: &mut Chars) -> Option<char> {
    let mut next = remainder.next();
    while next.is_some() && next.unwrap().is_whitespace() {
        next = remainder.next();
    }
    next
}

fn this_or_next(this: Option<char>, remainder: &mut Chars) -> Option<char> {
    match this {
        Some(c) if c.is_whitespace() => next_non_whitespace(remainder),
        _ => this,
    }
}

fn next_string(
    start: char,
    remainder: &mut Chars,
    accept_char: &Fn(char) -> bool,
) -> (String, Option<char>) {
    let mut s = String::from(start.to_string());
    let mut next = remainder.next();

    while next.is_some() && accept_char(next.unwrap()) {
        s.push(next.unwrap());
        next = remainder.next();
    }

    (s, next)
}

fn next_number(start: char, remainder: &mut Chars) -> (Token, Option<char>) {
    let (num, next) = next_string(start, remainder, &|c| c.is_digit(10));

    // TODO if next == Some('.') { -> Decimal

    (Token::Number(num), this_or_next(next, remainder))
}

fn next_variables(start: char, remainder: &mut Chars) -> (Vec<Token>, Option<char>) {
    let (mut chars, next) = next_string(start, remainder, &|c| c.is_alphabetic());

    let mut tokens = Vec::new();

    while chars.len() > 0 {
        chars = if let Some((fn_name, remainder)) = next_function(&chars) {
            tokens.push(Token::Function(fn_name.to_string()));
            remainder.to_string()
        } else {
            let (c, remainder) = chars.split_at(1);
            tokens.push(Token::Variable(c.to_string()));
            remainder.to_string()
        }
    }

    (tokens, this_or_next(next, remainder))
}

fn next_function<'a>(chars: &'a str) -> Option<(&'a str, &'a str)> {
    for fn_name in FUNCTION_NAMES.iter() {
        if chars.starts_with(fn_name) {
            return Some(chars.split_at(fn_name.len()));
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    fn num(i: u64) -> Token {
        Token::Number(i.to_string())
    }

    fn op(c: char) -> Token {
        Token::Operator(c)
    }

    fn var(c: char) -> Token {
        Token::Variable(c.to_string())
    }

    fn f(s: &str) -> Token {
        Token::Function(s.to_string())
    }

    #[test]
    fn get_next_non_whitespace() {
        let chars = &mut "   abc".chars();
        assert_eq!(Some('a'), next_non_whitespace(chars));
    }

    #[test]
    fn tokenize_number() {
        assert_eq!(Ok(vec![num(123)]), tokenize("123"));
    }

    #[test]
    fn tokenize_operator() {
        assert_eq!(Ok(vec![num(1), op('+'), num(2)]), tokenize("1+2"));
    }

    #[test]
    fn tokenize_variables() {
        assert_eq!(Ok(vec![var('x'), var('y')]), tokenize("xy"));
    }

    #[test]
    fn tokenize_function() {
        assert_eq!(Ok(vec![f("sin")]), tokenize("sin"));
        assert_eq!(Ok(vec![var('x'), var('y'), f("sin")]), tokenize("xysin"));
    }
}
