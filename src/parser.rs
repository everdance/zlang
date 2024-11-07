use std::{iter::Peekable, str::CharIndices, usize};

use crate::token::{TokenKind::*, *};

pub fn parse(s: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut iter = s.char_indices().peekable();
    let mut line: usize = 0;

    loop {
        match iter.next() {
            Some((pos, c)) => match c {
                '\n' => line += 1,
                '\t' | '\r' | ' ' => (),
                _ => match get_token(c, &mut iter) {
                    Ok(kind) => tokens.push(Token { kind, pos, line }),
                    Err(err) => panic!("{err} - line: {line} offset: {pos}"),
                },
            },
            _ => break,
        }
    }

    tokens
}

fn get_token(c: char, iter: &mut Peekable<CharIndices<'_>>) -> Result<TokenKind, String> {
    match c {
        '{' => Ok(LeftBrace),
        '}' => Ok(RightBrace),
        '(' => Ok(LeftParen),
        ')' => Ok(RightParen),
        ',' => Ok(Comma),
        '.' => Ok(Dot),
        '-' => Ok(Minus),
        '+' => Ok(Plus),
        ';' => Ok(Semicolon),
        '/' => slash(iter),
        '*' => Ok(Star),
        '!' | '=' | '>' | '<' => Ok(equal_token(c, iter)),
        '"' => get_string(iter),
        '0'..='9' => get_number(c, iter),
        'A'..='Z' | 'a'..='z' => get_identifer(c, iter),

        _ => Err(format!("unexpected char:'{}'", c)),
    }
}

fn get_string(iter: &mut Peekable<CharIndices<'_>>) -> Result<TokenKind, String> {
    let mut s = vec![];

    loop {
        if let Some((_, c)) = iter.next() {
            if c == '"' && *s.last().unwrap_or(&'\0') != BACKSLASH {
                break;
            }
            s.push(c);
        } else {
            let str = String::from_iter(s);
            return Err(format!("unterminated string '{str}'"));
        }
    }

    Ok(StrLiteral(String::from_iter(s)))
}

fn slash(iter: &mut Peekable<CharIndices<'_>>) -> Result<TokenKind, String> {
    match iter.peek() {
        Some((_, c)) => {
            // comments
            if *c == '/' {
                loop {
                    if let Some((_, c)) = iter.next() {
                        if c == '\n' {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                return Ok(Comment);
            }
            Ok(Slash)
        }
        None => Err(format!("unexpected '\' at the end")),
    }
}

fn get_number(start: char, iter: &mut Peekable<CharIndices<'_>>) -> Result<TokenKind, String> {
    let mut s = vec![start];

    loop {
        if let Some((_, c)) = iter.peek() {
            match *c {
                '0'..='9' | '.' => {
                    let (_, ch) = iter.next().unwrap();
                    s.push(ch);
                }
                '\t' | ' ' | '%' | '&' | ':'..='?' | '('..='/' => break,
                _ => {
                    let num = String::from_iter(s);
                    return Err(format!("unexpected char '{c}' after '{num}'"));
                }
            }
        } else {
            break;
        }
    }

    match String::from_iter(s).parse::<f64>() {
        Ok(num) => Ok(NumLiteral(num)),
        Err(err) => Err(err.to_string()),
    }
}

fn get_identifer(start: char, iter: &mut Peekable<CharIndices<'_>>) -> Result<TokenKind, String> {
    let mut s = vec![start];

    loop {
        if let Some((_, c)) = iter.peek() {
            if c.is_ascii_alphanumeric() || *c == '_' {
                let (_, ch) = iter.next().unwrap();
                s.push(ch);
            } else if c.is_ascii_whitespace() {
                break;
            } else if !c.is_ascii() {
                let prefix = String::from_iter(s);
                return Err(format!("unexpected char '{c}' after '{prefix}'"));
            }
        } else {
            break;
        }
    }

    let word = String::from_iter(s);

    let kind = match word.as_str() {
        "and" => And,
        "class" => Class,
        "else" => Else,
        "false" => False,
        "true" => True,
        "fun" => Fun,
        "for" => For,
        "if" => If,
        "nil" => Nil,
        "or" => Or,
        "return" => Return,
        "super" => Super,
        "this" => This,
        "var" => Var,
        "while" => While,
        _ => Identifier(word),
    };

    Ok(kind)
}

fn equal_token(c: char, iter: &mut Peekable<CharIndices<'_>>) -> TokenKind {
    let mut next_equal = false;

    if let Some((_, next)) = iter.peek() {
        if *next == '=' {
            iter.next();
            next_equal = true;
        }
    }

    match c {
        '!' => {
            if next_equal {
                BangEqual
            } else {
                Bang
            }
        }
        '=' => {
            if next_equal {
                DoubleEqual
            } else {
                Equal
            }
        }
        '>' => {
            if next_equal {
                GreaterEqual
            } else {
                Greater
            }
        }
        _ => {
            if next_equal {
                LessEqual
            } else {
                Less
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic(expected = "unexpected char 'x' after '3.14' - line: 0 offset: 0")]
    fn invalid_number() {
        parse("3.14xz");
    }

    #[test]
    #[should_panic(expected = "unexpected char '三' after 'count' - line: 0 offset: 0")]
    fn invalid_identifer() {
        parse("count三十");
    }

    #[test]
    #[should_panic(expected = "unterminated string 'this is a good day' - line: 0 offset: 0")]
    fn unterminated_string() {
        parse("\"this is a good day");
    }

    #[test]
    fn scan() {
        let s = "var x = 1";
        let tokens = parse(s);
        assert_eq!(tokens.len(), 4);
        assert_eq!(
            tokens[0],
            Token {
                kind: Var,
                pos: 0,
                line: 0
            }
        );
        assert_eq!(
            tokens[1],
            Token {
                kind: Identifier("x".to_string()),
                pos: 4,
                line: 0
            }
        );
        assert_eq!(
            tokens[2],
            Token {
                kind: Equal,
                pos: 6,
                line: 0
            }
        );
        assert_eq!(
            tokens[3],
            Token {
                kind: NumLiteral(1.0),
                pos: 8,
                line: 0
            }
        );
    }
}
