use std::{iter::Peekable, str::CharIndices, usize};

use crate::token::{TokenKind::*, *};

pub fn parse(s: &String) -> Result<Vec<Token>, String> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut iter = s.char_indices().peekable();
    let mut line: usize = 0;

    loop {
        match iter.next() {
            Some((pos, c)) => match c {
                '\n' => line += 1,
                '\t' | ' ' => (),
                _ => {
                    if let Some(kind) = get_token(c, &mut iter) {
                        tokens.push(Token { kind, pos, line })
                    }
                }
            },
            _ => break,
        }
    }

    Ok(tokens)
}

fn get_token(c: char, iter: &mut Peekable<CharIndices<'_>>) -> Option<TokenKind> {
    match c {
        '{' => Some(LeftBrace),
        '}' => Some(RightBrace),
        '(' => Some(LeftParen),
        ')' => Some(RightParen),
        ',' => Some(Comma),
        '.' => Some(Dot),
        '-' => Some(Minus),
        '+' => Some(Plus),
        ';' => Some(Semicolon),
        '/' => Some(Slash), // TODO: support comments
        '*' => Some(Star),
        '!' | '=' | '>' | '<' => Some(equal_token(c, iter)),
        '"' => Some(get_string(iter)),
        '0'..='9' => Some(get_number(iter)),
        'A'..='Z' | 'a'..='z' => Some(get_word(c, iter)),

        _ => None,
    }
}

fn get_string(iter: &mut Peekable<CharIndices<'_>>) -> TokenKind {
    StrLiteral("d".to_string())
}

fn get_number(iter: &mut Peekable<CharIndices<'_>>) -> TokenKind {
    NumLiteral(12.98)
}

fn get_word(start: char, iter: &mut Peekable<CharIndices<'_>>) -> TokenKind {
    let mut s = vec![start];

    loop {
        if let Some((_, c)) = iter.next() {
            if !c.is_alphanumeric() {
                break;
            }
            s.push(c);
        } else {
            break;
        }
    }

    let word = String::from_iter(s);

    match word.as_str() {
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
    }
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
