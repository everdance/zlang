use std::{iter::Peekable, str::CharIndices, usize};

use crate::token::{TokenKind::*, *};

pub struct Scanner;

impl Scanner {
    pub fn scan(s: &str) -> (Vec<Token>, Vec<Issue>) {
        let mut state = ScanState {
            iter: &mut s.char_indices().peekable(),
            tokens: vec![],
            issues: vec![],
            line: 0,
            offset: -1,
        };

        state.scan();
        (state.tokens, state.issues)
    }
}

// separator chars for number and identifer
fn is_sep_char(c: char) -> bool {
    match c {
        '\t' | ' ' | '\n' | '\r' | '%' | '&' | '|' | ':'..='?' | '('..='/' => true,
        _ => false,
    }
}

#[derive(Debug, PartialEq)]
pub struct Issue {
    pub line: usize,
    pub pos: usize,
    pub message: String,
}

struct ScanState<'a> {
    iter: &'a mut Peekable<CharIndices<'a>>,
    tokens: Vec<Token>,
    issues: Vec<Issue>,
    line: usize,
    offset: i32,
}

impl ScanState<'_> {
    fn scan(&mut self) {
        while let Some(c) = self.next() {
            if c == '\n' {
                self.line += 1;
                self.offset = -1;
            }

            if c.is_ascii_whitespace() {
                continue;
            }

            self.get_token(c);
        }
    }

    fn get_token(&mut self, c: char) {
        let pos = self.offset;
        let result = match c {
            '{' => Ok(LeftBrace),
            '}' => Ok(RightBrace),
            '(' => Ok(LeftParen),
            ')' => Ok(RightParen),
            ',' => Ok(Comma),
            '.' => Ok(Dot),
            '-' => Ok(Minus),
            '+' => Ok(Plus),
            ';' => Ok(Semicolon),
            '*' => Ok(Star),
            '!' | '=' | '>' | '<' => Ok(self.equal_token(c)),
            '/' => self.slash(),
            '"' => self.get_string(),
            '0'..='9' => self.get_number(c),
            'A'..='Z' | 'a'..='z' => self.get_identifer(c),

            _ => Err(format!("unexpected char:'{}'", c)),
        };

        match result {
            Ok(kind) => self.tokens.push(Token {
                kind,
                pos: pos as usize,
                line: self.line,
            }),
            Err(err) => self.issues.push(Issue {
                line: self.line,
                pos: pos as usize,
                message: err,
            }),
        }
    }

    fn get_string(&mut self) -> Result<TokenKind, String> {
        let mut s = vec![];

        while let Some(c) = self.next() {
            if c == '"' && *s.last().unwrap_or(&'\0') != '\\' {
                return Ok(StrLiteral(String::from_iter(s)));
            }
            s.push(c);
        }

        let str = String::from_iter(s);
        Err(format!("unterminated string '{str}'"))
    }

    fn slash(&mut self) -> Result<TokenKind, String> {
        match self.peek() {
            Some(c) => {
                // comments
                if c == '/' {
                    while let Some(c) = self.peek() {
                        if c == '\n' {
                            break;
                        }
                        self.next();
                    }
                    return Ok(Comment);
                }
                Ok(Slash)
            }
            None => Err(format!("unexpected '/' at the end")),
        }
    }

    fn get_number(&mut self, start: char) -> Result<TokenKind, String> {
        let mut number_str = vec![start];
        let mut invalid_chars: Vec<char> = vec![]; // invalid suffix chars
        let mut invalid_pos = 0;

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.next();
                number_str.push(c);
            } else if c == '.' {
                if number_str.contains(&c) {
                    invalid_pos = self.offset + 1;
                    invalid_chars.push(c);
                    break;
                }
                self.next();
                number_str.push(c);
            } else if is_sep_char(c) {
                break;
            } else {
                invalid_chars.push(self.next().unwrap());
                invalid_pos = self.offset;
                break;
            }
        }

        if invalid_chars.len() > 0 {
            while let Some(c) = self.peek() {
                if c.is_ascii_whitespace() {
                    break;
                }
                invalid_chars.push(self.next().unwrap());
            }

            let num = String::from_iter(number_str.clone());
            let chars = String::from_iter(invalid_chars);
            self.issues.push(Issue {
                message: format!("unexpected chars '{chars}' after '{num}'"),
                line: self.line,
                pos: invalid_pos,
            });
        }

        match String::from_iter(number_str).parse::<f64>() {
            Ok(num) => Ok(NumLiteral(num)),
            Err(err) => Err(err.to_string()),
        }
    }

    fn get_identifer(&mut self, start: char) -> Result<TokenKind, String> {
        let mut s = vec![start];
        let mut invalid_chars: Vec<char> = vec![]; // invalid suffix chars
        let mut invalid_pos = 0;

        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == '_' {
                s.push(self.next().unwrap());
            } else if is_sep_char(c) {
                break;
            } else {
                invalid_chars.push(self.next().unwrap());
                invalid_pos = self.offset;
                break;
            }
        }

        if invalid_chars.len() > 0 {
            while let Some(c) = self.peek() {
                if c.is_ascii_whitespace() {
                    break;
                }
                invalid_chars.push(self.next().unwrap());
            }

            let id = String::from_iter(s.clone());
            let chars = String::from_iter(invalid_chars);
            self.issues.push(Issue {
                message: format!("unexpected chars '{chars}' after '{id}'"),
                line: self.line,
                pos: invalid_pos,
            });
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

    fn equal_token(&mut self, c: char) -> TokenKind {
        let mut next_equal = false;

        if let Some(next) = self.peek() {
            if next == '=' {
                self.next();
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

    fn next(&mut self) -> Option<char> {
        match self.iter.next() {
            Some((_, c)) => {
                self.offset += 1;
                Some(c)
            }
            _ => None,
        }
    }

    fn peek(&mut self) -> Option<char> {
        match self.iter.peek() {
            Some((_, c)) => Some(*c),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn invalid_number() {
        let (tokens, issues) = Scanner::scan("3.14xy.454");
        assert_eq!(tokens.len(), 1);
        assert_eq!(
            tokens[0],
            Token {
                kind: NumLiteral(3.14),
                pos: 1,
                line: 0,
            }
        );
        assert_eq!(issues.len(), 1);
        assert_eq!(
            issues[0],
            Issue {
                line: 0,
                pos: 5,
                message: "unexpected chars 'xy.454' after '3.14'".to_string(),
            }
        );
    }

    #[test]
    fn invalid_identifer() {
        let (tokens, issues) = Scanner::scan("count三十");
        assert_eq!(tokens.len(), 1);
        assert_eq!(issues.len(), 1);
        assert_eq!(
            issues[0],
            Issue {
                line: 0,
                pos: 6,
                message: "unexpected chars '三十' after 'count'".to_string(),
            }
        );
    }

    #[test]
    fn unterminated_string() {
        let (tokens, issues) = Scanner::scan("\"this is a good day");
        assert_eq!(tokens.len(), 0);
        assert_eq!(issues.len(), 1);
        assert_eq!(
            issues[0],
            Issue {
                line: 0,
                pos: 1,
                message: "unterminated string 'this is a good day'".to_string(),
            }
        );
    }

    #[test]
    fn scan() {
        let s = "var x = 1";
        let (tokens, issues) = Scanner::scan(s);
        assert_eq!(issues.len(), 0);
        assert_eq!(tokens.len(), 4);
        assert_eq!(
            tokens[0],
            Token {
                kind: Var,
                pos: 1,
                line: 0
            }
        );
        assert_eq!(
            tokens[1],
            Token {
                kind: Identifier("x".to_string()),
                pos: 5,
                line: 0
            }
        );
        assert_eq!(
            tokens[2],
            Token {
                kind: Equal,
                pos: 7,
                line: 0
            }
        );
        assert_eq!(
            tokens[3],
            Token {
                kind: NumLiteral(1.0),
                pos: 9,
                line: 0
            }
        );
    }
}
