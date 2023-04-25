use super::token::Token;
use crate::parser::lexer::LexerError::InvalidToken;
use std::fmt::{Display, Formatter};

pub struct Lexer<'a> {
    input: &'a str,
    // position: usize,
    read_position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            // position: 0,
            read_position: 0,
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input.chars().nth(self.read_position)
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.peek_char();
        self.read_position += 1;
        ch
    }

    fn skip_whitespace(&mut self) {
        loop {
            let Some(ch) = self.peek_char() else { break };

            if is_whitespace(ch) {
                self.next_char();
            } else {
                return;
            }
        }
    }

    fn consume_while<F>(&mut self, pred: F) -> Option<&'a str>
    where
        F: Fn(char) -> bool,
    {
        let Some(ch) = self.peek_char() else { return None };
        if !pred(ch) {
            return None;
        }

        let read_position = self.read_position;

        loop {
            let Some(ch) = self.peek_char() else { break };

            if pred(ch) {
                self.next_char();
            } else {
                break;
            }
        }

        Some(&self.input[read_position..self.read_position])
    }

    fn read_string_lit(&mut self) -> String {
        let read_position = self.read_position;

        loop {
            let ch = self.next_char().expect("Expected a closing str literal");

            if ch == '"' {
                break;
            }
        }

        let str = &self.input[read_position..self.read_position - 1];
        str.to_string()
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();

        if let Some(tk) = self.try_consume_many(&[
            ("..", Token::Dots),
            (".", Token::Dot),
            ("<-", Token::ArrowLeft),
            ("as", Token::As),
            ("else", Token::Else),
            ("false", Token::False),
            ("fn", Token::Fn),
            ("if", Token::If),
            ("import", Token::Import),
            ("let", Token::Let),
            ("nil", Token::Nil),
            ("pub", Token::Pub),
            ("true", Token::True),
            ("use", Token::Use),
        ]) {
            return Ok(tk);
        };

        let Some(ch) = self.peek_char() else { return Ok(Token::Eof) };
        self.next_char();

        Ok(match ch {
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            '=' => match self.peek_char() {
                Some('=') => {
                    self.next_char();
                    Token::Eq
                }
                _ => Token::Assign,
            },
            ';' => Token::Semicolon,
            '+' => Token::Plus,
            '*' => Token::Mult,
            '<' => match self.peek_char() {
                Some('=') => {
                    self.next_char();
                    Token::LessEqual
                }
                _ => Token::Less,
            },
            '>' => match self.peek_char() {
                Some('=') => {
                    self.next_char();
                    Token::GreaterEqual
                }
                _ => Token::Greater,
            },
            '-' => Token::Minus,
            '!' => match self.peek_char() {
                Some('=') => {
                    self.next_char();
                    Token::NotEq
                }
                _ => Token::Bang,
            },
            ',' => Token::Comma,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '/' => match self.peek_char() {
                Some('/') => {
                    self.next_char();
                    self.consume_while(|c| c != '\n');
                    self.next_token()?
                }
                _ => Token::Slash,
            },
            '%' => Token::Percentage,
            '&' => match self.peek_char() {
                Some('&') => {
                    self.next_char();
                    Token::DoubleAnd
                }
                ch => return Err(InvalidToken(ch)),
            },
            '|' => match self.peek_char() {
                Some('|') => {
                    self.next_char();
                    Token::DoublePipe
                }

                Some('>') => {
                    self.next_char();
                    Token::PipeRight
                }
                ch => return Err(InvalidToken(ch)),
            },

            '"' => Token::String(self.read_string_lit()),

            _ => {
                self.read_position -= 1;

                let ch = self.peek_char().unwrap();

                if is_ident_starting_letter(ch) {
                    return Ok(self.expect_ident());
                }

                if is_ns_ident_starting_letter(ch) {
                    return Ok(self.expect_ns_ident());
                }

                if let Some(n) = self.consume_while(is_number) {
                    // TODO properly handle err
                    // TODO token parsing should belong to parser
                    return Ok(Token::Num(
                        n.parse().expect(format!("Invalid parse: `{n}`").as_str()),
                    ));
                }

                return Err(InvalidToken(self.peek_char()));
            }
        })
    }

    fn expect_ident(&mut self) -> Token {
        let ch = self.next_char().unwrap();
        let mut str = ch.to_string();
        if let Some(ident) = self.consume_while(is_ident_tail_letter) {
            str.push_str(ident);
        }
        Token::Ident(str.to_string())
    }

    fn expect_ns_ident(&mut self) -> Token {
        let ch = self.next_char().unwrap();
        let mut str = ch.to_string();
        if let Some(ident) = self.consume_while(is_ns_ident_tail_letter) {
            str.push_str(ident);
        }
        Token::NsIndent(str.to_string())
    }

    fn try_consume(&mut self, kw: &str) -> bool {
        let initial_position = self.read_position;
        for ch in kw.chars() {
            if self.peek_char() != Some(ch) {
                self.read_position = initial_position;
                return false;
            }

            self.next_char();
        }

        true
    }

    fn try_consume_many(&mut self, pairs: &[(&str, Token)]) -> Option<Token> {
        for (kw, tk) in pairs {
            if self.try_consume(*kw) {
                return Some(tk.clone());
            }
        }

        None
    }
}

fn is_ident_starting_letter(ch: char) -> bool {
    matches!(ch, '_' | 'a'..='z')
}

fn is_ident_tail_letter(ch: char) -> bool {
    matches!(ch, '_' | 'a'..='z' | '0' ..='9')
}

fn is_ns_ident_starting_letter(ch: char) -> bool {
    matches!(ch, 'A'..='Z')
}

fn is_ns_ident_tail_letter(ch: char) -> bool {
    matches!(ch, 'A'..='Z' | 'a'..='z')
}

fn is_whitespace(ch: char) -> bool {
    match ch {
        ' ' | '\t' | '\n' | '\r' => true,
        _ => false,
    }
}

fn is_number(ch: char) -> bool {
    match ch {
        '0'..='9' => true,
        _ => false,
    }
}

#[derive(Debug)]
pub enum LexerError {
    InvalidToken(Option<char>),
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InvalidToken(ch) => write!(
                f,
                "Invalid token (encountered {})",
                match ch {
                    Some(ch) => ch.to_string(),
                    None => "EOF".to_string(),
                }
            ),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(Token::Eof) => None,
            Ok(tk) => Some(Ok(tk)),
            Err(e) => Some(Err(e)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_ch() {
        assert_tokens("=", &[Token::Assign])
    }

    #[test]
    fn two_syms() {
        assert_tokens("+*", {
            use Token::*;
            &[Plus, Mult]
        });
    }

    #[test]
    fn whitespace() {
        assert_tokens("+ *", {
            use Token::*;
            &[Plus, Mult]
        });
    }

    #[test]
    fn literals() {
        assert_tokens("true false nil", {
            use Token::*;
            &[True, False, Nil]
        });
    }

    #[test]
    fn keywords() {
        assert_tokens("let use import pub as", {
            use Token::*;
            &[Let, Use, Import, Pub, As]
        });
    }

    #[test]
    fn operators() {
        assert_tokens("< > <= >= - ! != == + * / % && ||", {
            use Token::*;
            &[
                Less,
                Greater,
                LessEqual,
                GreaterEqual,
                Minus,
                Bang,
                NotEq,
                Eq,
                Plus,
                Mult,
                Slash,
                Percentage,
                DoubleAnd,
                DoublePipe,
            ]
        });
    }

    #[test]
    fn str_literal() {
        assert_tokens("\"\"", &[Token::String("".to_string())]);
        assert_tokens("\"abc\"", &[Token::String("abc".to_string())]);
    }

    #[test]
    fn ident() {
        assert_tokens("abc", &[Token::Ident("abc".to_string())]);
        assert_tokens("_", &[Token::Ident("_".to_string())]);
        assert_tokens("abc1", &[Token::Ident("abc1".to_string())]);
    }

    #[test]
    fn ns_ident() {
        assert_tokens("Abc", &[Token::NsIndent("Abc".to_string())]);
        assert_tokens("AbcAbc", &[Token::NsIndent("AbcAbc".to_string())]);

        assert_tokens(
            "AbcAbc.x",
            &[
                Token::NsIndent("AbcAbc".to_string()),
                Token::Dot,
                Token::Ident("x".to_string()),
            ],
        );
    }

    #[test]
    fn let_expr() {
        let src = "let num = 123;";

        assert_tokens(src, {
            use Token::*;
            &[Let, Ident("num".to_string()), Assign, Num(123.0), Semicolon]
        });
    }

    #[test]
    fn fn_expr() {
        let src = "fn (x, y) { f(x, y) }";

        assert_tokens(src, {
            use Token::*;
            &[
                Fn,
                LParen,
                Ident("x".to_string()),
                Comma,
                Ident("y".to_string()),
                RParen,
                LBrace,
                // <fn.body>
                Ident("f".to_string()),
                LParen,
                Ident("x".to_string()),
                Comma,
                Ident("y".to_string()),
                RParen,
                // </fn.body>
                RBrace,
            ]
        });
    }

    #[test]
    fn if_expr() {
        let src = "if (x < 10) { a } else { b }";

        assert_tokens(src, {
            use Token::*;
            &[
                If,
                LParen,
                Ident("x".to_string()),
                Less,
                Num(10.0),
                RParen,
                LBrace,
                Ident("a".to_string()),
                RBrace,
                Else,
                LBrace,
                Ident("b".to_string()),
                RBrace,
            ]
        });
    }

    #[test]
    fn comment() {
        let src = "if // comment until end of line \n else";

        assert_tokens(src, {
            use Token::*;
            &[If, Else]
        });
    }

    fn assert_tokens(src: &str, tokens: &[Token]) {
        let lexer = Lexer::new(src);
        assert_eq!(
            lexer.into_iter().map(|c| c.unwrap()).collect::<Vec<_>>(),
            tokens,
        )
    }
}
