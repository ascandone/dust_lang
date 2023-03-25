use crate::token::Token;

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

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let Some(ch) = self.next_char() else { return Token::Eof };

        match ch {
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
                _ => Token::Not,
            },
            ',' => Token::Comma,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,

            _ => {
                self.read_position -= 1;

                if let Some(ident) = self.consume_while(is_letter) {
                    return match ident {
                        "let" => Token::Let,
                        "fn" => Token::Fn,
                        _ => Token::Ident(ident.to_string()),
                    };
                }

                if let Some(n) = self.consume_while(is_number) {
                    return Token::Num(n.parse().expect(format!("Invalid parse: `{n}`").as_str()));
                }

                panic!("Invalid token: `{ch}`")
            }
        }
    }
}

fn is_whitespace(ch: char) -> bool {
    match ch {
        ' ' | '\t' | '\n' | '\r' => true,
        _ => false,
    }
}

fn is_letter(ch: char) -> bool {
    match ch {
        'a'..='z' | 'A'..='Z' | '_' => true,
        _ => false,
    }
}

fn is_number(ch: char) -> bool {
    match ch {
        '0'..='9' => true,
        _ => false,
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Token::Eof => None,
            tk => Some(tk),
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
    fn operators() {
        assert_tokens("< > <= >= - ! != ==", {
            use Token::*;
            &[
                Less,
                Greater,
                LessEqual,
                GreaterEqual,
                Minus,
                Not,
                NotEq,
                Eq,
            ]
        });
    }

    #[test]
    fn complex_example() {
        let src = "
let num = 123;
fn (x, y) { f(x, y) }


";

        assert_tokens(src, {
            use Token::*;
            &[
                Let,
                Ident("num".to_string()),
                Assign,
                Num(123.0),
                Semicolon,
                Fn,
                LParen,
                Ident("x".to_string()),
                Comma,
                Ident("y".to_string()),
                RParen,
                LBrace,
                // <body>
                Ident("f".to_string()),
                LParen,
                Ident("x".to_string()),
                Comma,
                Ident("y".to_string()),
                RParen,
                // </body>
                RBrace,
            ]
        });
    }

    fn assert_tokens(src: &str, tokens: &[Token]) {
        let lexer = Lexer::new(src);
        assert_eq!(lexer.into_iter().collect::<Vec<_>>(), tokens,)
    }
}
