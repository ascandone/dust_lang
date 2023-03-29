use super::token::Token;

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

    fn panic_invalid_token(&self) -> ! {
        panic!("Invalid token: `{:?}`", self.peek_char())
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
                    self.next_token()
                }
                _ => Token::Slash,
            },
            '%' => Token::Percentage,
            '&' => match self.peek_char() {
                Some('&') => {
                    self.next_char();
                    Token::DoubleAnd
                }
                _ => self.panic_invalid_token(),
            },
            '|' => match self.peek_char() {
                Some('|') => {
                    self.next_char();
                    Token::DoublePipe
                }
                _ => self.panic_invalid_token(),
            },

            '"' => Token::String(self.read_string_lit()),

            _ => {
                self.read_position -= 1;

                if let Some(ident) = self.consume_while(is_letter) {
                    return match ident {
                        "let" => Token::Let,
                        "fn" => Token::Fn,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "true" => Token::True,
                        "false" => Token::False,
                        "nil" => Token::Nil,
                        _ => Token::Ident(ident.to_string()),
                    };
                }

                if let Some(n) = self.consume_while(is_number) {
                    return Token::Num(n.parse().expect(format!("Invalid parse: `{n}`").as_str()));
                }

                self.panic_invalid_token()
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
    fn literals() {
        assert_tokens("true false nil", {
            use Token::*;
            &[True, False, Nil]
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
        assert_eq!(lexer.into_iter().collect::<Vec<_>>(), tokens,)
    }
}
