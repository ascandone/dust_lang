use super::token::Token;

pub type Spanned<T> = (Position, T, Position);

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub character: usize,
}

impl Position {
    pub fn new(line: usize, character: usize) -> Self {
        Self { line, character }
    }

    pub fn advance_mut(&mut self, ch: char) {
        if ch == '\n' {
            self.line += 1;
            self.character = 0;
        } else {
            self.character += 1;
        }
    }

    pub fn next(&self, ch: char) -> Self {
        let mut p = self.clone();
        p.advance_mut(ch);
        p
    }

    pub fn next_str<'a>(&self, str: &'a str) -> Self {
        let mut s = self.clone();
        for ch in str.chars() {
            s.advance_mut(ch)
        }
        s
    }
}

pub struct Lexer {
    chars: Vec<(Position, char)>,
    index: usize,
}

impl Lexer {
    pub fn new<'a>(input: &'a str) -> Self {
        Self {
            index: 0,
            chars: input
                .chars()
                .scan(Position::default(), |pos, ch| {
                    let ret = pos.clone();
                    pos.advance_mut(ch);
                    Some((ret, ch))
                })
                .collect(),
        }
    }

    fn peek_char(&self) -> Option<(Position, char)> {
        self.chars.get(self.index).copied()
    }

    fn next_char(&mut self) -> Option<(Position, char)> {
        let res = self.peek_char();
        self.index += 1;
        res
    }

    fn next_char_expect(&mut self, ch: char) -> Option<(Position, char)> {
        let p = self.peek_char()?;
        if p.1 == ch {
            self.next_char()
        } else {
            None
        }
    }

    fn next_char_predicate<F>(&mut self, pred: F) -> Option<(Position, char)>
    where
        F: Fn(char) -> bool,
    {
        let (_, ch) = self.peek_char()?;
        if pred(ch) {
            self.next_char()
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek_char() {
                Some((_, ch)) if is_whitespace(ch) => self.next_char(),
                _ => return,
            };
        }
    }

    fn try_consume_str(&mut self, s: &'static str) -> Option<(Position, Position)> {
        let initial_index = self.index;
        let (initial_pos, _) = self.peek_char()?;

        for ch in s.chars() {
            if let None = self.next_char_expect(ch) {
                self.index = initial_index;
                return None;
            }
        }

        Some((initial_pos, initial_pos.next_str(s)))
    }

    fn try_consume_keyword(&mut self, kw: &'static str) -> Option<(Position, Position)> {
        let initial_index = self.index;
        let (st, end) = self.try_consume_str(kw)?;
        if let Some((_, next_ch)) = self.peek_char() {
            // TODO extract predicate
            if next_ch.is_ascii_alphanumeric() || next_ch == '_' {
                self.index = initial_index;
                return None;
            }
        }
        Some((st, end))
    }

    fn one_of<F>(
        &mut self,
        try_consume: F,
        pairs: Vec<(&'static str, Token)>,
    ) -> Option<Spanned<Result<Token, LexerError>>>
    where
        F: Fn(&mut Self, &'static str) -> Option<(Position, Position)>,
    {
        for (kw, tk) in pairs {
            if let Some((st, end)) = try_consume(self, kw) {
                return Some((st, Ok(tk), end));
            }
        }
        None
    }

    fn consume_while<F>(&mut self, pred: F) -> Option<Spanned<String>>
    where
        F: Fn(char) -> bool,
    {
        let (pos, _) = self.peek_char()?;
        let mut s = String::new();

        let mut end_pos = pos;
        loop {
            let Some((pos, ch)) = self.next_char_predicate(&pred) else {
                break;
            };

            s.push(ch);
            end_pos = pos.next(ch);
        }

        Some((pos, s, end_pos))
    }

    fn consume_identifier<F, G>(&mut self, is_head: F, is_tail: G) -> Option<Spanned<String>>
    where
        F: Fn(char) -> bool,
        G: Fn(char) -> bool,
    {
        let (pos, head) = self.next_char_predicate(is_head)?;

        let mut end_pos = pos.next(head);
        let mut str = head.to_string();

        if let Some((_, ident, end)) = self.consume_while(is_tail) {
            end_pos = end;
            str.push_str(&ident);
        };

        Some((pos, str.to_string(), end_pos))
    }
}

fn is_ident_starting_letter(ch: char) -> bool {
    matches!(ch, '_' | 'a'..='z')
}

fn is_ident_tail_letter(ch: char) -> bool {
    matches!(ch, '_' | 'a'..='z' | '0' ..='9')
}

fn is_ns_ident_starting_letter(ch: char) -> bool {
    ch.is_ascii_uppercase()
}

fn is_ns_ident_tail_letter(ch: char) -> bool {
    ch.is_ascii_alphabetic()
}

fn is_whitespace(ch: char) -> bool {
    matches!(ch, ' ' | '\t' | '\n' | '\r')
}

fn is_number(ch: char) -> bool {
    ch.is_ascii_digit()
}

pub type LexerError = ();

impl Iterator for Lexer {
    type Item = Spanned<Result<Token, LexerError>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        if let Some(k) = self.one_of(
            Self::try_consume_keyword,
            vec![
                ("as", Token::As),
                ("match", Token::Match),
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
            ],
        ) {
            return Some(k);
        };

        if let Some(k) = self.one_of(
            Self::try_consume_str,
            vec![
                ("..", Token::Dots),
                (".", Token::Dot),
                ("<-", Token::ArrowLeft),
                ("<=", Token::LessEqual),
                ("<", Token::Less),
                (">=", Token::GreaterEqual),
                (">", Token::Greater),
                ("==", Token::Eq),
                ("=", Token::Assign),
                ("*", Token::Mult),
                ("+", Token::Plus),
                ("-", Token::Minus),
                ("!=", Token::NotEq),
                ("!", Token::Bang),
                ("/", Token::Slash),
                ("%", Token::Percentage),
                ("&&", Token::DoubleAnd),
                ("|>", Token::PipeRight),
                ("||", Token::DoublePipe),
                ("(", Token::LParen),
                (")", Token::RParen),
                ("[", Token::LBracket),
                ("]", Token::RBracket),
                ("{", Token::LBrace),
                ("}", Token::RBrace),
                ("#{", Token::HashLBrace),
                ("#(", Token::HashLParen),
            ],
        ) {
            return Some(k);
        };

        if let Some((st, id, end)) =
            self.consume_identifier(is_ident_starting_letter, is_ident_tail_letter)
        {
            return Some((st, Ok(Token::Ident(id)), end));
        }

        if let Some((st, id, end)) =
            self.consume_identifier(is_ns_ident_starting_letter, is_ns_ident_tail_letter)
        {
            return Some((st, Ok(Token::NsIndent(id)), end));
        }

        let (_pos, ch) = self.peek_char()?;
        todo!("Invalid char: \"{ch}\"");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_ch() {
        assert_tokens(
            "=",
            &[(Position::new(0, 0), Ok(Token::Assign), Position::new(0, 1))],
        )
    }

    #[test]
    fn sep_by_space() {
        assert_tokens(
            "=  =  ",
            &[
                (Position::new(0, 0), Ok(Token::Assign), Position::new(0, 1)),
                (Position::new(0, 3), Ok(Token::Assign), Position::new(0, 4)),
            ],
        )
    }

    #[test]
    fn two_chars_ident() {
        assert_tokens(
            "==",
            &[(Position::new(0, 0), Ok(Token::Eq), Position::new(0, 2))],
        )
    }

    #[test]
    fn two_syms() {
        assert_tokens(
            "+*",
            &[
                (Position::new(0, 0), Ok(Token::Plus), Position::new(0, 1)),
                (Position::new(0, 1), Ok(Token::Mult), Position::new(0, 2)),
            ],
        )
    }

    #[test]
    fn keywords() {
        const STR: &'static str = "true false nil let use import pub as";

        fn tk(t: Token, s: &'static str) -> Spanned<Result<Token, LexerError>> {
            let st = STR.find(s).unwrap();
            (Position::new(0, st), Ok(t), Position::new(0, st + s.len()))
        }

        assert_tokens(
            STR,
            &[
                tk(Token::True, "true"),
                tk(Token::False, "false"),
                tk(Token::Nil, "nil"),
                tk(Token::Let, "let"),
                tk(Token::Use, "use"),
                tk(Token::Import, "import"),
                tk(Token::Pub, "pub"),
                tk(Token::As, "as"),
            ],
        )
    }

    #[test]
    fn delimiters() {
        assert_tokens_no_pos("( ) [ ] { } #( #{", {
            use Token::*;
            &[
                LParen, RParen, LBracket, RBracket, LBrace, RBrace, HashLParen, HashLBrace,
            ]
        });
    }

    #[test]
    fn operators() {
        assert_tokens_no_pos("< > <= >= - ! != == + * / % && || |>", {
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
                PipeRight,
            ]
        });
    }

    #[test]
    fn idents() {
        const STR: &'static str = "abc k de_f x1";

        fn ident(s: &'static str) -> Spanned<Result<Token, LexerError>> {
            let st = STR.find(s).unwrap();
            (
                one_line_pos(st),
                Ok(Token::Ident(s.to_string())),
                one_line_pos(st + s.len()),
            )
        }

        assert_tokens(STR, &[ident("abc"), ident("k"), ident("de_f"), ident("x1")])
    }

    #[test]
    fn ns_idents() {
        const STR: &'static str = "Abc Def";

        fn ident(s: &'static str) -> Spanned<Result<Token, LexerError>> {
            let st = STR.find(s).unwrap();
            (
                one_line_pos(st),
                Ok(Token::NsIndent(s.to_string())),
                one_line_pos(st + s.len()),
            )
        }

        assert_tokens(STR, &[ident("Abc"), ident("Def")])
    }

    #[test]
    fn ns_idents_with_dot() {
        const STR: &'static str = "A.B";

        assert_tokens(
            STR,
            &[
                (
                    one_line_pos(0),
                    Ok(Token::NsIndent("A".to_string())),
                    one_line_pos(1),
                ),
                (one_line_pos(1), Ok(Token::Dot), one_line_pos(2)),
                (
                    one_line_pos(2),
                    Ok(Token::NsIndent("B".to_string())),
                    one_line_pos(3),
                ),
            ],
        )
    }

    fn one_line_pos(index: usize) -> Position {
        Position {
            line: 0,
            character: index,
        }
    }

    fn assert_tokens(src: &str, expected: &[Spanned<Result<Token, LexerError>>]) {
        let tokens = Lexer::new(src).into_iter().collect::<Vec<_>>();
        assert_eq!(tokens, expected)
    }

    fn assert_tokens_no_pos(src: &str, expected: &[Token]) {
        let tokens = Lexer::new(src)
            .into_iter()
            .map(|(_, r, _)| r.unwrap())
            .collect::<Vec<_>>();
        assert_eq!(tokens, expected)
    }

    #[test]
    fn multiline() {
        assert_tokens(
            "=\n=",
            &[
                (
                    Position {
                        character: 0,
                        line: 0,
                    },
                    Ok(Token::Assign),
                    Position {
                        character: 1,
                        line: 0,
                    },
                ),
                (
                    Position {
                        character: 0,
                        line: 1,
                    },
                    Ok(Token::Assign),
                    Position {
                        character: 1,
                        line: 1,
                    },
                ),
            ],
        )
    }

    #[test]
    fn position_next() {
        let mut pos = Position::default();
        pos.advance_mut('a');
        assert_eq!(
            pos,
            Position {
                character: 1,
                line: 0,
            }
        );

        pos.advance_mut('b');
        assert_eq!(
            pos,
            Position {
                character: 2,
                line: 0,
            }
        );

        pos.advance_mut('\n');
        assert_eq!(
            pos,
            Position {
                character: 0,
                line: 1,
            }
        );

        pos.advance_mut('c');
        assert_eq!(
            pos,
            Position {
                character: 1,
                line: 1,
            }
        );
    }
}
