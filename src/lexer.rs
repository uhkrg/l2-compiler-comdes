use std::char;

use crate::errors::LexerError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexToken {
    BraceOpen,
    BraceClose,
    ParenOpen,
    ParenClose,
    Semicolon,
    QuestionMark,
    Colon,
    Unop(Unop),
    Binop(Binop),
    Asnop(Option<Binop>),
    Minus,
    IntConst(i32),
    Ident(String),
    Reserved(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Unop {
    LogNot,
    BitNot,
    Neg,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Binop {
    Less,
    Leq,
    Greater,
    Geq,
    Eq,
    Neq,
    Add,
    Minus,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitXor,
    BitOr,
    ShiftL,
    ShiftR,
    LogAnd,
    LogOr,
}

pub fn lex(input: &str) -> Result<Vec<LexToken>, LexerError> {
    let mut lexer = Lexer {
        text: input.chars().collect(),
        next_pos: 0,
        tokens: vec![],
    };
    lexer.lex_()?;
    Ok(lexer.tokens)
}

struct Lexer {
    text: Vec<char>,
    next_pos: usize,
    tokens: Vec<LexToken>,
}

impl Lexer {
    fn lex_(&mut self) -> Result<(), LexerError> {
        if self.next_pos == self.text.len() {
            return Ok(());
        }

        self.next_pos += 1;
        let token = match self.text[self.next_pos - 1] {
            '(' => Some(LexToken::ParenOpen),
            ')' => Some(LexToken::ParenClose),
            '{' => Some(LexToken::BraceOpen),
            '}' => Some(LexToken::BraceClose),
            ';' => Some(LexToken::Semicolon),
            '?' => Some(LexToken::QuestionMark),
            ':' => Some(LexToken::Colon),
            '!' => Some(self.one_or_two(
                LexToken::Unop(Unop::LogNot),
                '=',
                LexToken::Binop(Binop::Neq),
            )),
            '~' => Some(LexToken::Unop(Unop::BitNot)),
            '-' => Some(self.one_or_two(LexToken::Minus, '=', LexToken::Asnop(Some(Binop::Minus)))),
            '=' => Some(self.one_or_two(LexToken::Asnop(None), '=', LexToken::Binop(Binop::Eq))),
            '+' => Some(self.one_or_two(
                LexToken::Binop(Binop::Add),
                '=',
                LexToken::Asnop(Some(Binop::Add)),
            )),
            '*' => Some(self.one_or_two(
                LexToken::Binop(Binop::Mul),
                '=',
                LexToken::Asnop(Some(Binop::Mul)),
            )),
            '%' => Some(self.one_or_two(
                LexToken::Binop(Binop::Mod),
                '=',
                LexToken::Asnop(Some(Binop::Mod)),
            )),
            '^' => Some(self.one_or_two(
                LexToken::Binop(Binop::BitXor),
                '=',
                LexToken::Asnop(Some(Binop::BitXor)),
            )),
            '&' => Some(self.parse_and()),
            '|' => Some(self.parse_pipe()),
            '<' => Some(self.parse_less()),
            '>' => Some(self.parse_greater()),
            '/' => self.parse_slash()?,
            '\t' => None,
            '\r' => None,
            '\n' => None,
            ' ' => None,
            '0' => Some(self.parse_zero()?),
            c => {
                if '1' <= c && c <= '9' {
                    self.next_pos -= 1;
                    Some(self.parse_decnum()?)
                } else if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_' {
                    self.next_pos -= 1;
                    Some(self.parse_ident())
                } else {
                    return Err(LexerError::UnknownChar(c));
                }
            }
        };

        if let Some(token) = token {
            self.tokens.push(token);
        }

        self.lex_()
    }

    fn one_or_two(&mut self, token_one: LexToken, second: char, token_two: LexToken) -> LexToken {
        if self
            .text
            .get(self.next_pos)
            .is_some_and(|&next| next == second)
        {
            self.next_pos += 1;
            token_two
        } else {
            token_one
        }
    }

    fn parse_and(&mut self) -> LexToken {
        match self.text.get(self.next_pos) {
            Some('=') => {
                self.next_pos += 1;
                LexToken::Asnop(Some(Binop::BitAnd))
            }
            Some('&') => {
                self.next_pos += 1;
                LexToken::Binop(Binop::LogAnd)
            }
            _ => LexToken::Binop(Binop::BitAnd),
        }
    }

    fn parse_pipe(&mut self) -> LexToken {
        match self.text.get(self.next_pos) {
            Some('=') => {
                self.next_pos += 1;
                LexToken::Asnop(Some(Binop::BitOr))
            }
            Some('|') => {
                self.next_pos += 1;
                LexToken::Binop(Binop::LogOr)
            }
            _ => LexToken::Binop(Binop::BitOr),
        }
    }

    fn parse_less(&mut self) -> LexToken {
        match self.text.get(self.next_pos) {
            Some('=') => {
                self.next_pos += 1;
                LexToken::Binop(Binop::Leq)
            }
            Some('<') => {
                self.next_pos += 1;
                self.one_or_two(
                    LexToken::Binop(Binop::ShiftL),
                    '=',
                    LexToken::Asnop(Some(Binop::ShiftL)),
                )
            }
            _ => LexToken::Binop(Binop::Less),
        }
    }

    fn parse_greater(&mut self) -> LexToken {
        match self.text.get(self.next_pos) {
            Some('=') => {
                self.next_pos += 1;
                LexToken::Binop(Binop::Geq)
            }
            Some('>') => {
                self.next_pos += 1;
                self.one_or_two(
                    LexToken::Binop(Binop::ShiftR),
                    '=',
                    LexToken::Asnop(Some(Binop::ShiftR)),
                )
            }
            _ => LexToken::Binop(Binop::Greater),
        }
    }

    fn parse_slash(&mut self) -> Result<Option<LexToken>, LexerError> {
        match self.text.get(self.next_pos) {
            Some('/') => {
                self.skip_while(|c| c != '\n');
                self.next_pos += 1;
                Ok(None)
            }
            Some('*') => {
                self.parse_comment()?;
                Ok(None)
            }
            Some('=') => Ok(Some(LexToken::Asnop(Some(Binop::Div)))),
            _ => Ok(Some(LexToken::Binop(Binop::Div))),
        }
    }

    fn parse_comment(&mut self) -> Result<(), LexerError> {
        self.next_pos += 1;

        loop {
            self.skip_while(|c| c != '*' && c != '/');

            let found = match self.peek() {
                None => return Err(LexerError::EOF(String::from("parsing comment"))),
                Some(&c) => c,
            };

            self.next_pos += 1;

            match found {
                '*' => {
                    if self.peek().is_some_and(|&c| c == '/') {
                        self.next_pos += 1;
                        return Ok(());
                    }
                }
                '/' => {
                    if self.peek().is_some_and(|&c| c == '*') {
                        self.parse_comment()?;
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    fn parse_zero(&mut self) -> Result<LexToken, LexerError> {
        if self.peek().is_some_and(|&c| c == 'x' || c == 'X') {
            self.next_pos += 1;
            self.parse_hexnum()
        } else {
            Ok(LexToken::IntConst(0))
        }
    }

    const HEX_CHARS: [char; 22] = [
        'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f', '0', '1', '2', '3', '4', '5',
        '6', '7', '8', '9',
    ];

    fn parse_hexnum(&mut self) -> Result<LexToken, LexerError> {
        let mut chars = vec![];
        while let Some(next) = self.peek() {
            if Lexer::HEX_CHARS.contains(next) {
                chars.push(*next);
            } else {
                break;
            }
            self.next_pos += 1;
        }
        if chars.is_empty() {
            Err(LexerError::EmptyNum)
        } else {
            let num_text = chars.into_iter().collect::<String>();

            Ok(LexToken::IntConst(
                u32::from_str_radix(&num_text, 16)? as i32
            ))
        }
    }

    fn parse_decnum(&mut self) -> Result<LexToken, LexerError> {
        let start = self.next_pos;
        while let Some(&next) = self.peek() {
            if '0' > next || '9' < next {
                break;
            }
            self.next_pos += 1;
        }
        let num_text = self.text[start..self.next_pos]
            .iter()
            .cloned()
            .collect::<String>();
        Ok(LexToken::IntConst(num_text.parse::<u32>()? as i32))
    }

    const RESERVED: [&str; 21] = [
        "struct",
        "if",
        "else",
        "while",
        "for",
        "continue",
        "break",
        "return",
        "assert",
        "true",
        "false",
        "NULL",
        "print",
        "read",
        "alloc",
        "alloc_array",
        "int",
        "bool",
        "void",
        "char",
        "string",
    ];

    fn parse_ident(&mut self) -> LexToken {
        let start = self.next_pos;
        while let Some(&next) = self.peek() {
            if ('A' > next || 'Z' < next)
                && ('a' > next || 'z' < next)
                && ('0' > next || '9' < next)
                && '_' != next
            {
                break;
            }
            self.next_pos += 1;
        }

        let string: String = self.text[start..self.next_pos].into_iter().collect();
        if Lexer::RESERVED.contains(&string.as_str()) {
            LexToken::Reserved(string)
        } else {
            LexToken::Ident(string)
        }
    }

    fn skip_while(&mut self, f: fn(char) -> bool) {
        while self.next_pos < self.text.len() && f(self.text[self.next_pos]) {
            self.next_pos += 1;
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.text.get(self.next_pos)
    }
}
