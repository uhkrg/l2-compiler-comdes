use std::{fmt::Display, num::ParseIntError};

#[derive(Debug)]
pub enum LexerError {
    EOF(String),
    IntSizeError(String),
    UnknownChar(char),
    EmptyNum,
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::EOF(pos) => write!(f, "end of file while {pos}"),
            LexerError::IntSizeError(num) => write!(f, "{num} does not fit into 32bits"),
            LexerError::UnknownChar(c) => write!(f, "unknown char {c} encountered while parsing"),
            LexerError::EmptyNum => write!(f, "number description contained no digits"),
        }
    }
}

impl From<ParseIntError> for LexerError {
    fn from(value: ParseIntError) -> Self {
        LexerError::IntSizeError(value.to_string())
    }
}

#[derive(Debug)]
pub enum ParserError {
    EOF(String),
    Mismatch { found: String, expected: String },
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::EOF(missing) => write!(f, "end of file, but expected {missing}"),
            ParserError::Mismatch { found, expected } => {
                write!(f, "expected {expected} but found {found}")
            }
        }
    }
}
