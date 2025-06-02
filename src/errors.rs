use std::{fmt::Display, num::ParseIntError};

use crate::parser::Type;

#[derive(Debug)]
pub enum LexerError {
    EOF(String),
    IntSizeError(String),
    UnknownChar(char),
    EmptyNum,
}

impl LexerError {
    pub fn error_code(&self) -> i32 {
        match self {
            LexerError::IntSizeError(_) => 7,
            _ => 42,
        }
    }
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
    NoEOF(String),
    Mismatch { found: String, expected: String },
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::EOF(missing) => write!(f, "end of file, but expected {missing}"),
            ParserError::NoEOF(found) => write!(f, "expected end of file, but found {found}"),
            ParserError::Mismatch { found, expected } => {
                write!(f, "expected {expected} but found {found}")
            }
        }
    }
}

#[derive(Debug)]
pub enum StaticAnalysisError {
    TypeError(TypeError),
    BreakContinuePlacement,
    DeclareStep,
    MissingReturn,
}

impl Display for StaticAnalysisError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StaticAnalysisError::TypeError(e) => write!(f, "problem in type check: {e}"),
            Self::BreakContinuePlacement => write!(f, "found a break or continue outside a loop"),
            Self::DeclareStep => write!(f, "a for loop step may not declare a variable"),
            Self::MissingReturn => write!(f, "missing a return on at least one control path"),
        }
    }
}

impl From<TypeError> for StaticAnalysisError {
    fn from(value: TypeError) -> Self {
        StaticAnalysisError::TypeError(value)
    }
}

#[derive(Debug)]
pub enum TypeError {
    Mismatch { expected: Type, found: Type },
    MissingDec(String),
    Redecleration(String),
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::Mismatch { expected, found } => write!(
                f,
                "mismatched type: expected {:?} but found {:?}",
                expected, found
            ),
            TypeError::MissingDec(var) => write!(f, "Variable {var} not declared before use"),
            TypeError::Redecleration(var) => {
                write!(f, "Variable {var} declared multiple times in same scope")
            }
        }
    }
}
