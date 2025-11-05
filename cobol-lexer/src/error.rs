use std::fmt;

/// Error type for lexer failures.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    /// Unexpected character at a given position
    UnexpectedChar {
        char: char,
        line: usize,
        column: usize,
    },
    
    /// Unterminated string literal
    UnterminatedString {
        line: usize,
        column: usize,
    },
    
    /// Invalid level number
    InvalidLevelNumber {
        value: String,
        line: usize,
        column: usize,
    },
    
    /// Invalid numeric literal
    InvalidNumericLiteral {
        value: String,
        line: usize,
        column: usize,
    },
    
    /// Line continuation error in fixed format
    InvalidContinuation {
        line: usize,
        column: usize,
    },
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexError::UnexpectedChar { char, line, column } => {
                write!(f, "Unexpected character '{}' at line {}, column {}", char, line, column)
            }
            LexError::UnterminatedString { line, column } => {
                write!(f, "Unterminated string literal at line {}, column {}", line, column)
            }
            LexError::InvalidLevelNumber { value, line, column } => {
                write!(
                    f,
                    "Invalid level number '{}' at line {}, column {} (must be 01-49, 66, 77, or 88)",
                    value, line, column
                )
            }
            LexError::InvalidNumericLiteral { value, line, column } => {
                write!(
                    f,
                    "Invalid numeric literal '{}' at line {}, column {}",
                    value, line, column
                )
            }
            LexError::InvalidContinuation { line, column } => {
                write!(
                    f,
                    "Invalid line continuation at line {}, column {}",
                    line, column
                )
            }
        }
    }
}

impl std::error::Error for LexError {}

/// Result type alias for lexer operations.
pub type LexResult<T> = Result<T, LexError>;
