use cobol_lexer::{LexError, Token};
use std::fmt;

/// Parse error type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    /// Unexpected token encountered
    UnexpectedToken {
        expected: Vec<String>,
        found: Token,
    },
    
    /// End of input encountered unexpectedly
    UnexpectedEof {
        expected: Vec<String>,
    },
    
    /// Lexer error
    LexerError(LexError),
    
    /// Invalid syntax
    InvalidSyntax {
        message: String,
        token: Option<Token>,
    },
    
    /// Invalid level number
    InvalidLevelNumber {
        value: u8,
        token: Token,
    },
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, found } => {
                write!(
                    f,
                    "Unexpected token at line {}, column {}: found '{}', expected {}",
                    found.line,
                    found.column,
                    found.lexeme,
                    if expected.len() == 1 {
                        format!("'{}'", expected[0])
                    } else {
                        format!("one of: {}", expected.join(", "))
                    }
                )
            }
            ParseError::UnexpectedEof { expected } => {
                write!(
                    f,
                    "Unexpected end of input: expected {}",
                    if expected.len() == 1 {
                        format!("'{}'", expected[0])
                    } else {
                        format!("one of: {}", expected.join(", "))
                    }
                )
            }
            ParseError::LexerError(e) => write!(f, "Lexer error: {}", e),
            ParseError::InvalidSyntax { message, token } => {
                if let Some(t) = token {
                    write!(
                        f,
                        "Invalid syntax at line {}, column {}: {}",
                        t.line, t.column, message
                    )
                } else {
                    write!(f, "Invalid syntax: {}", message)
                }
            }
            ParseError::InvalidLevelNumber { value, token } => {
                write!(
                    f,
                    "Invalid level number {} at line {}, column {}: must be 01-49, 66, 77, or 88",
                    value, token.line, token.column
                )
            }
        }
    }
}

impl std::error::Error for ParseError {}

impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        ParseError::LexerError(err)
    }
}

/// Result type alias for parser operations.
pub type ParseResult<T> = Result<T, ParseError>;
