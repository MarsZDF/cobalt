//! # cobol-parser
//!
//! Parser for COBOL source code that converts tokens into an Abstract Syntax Tree (AST).
//!
//! This crate bridges the gap between `cobol-lexer` (which tokenizes COBOL source)
//! and `cobol-ast` (which provides AST data structures).
//!
//! ## Features
//!
//! - Recursive descent parser for COBOL programs
//! - Parses all four divisions (Identification, Environment, Data, Procedure)
//! - Handles statements, expressions, and data definitions
//! - Comprehensive error reporting with location information
//! - Incremental parsing support
//!
//! ## Example
//!
//! ```rust
//! use cobol_parser::parse;
//! use cobol_lexer::{tokenize, Format};
//!
//! let source = r#"
//!    IDENTIFICATION DIVISION.
//!    PROGRAM-ID. HELLO-WORLD.
//!    PROCEDURE DIVISION.
//!        DISPLAY "Hello, World!".
//!        STOP RUN.
//! "#;
//!
//! // First tokenize
//! let tokens = tokenize(source, Format::FreeFormat).unwrap();
//!
//! // Then parse
//! let program = parse(tokens).unwrap();
//! ```

pub mod error;
pub mod parser;

pub use error::{ParseError, ParseResult};
pub use parser::Parser;

/// Parse tokens into an AST program.
///
/// This is a convenience function that creates a parser and parses the tokens.
pub fn parse(
    tokens: Vec<cobol_lexer::Token>,
) -> ParseResult<cobol_ast::Spanned<cobol_ast::Program>> {
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}

/// Parse a COBOL source string directly.
///
/// This convenience function tokenizes and then parses the source code.
pub fn parse_source(
    source: &str,
    format: cobol_lexer::Format,
) -> ParseResult<cobol_ast::Spanned<cobol_ast::Program>> {
    let tokens = cobol_lexer::tokenize(source, format)?;
    parse(tokens)
}
