//! # cobol-lexer
//!
//! A fast, modular lexer for COBOL source code supporting both fixed-format and free-format.
//!
//! ## Features
//!
//! - Tokenizes COBOL source code into a stream of tokens
//! - Supports both fixed-format (column-based) and free-format COBOL
//! - Zero-copy where possible (uses references)
//! - Comprehensive error reporting with line and column information
//! - Case-insensitive keyword recognition
//! - Handles COBOL-specific constructs (level numbers, PICTURE clauses, etc.)
//!
//! ## Example
//!
//! ```rust
//! use cobol_lexer::{tokenize, Format};
//!
//! let source = r#"
//! IDENTIFICATION DIVISION.
//! PROGRAM-ID. HELLO-WORLD.
//! PROCEDURE DIVISION.
//!     DISPLAY "Hello, World!".
//!     STOP RUN.
//! "#;
//!
//! let tokens = tokenize(source, Format::FreeFormat).unwrap();
//! for token in tokens {
//!     println!("{:?}", token);
//! }
//! ```
//!
//! ## Architecture
//!
//! This crate is part of the Cobalt COBOL tooling ecosystem. It focuses solely
//! on lexical analysis - converting source code into tokens. The tokens are
//! designed to be consumed by parsers in other crates (e.g., `cobol-parser`).

pub mod error;
pub mod fixed;
pub mod free;
pub mod lexer;
pub mod token;

pub use error::{LexError, LexResult};
pub use lexer::{detect_format, tokenize, Format};
pub use token::{Token, TokenType};
