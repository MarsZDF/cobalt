//! Auto-formatter for COBOL source code (like rustfmt or black).
//!
//! This crate provides automatic code formatting for COBOL source files,
//! enforcing consistent style including indentation, case conventions, spacing,
//! and alignment.
//!
//! # Features
//!
//! - ✅ Configurable indentation (spaces/tabs, width)
//! - ✅ Keyword case normalization (UPPER, lower, preserve)
//! - ✅ Identifier case normalization
//! - ✅ Spacing around operators
//! - ✅ Column alignment for data items
//! - ✅ Line length enforcement
//! - ✅ Comment preservation
//! - ✅ Division and section formatting
//!
//! # Quick Start
//!
//! ```rust
//! use cobol_fmt::{format_source, FormatConfig};
//! use cobol_lexer::detect_format;
//!
//! let source = r#"
//!    IDENTIFICATION DIVISION.
//!    PROGRAM-ID. HELLO-WORLD.
//!    PROCEDURE DIVISION.
//!        DISPLAY "Hello, World!".
//!        STOP RUN.
//! "#;
//!
//! let format = detect_format(source);
//! let config = FormatConfig::traditional(); // or FormatConfig::modern()
//! let formatted = format_source(source, format, config)?;
//! println!("{}", formatted);
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! # Configuration
//!
//! ```rust
//! use cobol_fmt::{FormatConfig, KeywordCase, IdentifierCase};
//!
//! let config = FormatConfig {
//!     indent_width: 4,
//!     keyword_case: KeywordCase::Upper,
//!     identifier_case: IdentifierCase::Upper,
//!     ..Default::default()
//! };
//! ```

pub mod config;
pub mod formatter;

pub use config::{FormatConfig, KeywordCase, IdentifierCase};
pub use formatter::Formatter;

use cobol_lexer::{tokenize, Format};
use cobol_parser::parse_source;
use anyhow::Result;

/// Format COBOL source code with the given configuration.
pub fn format_source(source: &str, format: Format, config: FormatConfig) -> Result<String> {
    let tokens = tokenize(source, format)?;
    
    // Try to parse for better indentation (optional, will fall back to heuristics if fails)
    let ast = parse_source(source, format).ok();
    
    let formatter = Formatter::new(config);
    let formatted = formatter.format(&tokens, ast.as_ref());
    
    Ok(formatted)
}

/// Format COBOL source code with default configuration.
pub fn format(source: &str) -> Result<String> {
    let format = cobol_lexer::detect_format(source);
    format_source(source, format, FormatConfig::default())
}

