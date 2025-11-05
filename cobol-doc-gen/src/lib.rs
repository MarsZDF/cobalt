//! # cobol-doc-gen
//!
//! Documentation generator for COBOL programs - converts COBOL AST to readable specifications.
//!
//! ## Features
//!
//! - Generate HTML, Markdown, and JSON documentation from COBOL AST
//! - Extract data structures, business logic, and program flow
//! - Create cross-reference tables for variables and paragraphs
//! - Generate API documentation for called programs
//! - Support for customizable templates
//!
//! ## Example
//!
//! ```rust
//! use cobol_doc_gen::{DocumentGenerator, OutputFormat};
//! use cobol_ast::Program;
//!
//! let generator = DocumentGenerator::new();
//! let documentation = generator.generate(&program, OutputFormat::Html)?;
//! ```

pub mod analyzer;
pub mod generator;
pub mod templates;
pub mod models;
pub mod formats;

pub use generator::{DocumentGenerator, GeneratorConfig};
pub use formats::OutputFormat;
pub use models::{Documentation, ProgramSummary, DataStructure, BusinessRule};

use anyhow::Result;

/// Generate documentation for a COBOL program.
pub fn generate_documentation(
    program: &cobol_ast::Program,
    format: OutputFormat,
    config: Option<GeneratorConfig>,
) -> Result<String> {
    let generator = DocumentGenerator::new(config.unwrap_or_default());
    generator.generate(program, format)
}