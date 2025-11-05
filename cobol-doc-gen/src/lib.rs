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
//! ```rust,no_run
//! use cobol_doc_gen::{DocumentGenerator, GeneratorConfig, OutputFormat};
//! use cobol_ast::Program;
//! use anyhow::Result;
//!
//! fn main() -> Result<()> {
//!     let config = GeneratorConfig::default();
//!     let generator = DocumentGenerator::new(config);
//!     // let program: Program = ...; // Parse your COBOL program
//!     // let documentation = generator.generate(&program, OutputFormat::Html)?;
//!     Ok(())
//! }
//! ```

pub mod analyzer;
pub mod formats;
pub mod generator;
pub mod models;
pub mod security;
pub mod templates;

pub use formats::OutputFormat;
pub use generator::{DocumentGenerator, GeneratorConfig};
pub use models::{BusinessRule, DataStructure, Documentation, ProgramSummary};

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
