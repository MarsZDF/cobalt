//! # cobol-ast
//!
//! Abstract Syntax Tree (AST) data structures for COBOL programs.
//!
//! This crate provides a complete representation of COBOL programs as an AST,
//! suitable for use by parsers, analyzers, and code generators.
//!
//! ## Features
//!
//! - Complete AST representation of COBOL programs
//! - All four divisions (Identification, Environment, Data, Procedure)
//! - Data definitions with PICTURE clauses, OCCURS, REDEFINES, etc.
//! - Procedure division statements (PERFORM, IF, MOVE, COMPUTE, etc.)
//! - Expression trees for arithmetic, logical, and string operations
//! - Source location information (spans) for all nodes
//! - Visitor pattern for AST traversal
//! - Optional serialization support (serde)
//!
//! ## Example
//!
//! ```rust
//! use cobol_ast::{Program, Statement, Span, Spanned};
//!
//! // Build a simple AST programmatically
//! let program = Program {
//!     // ... program structure
//! };
//! ```
//!
//! ## Architecture
//!
//! This crate is part of the Cobalt COBOL tooling ecosystem. It provides
//! the data structures that parsers (e.g., `cobol-parser`) will produce
//! and analyzers will consume.

pub mod data;
pub mod expression;
pub mod literal;
pub mod program;
pub mod span;
pub mod statement;
pub mod visitor;

// Re-export commonly used types
pub use data::{
    DataItem, FigurativeConstant, InitialValue, OccursClause, OccursCount, Picture, Usage,
};
pub use expression::{BinaryOp, Expression, UnaryOp};
pub use literal::{Literal, NumericLiteral};
pub use program::{
    ConfigurationSection, DataDivision, EnvironmentDivision, FileControl, FileDescription,
    FileEntry, FileSection, IdentificationDivision, InputOutputSection, IoControl,
    LinkageSection, LocalStorageSection, Paragraph, ProcedureDivision, Program, Section,
    SpecialName, WorkingStorageSection,
};
pub use span::{Span, Spanned};
pub use statement::{
    AcceptStatement, CallStatement, ComputeStatement, DisplayOperand, DisplayStatement,
    ExitStatement, GoBackStatement, GoToStatement, IfStatement, MoveSource, MoveStatement,
    ParagraphStatement, PerformStatement, PerformVaryingClause, ReturnStatement, SectionStatement,
    Statement, StopStatement,
};
pub use visitor::Visitor;
