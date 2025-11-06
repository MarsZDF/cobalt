//! Dead code detector for COBOL programs.
//!
//! This crate provides analysis tools to detect unused code in COBOL programs,
//! including unused variables, unused paragraphs/sections, and unreachable code.
//!
//! # Features
//!
//! - ✅ Control Flow Graph (CFG) construction
//! - ✅ Reachability analysis
//! - ✅ Unused variable detection
//! - ✅ Unused paragraph/section detection
//! - ✅ Unreachable statement detection
//!
//! # Quick Start
//!
//! ```rust,no_run
//! use cobol_dead_code::analyze_program;
//! use cobol_parser::parse_source;
//! use cobol_lexer::detect_format;
//!
//! let source = r#"
//!    IDENTIFICATION DIVISION.
//!    PROGRAM-ID. TEST.
//!    DATA DIVISION.
//!    WORKING-STORAGE SECTION.
//!    01 UNUSED-VAR PIC 9(5).
//!    PROCEDURE DIVISION.
//!        DISPLAY "Hello".
//!        STOP RUN.
//! "#;
//!
//! let format = detect_format(source);
//! let program = parse_source(source, format).unwrap();
//! let report = analyze_program(program.node);
//!
//! println!("Unused variables: {}", report.unused_variables.len());
//! ```

pub mod cfg;
pub mod analyzer;

pub use analyzer::{DeadCodeAnalyzer, DeadCodeReport, UnusedVariable, UnreachableStatement};
pub use cfg::{ControlFlowGraph, CfgNode, CfgEdge, EdgeType};

use cobol_ast::Program;

/// Analyze a COBOL program for dead code.
pub fn analyze_program(program: Program) -> DeadCodeReport {
    let analyzer = DeadCodeAnalyzer::new(program);
    analyzer.analyze()
}

