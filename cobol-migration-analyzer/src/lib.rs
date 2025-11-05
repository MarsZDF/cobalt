//! # cobol-migration-analyzer
//!
//! Migration assessment tool for COBOL to cloud/microservices transformation.
//!
//! ## Features
//!
//! - Analyze COBOL codebases for cloud migration readiness
//! - Identify microservices boundaries based on business logic
//! - Estimate migration effort and complexity
//! - Generate migration roadmaps and recommendations
//! - Assess technical debt and modernization opportunities
//! - Identify security vulnerabilities and compliance issues
//!
//! ## Example
//!
//! ```rust,no_run
//! use cobol_migration_analyzer::{MigrationAnalyzer, AnalysisConfig};
//! use cobol_ast::Program;
//! use anyhow::Result;
//!
//! fn main() -> Result<()> {
//!     let analyzer = MigrationAnalyzer::new(AnalysisConfig::default());
//!     // let program: Program = ...; // Parse your COBOL program
//!     // let assessment = analyzer.analyze_program(&program)?;
//!     Ok(())
//! }
//! ```

pub mod analysis;
pub mod assessment;
pub mod microservices;
pub mod cloud_readiness;
pub mod effort_estimation;
pub mod recommendations;
pub mod models;
pub mod security;

pub use analysis::{MigrationAnalyzer, AnalysisConfig};
pub use assessment::MigrationAssessment;
pub use models::*;

use anyhow::Result;

/// Analyze a COBOL program for migration readiness.
pub fn analyze_migration_readiness(
    program: &cobol_ast::Program,
    config: Option<AnalysisConfig>,
) -> Result<MigrationAssessment> {
    let analyzer = MigrationAnalyzer::new(config.unwrap_or_default());
    analyzer.analyze_program(program)
}