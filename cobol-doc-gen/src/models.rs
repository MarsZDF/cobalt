use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Complete documentation for a COBOL program.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Documentation {
    pub program_summary: ProgramSummary,
    pub data_structures: Vec<DataStructure>,
    pub business_rules: Vec<BusinessRule>,
    pub procedure_flow: Vec<ProcedureBlock>,
    pub cross_references: CrossReferences,
    pub complexity_metrics: ComplexityMetrics,
}

/// High-level program summary.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProgramSummary {
    pub program_id: String,
    pub author: Option<String>,
    pub date_written: Option<String>,
    pub date_compiled: Option<String>,
    pub purpose: Option<String>,
    pub remarks: Option<String>,
    pub total_lines: usize,
    pub divisions: Vec<String>,
    pub called_programs: Vec<String>,
    pub input_files: Vec<String>,
    pub output_files: Vec<String>,
}

/// Data structure documentation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataStructure {
    pub name: String,
    pub level: u8,
    pub picture: Option<String>,
    pub usage: Option<String>,
    pub description: Option<String>,
    pub length: Option<usize>,
    pub initial_value: Option<String>,
    pub occurs: Option<String>,
    pub children: Vec<DataStructure>,
    pub line_number: Option<usize>,
}

/// Business rule extracted from code.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BusinessRule {
    pub id: String,
    pub description: String,
    pub rule_type: BusinessRuleType,
    pub location: SourceLocation,
    pub conditions: Vec<String>,
    pub actions: Vec<String>,
    pub complexity: RuleComplexity,
}

/// Type of business rule.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BusinessRuleType {
    Validation,
    Calculation,
    DataTransformation,
    ControlFlow,
    ErrorHandling,
}

/// Complexity assessment of a business rule.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RuleComplexity {
    Simple,
    Moderate,
    Complex,
    VeryComplex,
}

/// Source code location.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceLocation {
    pub paragraph: Option<String>,
    pub section: Option<String>,
    pub line_start: usize,
    pub line_end: usize,
}

/// Procedure division block (paragraph or section).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcedureBlock {
    pub name: String,
    pub block_type: ProcedureBlockType,
    pub purpose: Option<String>,
    pub statements: Vec<StatementSummary>,
    pub calls_to: Vec<String>,
    pub called_by: Vec<String>,
    pub cyclomatic_complexity: u32,
}

/// Type of procedure block.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ProcedureBlockType {
    Paragraph,
    Section,
}

/// Summary of a statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StatementSummary {
    pub statement_type: String,
    pub description: String,
    pub line_number: usize,
    pub variables_modified: Vec<String>,
    pub variables_used: Vec<String>,
}

/// Cross-reference information.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CrossReferences {
    pub variable_usage: HashMap<String, VariableUsage>,
    pub paragraph_calls: HashMap<String, Vec<String>>,
    pub copybook_includes: Vec<CopybookReference>,
}

/// Variable usage tracking.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariableUsage {
    pub definition_line: usize,
    pub modifications: Vec<usize>,
    pub references: Vec<usize>,
    pub data_type: String,
    pub picture: Option<String>,
}

/// Copybook reference.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CopybookReference {
    pub name: String,
    pub line_number: usize,
    pub replacing: Option<String>,
}

/// Code complexity metrics.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplexityMetrics {
    pub cyclomatic_complexity: u32,
    pub nesting_depth: u32,
    pub lines_of_code: usize,
    pub comment_ratio: f64,
    pub maintainability_index: f64,
    pub technical_debt_minutes: f64,
}