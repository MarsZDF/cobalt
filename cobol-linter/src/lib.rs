use cobol_ast::{Program, Statement};
use cobol_lexer::Format;
use cobol_parser::parse_source;
use serde::{Deserialize, Serialize};

/// A lint rule that checks for code issues.
pub trait LintRule {
    fn name(&self) -> &str;
    fn check(&self, program: &Program) -> Vec<LintIssue>;
}

/// A linting issue found in the code.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LintIssue {
    pub severity: Severity,
    pub rule: String,
    pub message: String,
    pub location: Option<String>,
}

/// Severity level of a lint issue.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Info,
    Warning,
    Error,
}

impl std::fmt::Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Severity::Info => write!(f, "info"),
            Severity::Warning => write!(f, "warning"),
            Severity::Error => write!(f, "error"),
        }
    }
}

/// Linter that checks COBOL programs for issues.
pub struct Linter {
    rules: Vec<Box<dyn LintRule>>,
}

impl Linter {
    pub fn new() -> Self {
        let mut linter = Self { rules: Vec::new() };
        linter.add_default_rules();
        linter
    }

    pub fn add_rule(&mut self, rule: Box<dyn LintRule>) {
        self.rules.push(rule);
    }

    fn add_default_rules(&mut self) {
        self.rules.push(Box::new(StopRunRule));
        self.rules.push(Box::new(NamingConventionRule));
        self.rules.push(Box::new(ComplexityRule));
        self.rules.push(Box::new(UnusedVariableRule));
        self.rules.push(Box::new(DeprecatedSyntaxRule));
        self.rules.push(Box::new(Y2KDateRule));
        self.rules.push(Box::new(Cobol2014ComplianceRule));
    }

    pub fn lint(&self, source: &str, format: Format) -> Result<Vec<LintIssue>, String> {
        let program = parse_source(source, format).map_err(|e| format!("Parse error: {}", e))?;

        let mut issues = Vec::new();
        for rule in &self.rules {
            issues.extend(rule.check(&program.node));
        }

        Ok(issues)
    }

    pub fn lint_file(&self, path: &str) -> Result<Vec<LintIssue>, String> {
        let content =
            std::fs::read_to_string(path).map_err(|e| format!("Failed to read file: {}", e))?;

        let format = cobol_lexer::detect_format(&content);
        self.lint(&content, format)
    }
}

impl Default for Linter {
    fn default() -> Self {
        Self::new()
    }
}

// Lint Rules

/// Checks for STOP RUN statements.
struct StopRunRule;

impl LintRule for StopRunRule {
    fn name(&self) -> &str {
        "stop-run-present"
    }

    fn check(&self, program: &Program) -> Vec<LintIssue> {
        let mut issues = Vec::new();
        let mut has_stop_run = false;

        for stmt in &program.procedure.node.statements {
            if let Statement::Stop { .. } = stmt.node {
                has_stop_run = true;
                break;
            }
        }

        if !has_stop_run {
            issues.push(LintIssue {
                severity: Severity::Warning,
                rule: self.name().to_string(),
                message: "Program should have a STOP RUN statement".to_string(),
                location: None,
            });
        }

        issues
    }
}

/// Checks naming conventions.
struct NamingConventionRule;

impl LintRule for NamingConventionRule {
    fn name(&self) -> &str {
        "naming-convention"
    }

    fn check(&self, program: &Program) -> Vec<LintIssue> {
        let mut issues = Vec::new();

        // Check program ID naming (should be uppercase with hyphens)
        if let Some(prog_id) = &program.identification.node.program_id {
            let id = prog_id.as_str();
            if !id.chars().all(|c| c.is_uppercase() || c == '-') {
                issues.push(LintIssue {
                    severity: Severity::Info,
                    rule: self.name().to_string(),
                    message: format!("Program ID '{}' should be uppercase with hyphens", id),
                    location: Some("IDENTIFICATION DIVISION".to_string()),
                });
            }
        }

        // Check data item naming
        if let Some(data) = &program.data {
            if let Some(ws) = &data.node.working_storage_section {
                for item in &ws.node.data_items {
                    let name_str = item.node.name.node.as_str();
                    if name_str.len() > 30 {
                        issues.push(LintIssue {
                            severity: Severity::Warning,
                            rule: self.name().to_string(),
                            message: format!(
                                "Data item '{}' exceeds 30 characters (COBOL limit)",
                                name_str
                            ),
                            location: None,
                        });
                    }
                }
            }
        }

        issues
    }
}

/// Checks for code complexity.
struct ComplexityRule;

impl LintRule for ComplexityRule {
    fn name(&self) -> &str {
        "complexity"
    }

    fn check(&self, program: &Program) -> Vec<LintIssue> {
        let mut issues = Vec::new();
        let statement_count = program.procedure.node.statements.len();

        if statement_count > 100 {
            issues.push(LintIssue {
                severity: Severity::Warning,
                rule: self.name().to_string(),
                message: format!(
                    "High complexity: {} statements (consider refactoring)",
                    statement_count
                ),
                location: None,
            });
        }

        // Count nested IF statements
        let mut max_depth = 0;
        let mut current_depth = 0;

        for stmt in &program.procedure.node.statements {
            match &stmt.node {
                Statement::If { .. } => {
                    current_depth += 1;
                    max_depth = max_depth.max(current_depth);
                }
                _ => {
                    // Reset depth when we exit an IF block
                    // This is simplified - real implementation would track END-IF
                }
            }
        }

        if max_depth > 3 {
            issues.push(LintIssue {
                severity: Severity::Warning,
                rule: self.name().to_string(),
                message: format!("Deep nesting detected (depth: {})", max_depth),
                location: None,
            });
        }

        issues
    }
}

/// Checks for unused variables (simplified).
struct UnusedVariableRule;

impl LintRule for UnusedVariableRule {
    fn name(&self) -> &str {
        "unused-variable"
    }

    fn check(&self, _program: &Program) -> Vec<LintIssue> {
        // This is a placeholder - real implementation would track variable usage
        Vec::new()
    }
}

/// Checks for deprecated COBOL syntax.
struct DeprecatedSyntaxRule;

impl LintRule for DeprecatedSyntaxRule {
    fn name(&self) -> &str {
        "deprecated-syntax"
    }

    fn check(&self, program: &Program) -> Vec<LintIssue> {
        let mut issues = Vec::new();

        // Check for deprecated statements in PROCEDURE DIVISION
        for stmt in &program.procedure.node.statements {
            match &stmt.node {
                Statement::GoTo { .. } => {
                    issues.push(LintIssue {
                        severity: Severity::Warning,
                        rule: self.name().to_string(),
                        message: "GO TO statement is deprecated. Use structured programming constructs instead (IF, PERFORM, EVALUATE)".to_string(),
                        location: None,
                    });
                }
                // ALTER statement would be checked here if we had it in the AST
                _ => {}
            }
        }

        issues
    }
}

/// Checks for Y2K-style date issues.
struct Y2KDateRule;

impl LintRule for Y2KDateRule {
    fn name(&self) -> &str {
        "y2k-date-format"
    }

    fn check(&self, program: &Program) -> Vec<LintIssue> {
        let mut issues = Vec::new();

        // Check for 2-digit year patterns in data items
        if let Some(data) = &program.data {
            if let Some(ws) = &data.node.working_storage_section {
                for item in &ws.node.data_items {
                    // Check PICTURE clauses for date patterns
                    if let Some(pic) = &item.node.picture {
                        let pic_str = pic.node.string.to_uppercase();
                        
                        // Check for YYMMDD pattern (6 digits - 2-digit year)
                        if pic_str.contains("9(6)") || pic_str == "999999" {
                            let name_lower = item.node.name.node.to_lowercase();
                            // Check if name suggests it's a date
                            if name_lower.contains("date") || name_lower.contains("dt") {
                                issues.push(LintIssue {
                                    severity: Severity::Warning,
                                    rule: self.name().to_string(),
                                    message: format!(
                                        "Data item '{}' uses 6-digit date format (YYMMDD) which may cause Y2K issues. Consider using 8-digit format (YYYYMMDD)",
                                        item.node.name.node
                                    ),
                                    location: None,
                                });
                            }
                        }
                        
                        // Check for YY pattern (2-digit year)
                        if pic_str.contains("99") && pic_str.len() <= 4 {
                            let name_lower = item.node.name.node.to_lowercase();
                            if name_lower.contains("year") || name_lower.contains("yr") {
                                issues.push(LintIssue {
                                    severity: Severity::Warning,
                                    rule: self.name().to_string(),
                                    message: format!(
                                        "Data item '{}' uses 2-digit year format which may cause Y2K issues. Consider using 4-digit year format",
                                        item.node.name.node
                                    ),
                                    location: None,
                                });
                            }
                        }
                    }
                }
            }
        }

        issues
    }
}

/// Checks for COBOL 2014 compliance.
struct Cobol2014ComplianceRule;

impl LintRule for Cobol2014ComplianceRule {
    fn name(&self) -> &str {
        "cobol-2014-compliance"
    }

    fn check(&self, program: &Program) -> Vec<LintIssue> {
        let mut issues = Vec::new();

        // Check for features that may not be in COBOL 2014 standard
        // This is a simplified check - a full implementation would need
        // comprehensive knowledge of COBOL 2014 standard
        
        // Check for GO TO (deprecated in modern COBOL)
        for stmt in &program.procedure.node.statements {
            match &stmt.node {
                Statement::GoTo { .. } => {
                    issues.push(LintIssue {
                        severity: Severity::Info,
                        rule: self.name().to_string(),
                        message: "GO TO statement may not be compliant with modern COBOL standards. Consider refactoring to structured programming".to_string(),
                        location: None,
                    });
                }
                _ => {}
            }
        }

        // Check for ALTER statement (deprecated)
        // This would require checking the AST for ALTER statements
        
        // Check for proper program structure
        if program.identification.node.program_id.is_none() {
            issues.push(LintIssue {
                severity: Severity::Warning,
                rule: self.name().to_string(),
                message: "Program should have a PROGRAM-ID for COBOL 2014 compliance".to_string(),
                location: Some("IDENTIFICATION DIVISION".to_string()),
            });
        }

        issues
    }
}
