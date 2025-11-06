use anyhow::{Context, Result};
use clap::{Arg, Command};
use cobol_ast::{Program, Spanned, Statement};
use cobol_lexer::Format;
use cobol_parser::parse_source;
use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct LintIssue {
    pub severity: Severity,
    pub rule: String,
    pub message: String,
    pub line: usize,
    pub column: usize,
    pub file: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
}

impl LintIssue {
    pub fn error(rule: &str, message: &str, line: usize, column: usize) -> Self {
        Self {
            severity: Severity::Error,
            rule: rule.to_string(),
            message: message.to_string(),
            line,
            column,
            file: None,
        }
    }

    pub fn warning(rule: &str, message: &str, line: usize, column: usize) -> Self {
        Self {
            severity: Severity::Warning,
            rule: rule.to_string(),
            message: message.to_string(),
            line,
            column,
            file: None,
        }
    }

    pub fn info(rule: &str, message: &str, line: usize, column: usize) -> Self {
        Self {
            severity: Severity::Info,
            rule: rule.to_string(),
            message: message.to_string(),
            line,
            column,
            file: None,
        }
    }

    pub fn with_file(mut self, file: String) -> Self {
        self.file = Some(file);
        self
    }
}

pub struct Linter {
    issues: Vec<LintIssue>,
    visited_identifiers: HashSet<String>,
}

impl Linter {
    pub fn new() -> Self {
        Self {
            issues: Vec::new(),
            visited_identifiers: HashSet::new(),
        }
    }

    pub fn lint(&mut self, program: &Spanned<Program>, filename: Option<&str>) -> Vec<LintIssue> {
        self.issues.clear();
        self.visited_identifiers.clear();

        // Check program structure
        self.check_program_structure(program, filename);

        // Check data division
        if let Some(data) = &program.node.data {
            self.check_data_division(&data.node, filename);
        }

        // Check procedure division
        self.check_procedure_division(&program.node.procedure.node, filename);

        self.issues.clone()
    }

    fn check_program_structure(&mut self, program: &Spanned<Program>, filename: Option<&str>) {
        let id = &program.node.identification.node;

        if id.program_id.is_none() {
            self.add_issue(
                LintIssue::warning(
                    "missing-program-id",
                    "Program should have a PROGRAM-ID",
                    program.span.start_line,
                    program.span.start_column,
                )
                .with_optional_file(filename),
            );
        }

        if id.author.is_none() {
            self.add_issue(
                LintIssue::info(
                    "missing-author",
                    "Consider adding AUTHOR information",
                    program.span.start_line,
                    program.span.start_column,
                )
                .with_optional_file(filename),
            );
        }

        if program.node.environment.is_none() {
            self.add_issue(
                LintIssue::warning(
                    "missing-environment-division",
                    "Program should have Environment Division",
                    program.span.start_line,
                    program.span.start_column,
                )
                .with_optional_file(filename),
            );
        }

        if program.node.data.is_none() {
            self.add_issue(
                LintIssue::warning(
                    "missing-data-division",
                    "Program should have Data Division",
                    program.span.start_line,
                    program.span.start_column,
                )
                .with_optional_file(filename),
            );
        }
    }

    fn check_data_division(&mut self, data: &cobol_ast::DataDivision, filename: Option<&str>) {
        if let Some(ws) = &data.working_storage_section {
            for item in &ws.node.data_items {
                // Check for very long names (COBOL convention is typically 30 chars max)
                if item.node.name.node.len() > 30 {
                    self.add_issue(
                        LintIssue::warning(
                            "long-identifier-name",
                            &format!(
                                "Identifier '{}' is very long ({} chars)",
                                item.node.name.node,
                                item.node.name.node.len()
                            ),
                            item.span.start_line,
                            item.span.start_column,
                        )
                        .with_optional_file(filename),
                    );
                }
            }
        }
    }

    fn check_procedure_division(
        &mut self,
        procedure: &cobol_ast::ProcedureDivision,
        filename: Option<&str>,
    ) {
        let mut statement_count = 0;
        let mut nested_depth = 0;
        let mut max_nested_depth = 0;

        for statement in &procedure.statements {
            statement_count += 1;

            match &statement.node {
                Statement::If(if_stmt) => {
                    let if_stmt = &if_stmt.node;
                    let then_statements = &if_stmt.then_statements;
                    let else_statements = &if_stmt.else_statements;
                    nested_depth += 1;
                    max_nested_depth = max_nested_depth.max(nested_depth);

                    if nested_depth > 5 {
                        self.add_issue(
                            LintIssue::warning(
                                "deeply-nested-code",
                                &format!("Deeply nested code (depth: {})", nested_depth),
                                statement.span.start_line,
                                statement.span.start_column,
                            )
                            .with_optional_file(filename),
                        );
                    }

                    // Check nested statements
                    for stmt in then_statements {
                        self.check_statement(stmt, nested_depth, filename);
                    }
                    if let Some(else_stmts) = else_statements {
                        for stmt in else_stmts {
                            self.check_statement(stmt, nested_depth, filename);
                        }
                    }

                    nested_depth -= 1;
                }
                Statement::Perform { .. } => {
                    // PERFORM loops can add complexity
                    nested_depth += 1;
                    max_nested_depth = max_nested_depth.max(nested_depth);
                    nested_depth -= 1;
                }
                Statement::Stop { .. } => {
                    // Check if STOP RUN is at the end
                    if statement_count < procedure.statements.len() {
                        self.add_issue(
                            LintIssue::warning(
                                "stop-run-not-last",
                                "STOP RUN should typically be the last statement",
                                statement.span.start_line,
                                statement.span.start_column,
                            )
                            .with_optional_file(filename),
                        );
                    }
                }
                _ => {
                    self.check_statement(statement, nested_depth, filename);
                }
            }
        }

        // Check for very long procedures
        if statement_count > 100 {
            self.add_issue(
                LintIssue::warning(
                    "large-procedure-division",
                    &format!(
                        "Procedure Division has {} statements (consider splitting)",
                        statement_count
                    ),
                    1, // Default line number
                    1, // Default column number
                )
                .with_optional_file(filename),
            );
        }

        // Check for high cyclomatic complexity
        if max_nested_depth > 3 {
            self.add_issue(
                LintIssue::warning(
                    "high-complexity",
                    &format!("High nesting depth detected (max: {})", max_nested_depth),
                    1, // Default line number
                    1, // Default column number
                )
                .with_optional_file(filename),
            );
        }
    }

    fn check_statement(
        &mut self,
        statement: &Spanned<Statement>,
        depth: usize,
        filename: Option<&str>,
    ) {
        match &statement.node {
            Statement::Display { .. } => {
                // Display statements are generally fine
            }
            Statement::Move { .. } => {
                // Check for potential issues with MOVE
            }
            Statement::Compute { .. } => {
                // Complex computations might need review
            }
            Statement::Stop { .. } => {
                if depth > 0 {
                    self.add_issue(
                        LintIssue::warning(
                            "stop-run-in-nested-context",
                            "STOP RUN inside nested block may cause unexpected behavior",
                            statement.span.start_line,
                            statement.span.start_column,
                        )
                        .with_optional_file(filename),
                    );
                }
            }
            _ => {}
        }
    }

    fn add_issue(&mut self, issue: LintIssue) {
        self.issues.push(issue);
    }
}

impl Default for Linter {
    fn default() -> Self {
        Self::new()
    }
}

trait WithOptionalFile {
    fn with_optional_file(self, filename: Option<&str>) -> Self;
}

impl WithOptionalFile for LintIssue {
    fn with_optional_file(mut self, filename: Option<&str>) -> Self {
        if let Some(f) = filename {
            self.file = Some(f.to_string());
        }
        self
    }
}

fn main() -> Result<()> {
    let matches = Command::new("cobol-linter")
        .about("Static analysis linter for COBOL code")
        .version("0.1.0")
        .arg(
            Arg::new("input")
                .help("Input COBOL file(s)")
                .required(true)
                .num_args(1..),
        )
        .arg(
            Arg::new("format")
                .short('f')
                .long("format")
                .help("Output format (text, json)")
                .default_value("text"),
        )
        .arg(
            Arg::new("severity")
                .short('s')
                .long("severity")
                .help("Minimum severity level (error, warning, info)")
                .default_value("info"),
        )
        .get_matches();

    let files: Vec<String> = matches
        .get_many::<String>("input")
        .unwrap()
        .cloned()
        .collect();
    let format = matches.get_one::<String>("format").unwrap();
    let min_severity = matches.get_one::<String>("severity").unwrap();

    let min_sev = match min_severity.as_str() {
        "error" => Severity::Error,
        "warning" => Severity::Warning,
        "info" => Severity::Info,
        _ => Severity::Info,
    };

    let mut all_issues = Vec::new();

    for file in files {
        let file_ref = &file;
        let path = PathBuf::from(file_ref);
        let contents = fs::read_to_string(&path)
            .with_context(|| format!("Failed to read file: {}", file_ref))?;

        match parse_source(&contents, Format::FreeFormat) {
            Ok(program) => {
                let mut linter = Linter::new();
                let issues = linter.lint(&program, Some(file_ref));
                all_issues.extend(issues);
            }
            Err(e) => {
                eprintln!("Error parsing {}: {}", file, e);
            }
        }
    }

    // Filter by severity
    let filtered_issues: Vec<LintIssue> = all_issues
        .into_iter()
        .filter(|issue| match (min_sev, issue.severity) {
            (Severity::Error, _) => issue.severity == Severity::Error,
            (Severity::Warning, Severity::Error) => true,
            (Severity::Warning, Severity::Warning) => true,
            (Severity::Warning, Severity::Info) => false,
            (Severity::Info, _) => true,
        })
        .collect();

    match format.as_str() {
        "json" => {
            let json = serde_json::to_string_pretty(&filtered_issues)?;
            println!("{}", json);
        }
        _ => {
            print_text_output(&filtered_issues);
        }
    }

    let error_count = filtered_issues
        .iter()
        .filter(|i| i.severity == Severity::Error)
        .count();
    let warning_count = filtered_issues
        .iter()
        .filter(|i| i.severity == Severity::Warning)
        .count();
    let info_count = filtered_issues
        .iter()
        .filter(|i| i.severity == Severity::Info)
        .count();

    if error_count > 0 || warning_count > 0 {
        std::process::exit(1);
    }

    Ok(())
}

fn print_text_output(issues: &[LintIssue]) {
    if issues.is_empty() {
        println!("✓ No linting issues found!");
        return;
    }

    for issue in issues {
        let severity_str = match issue.severity {
            Severity::Error => "ERROR",
            Severity::Warning => "WARNING",
            Severity::Info => "INFO",
        };

        let file_str = issue
            .file
            .as_ref()
            .map(|f| format!("{}:", f))
            .unwrap_or_default();

        if file_str.is_empty() {
            println!(
                "{}:{}:{} [{}] {}",
                issue.line, issue.column, severity_str, issue.rule, issue.message
            );
        } else {
            println!(
                "{}:{}:{}:{} [{}] {}",
                file_str.trim_end_matches(':'),
                issue.line,
                issue.column,
                severity_str,
                issue.rule,
                issue.message
            );
        }
    }

    let error_count = issues
        .iter()
        .filter(|i| i.severity == Severity::Error)
        .count();
    let warning_count = issues
        .iter()
        .filter(|i| i.severity == Severity::Warning)
        .count();
    let info_count = issues
        .iter()
        .filter(|i| i.severity == Severity::Info)
        .count();

    println!(
        "\nSummary: {} errors, {} warnings, {} info",
        error_count, warning_count, info_count
    );
}

// Implement serde for LintIssue
impl serde::Serialize for LintIssue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("LintIssue", 6)?;
        state.serialize_field("severity", &format!("{:?}", self.severity))?;
        state.serialize_field("rule", &self.rule)?;
        state.serialize_field("message", &self.message)?;
        state.serialize_field("line", &self.line)?;
        state.serialize_field("column", &self.column)?;
        state.serialize_field("file", &self.file)?;
        state.end()
    }
}

impl serde::Serialize for Severity {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&format!("{:?}", self))
    }
}
